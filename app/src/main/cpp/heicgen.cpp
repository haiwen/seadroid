#include "android/log.h"
#include "cstdio"
#include "cstring"
#include "jni.h"
#include "memory"
#include "string"
#include "vector"
#include "algorithm"
#include "cctype"
#include <setjmp.h>
#include <type_traits>

// libjpeg for JPEG decoding
extern "C" {
#include "libjpeg/jpeglib.h"
}

// libheif for HEIC encoding
#include "libheif/heif.h"
#include "libheif/heif_brands.h"
#include "libheif/heif_sequences.h"

#define TAG "HeicSeq"
#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG, TAG, __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR, TAG, __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO, TAG, __VA_ARGS__)

#define TAG_MP "MotionPhotoNative"
#define LOGD_MP(...) __android_log_print(ANDROID_LOG_DEBUG, TAG_MP, __VA_ARGS__)
#define LOGI_MP(...) __android_log_print(ANDROID_LOG_INFO, TAG_MP, __VA_ARGS__)
#define LOGE_MP(...) __android_log_print(ANDROID_LOG_ERROR, TAG_MP, __VA_ARGS__)
#define LOGW_MP(...) __android_log_print(ANDROID_LOG_WARN, TAG_MP, __VA_ARGS__)

static bool IsLikelyJpegBytes(const std::vector<uint8_t> &data);
static bool HasJpegSoi(const std::vector<uint8_t> &data);

/**
 * Encode RGBA pixel data into a single HEVC still image (with alpha)
 *
 * Key: This creates a completely independent HEVC intra image that can be decoded independently
 */
static heif_image *CreateHeifImageFromRGBA(const uint8_t *rgba, int width,
                                           int height, int stride) {
    heif_image *image = nullptr;
    heif_error err = heif_image_create(width, height, heif_colorspace_RGB,
                                       heif_chroma_interleaved_RGBA, &image);
    if (err.code != heif_error_Ok || !image) {
        LOGE("Failed to create heif_image: %s", err.message);
        return nullptr;
    }

    err = heif_image_add_plane(image, heif_channel_interleaved, width, height, 8);
    if (err.code != heif_error_Ok) {
        heif_image_release(image);
        LOGE("Failed to add image plane: %s", err.message);
        return nullptr;
    }

    int plane_stride = 0;
    uint8_t *plane =
            heif_image_get_plane(image, heif_channel_interleaved, &plane_stride);

    for (int y = 0; y < height; ++y) {
        const uint8_t *src_row = rgba + y * stride;
        uint8_t *dst_row = plane + y * plane_stride;
        for (int x = 0; x < width; ++x) {
            dst_row[x * 4 + 0] = src_row[x * 4 + 0]; // R
            dst_row[x * 4 + 1] = src_row[x * 4 + 1]; // G
            dst_row[x * 4 + 2] = src_row[x * 4 + 2]; // B
            dst_row[x * 4 + 3] = 255;                // A
        }
    }

    // Set NCLX color profile (BT.709, limited range - consistent with rally_burst)
    heif_color_profile_nclx *nclx = heif_nclx_color_profile_alloc();
    if (nclx) {
        nclx->color_primaries = heif_color_primaries_ITU_R_BT_709_5;
        nclx->transfer_characteristics =
                heif_transfer_characteristic_ITU_R_BT_709_5;
        nclx->matrix_coefficients = heif_matrix_coefficients_ITU_R_BT_709_5;
        nclx->full_range_flag = 0; // limited range (tv), consistent with rally_burst
        heif_image_set_nclx_color_profile(image, nclx);
        heif_nclx_color_profile_free(nclx);
    }

    return image;
}

/**
 * Decode from JPEG data and encode as HEIC Primary Image
 */
static bool EncodePrimaryImageFromJpeg(const std::vector<uint8_t> &jpegBytes,
                                       heif_context *ctx) {
    if (jpegBytes.empty() || !ctx) {
        LOGE("Invalid input for primary image encoding");
        return false;
    }

    jpeg_decompress_struct cinfo;
    jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);

    jpeg_mem_src(&cinfo, const_cast<unsigned char *>(jpegBytes.data()),
                 jpegBytes.size());

    if (jpeg_read_header(&cinfo, TRUE) != JPEG_HEADER_OK) {
        jpeg_destroy_decompress(&cinfo);
        LOGE("Failed to read JPEG header");
        return false;
    }

    cinfo.out_color_space = JCS_RGB;
    if (!jpeg_start_decompress(&cinfo)) {
        jpeg_destroy_decompress(&cinfo);
        LOGE("Failed to start JPEG decompression");
        return false;
    }

    int width = static_cast<int>(cinfo.output_width);
    int height = static_cast<int>(cinfo.output_height);
    int comps = static_cast<int>(cinfo.output_components);

    LOGI("JPEG: %dx%d, components=%d", width, height, comps);

    // Decode to RGBA format
    size_t src_row_stride = static_cast<size_t>(width * comps);
    std::vector<uint8_t> row(src_row_stride);
    size_t rgba_stride = static_cast<size_t>(width) * 4;
    std::vector<uint8_t> rgba(static_cast<size_t>(height) * rgba_stride);

    while (cinfo.output_scanline < cinfo.output_height) {
        JSAMPROW rowptr = row.data();
        jpeg_read_scanlines(&cinfo, &rowptr, 1);
        size_t y = static_cast<size_t>(cinfo.output_scanline - 1);
        uint8_t *dst = rgba.data() + y * rgba_stride;

        if (comps == 3) {
            for (int x = 0; x < width; ++x) {
                dst[4 * x + 0] = row[3 * x + 0]; // R
                dst[4 * x + 1] = row[3 * x + 1]; // G
                dst[4 * x + 2] = row[3 * x + 2]; // B
                dst[4 * x + 3] = 255;            // A
            }
        } else if (comps == 1) {
            for (int x = 0; x < width; ++x) {
                uint8_t g = row[x];
                dst[4 * x + 0] = g;
                dst[4 * x + 1] = g;
                dst[4 * x + 2] = g;
                dst[4 * x + 3] = 255;
            }
        } else {
            jpeg_finish_decompress(&cinfo);
            jpeg_destroy_decompress(&cinfo);
            LOGE("Unsupported JPEG format: %d components", comps);
            return false;
        }
    }

    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);

    heif_image *image = CreateHeifImageFromRGBA(rgba.data(), width, height,
                                                static_cast<int>(rgba_stride));
    if (!image) {
        return false;
    }

    heif_encoder *encoder = nullptr;
    heif_error err =
            heif_context_get_encoder_for_format(ctx, heif_compression_HEVC, &encoder);
    if (err.code != heif_error_Ok || !encoder) {
        heif_image_release(image);
        LOGE("Failed to get HEVC encoder: %s", err.message);
        return false;
    }

    heif_encoder_set_lossy_quality(encoder, 90);

    // Disable alpha channel saving to avoid generating alpha auxiliary track which causes image to display as black
    heif_encoding_options *enc_options = heif_encoding_options_alloc();
    if (enc_options) {
        enc_options->save_alpha_channel = 0;
    }

    heif_image_handle *handle = nullptr;
    err = heif_context_encode_image(ctx, image, encoder, enc_options, &handle);

    if (enc_options) {
        heif_encoding_options_free(enc_options);
    }

    if (err.code == heif_error_Ok && handle) {
        heif_context_set_primary_image(ctx, handle);
    }
    if (handle)
        heif_image_handle_release(handle);
    heif_encoder_release(encoder);
    heif_image_release(image);

    if (err.code != heif_error_Ok) {
        LOGE("Failed to encode primary image: %s", err.message);
        return false;
    }

    LOGI("Primary image encoded successfully (alpha disabled)");
    return true;
}

/**
 * Generate static HEIC
 */
extern "C" JNIEXPORT jboolean

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_GenStillHeicSeq(
        JNIEnv *env, jclass clazz, jbyteArray jpegBytes, jstring outputPath) {
    const char *outPath = env->GetStringUTFChars(outputPath, nullptr);
    if (!outPath) {
        return JNI_FALSE;
    }

    jsize imgLen = env->GetArrayLength(jpegBytes);
    if (imgLen <= 0) {
        env->ReleaseStringUTFChars(outputPath, outPath);
        return JNI_FALSE;
    }

    std::vector<uint8_t> jpegData(imgLen);
    jbyte *imgData = env->GetByteArrayElements(jpegBytes, nullptr);
    if (!imgData) {
        env->ReleaseStringUTFChars(outputPath, outPath);
        return JNI_FALSE;
    }
    memcpy(jpegData.data(), imgData, imgLen);
    env->ReleaseByteArrayElements(jpegBytes, imgData, JNI_ABORT);

    heif_context *ctx = heif_context_alloc();
    if (!ctx) {
        env->ReleaseStringUTFChars(outputPath, outPath);
        return JNI_FALSE;
    }

    bool success = EncodePrimaryImageFromJpeg(jpegData, ctx);
    if (success) {
        heif_error werr = heif_context_write_to_file(ctx, outPath);
        success = (werr.code == heif_error_Ok);
        if (!success) {
            LOGE("Failed to write HEIC: %s", werr.message);
        }
    }

    heif_context_free(ctx);
    env->ReleaseStringUTFChars(outputPath, outPath);

    return success ? JNI_TRUE : JNI_FALSE;
}

/**
 * Get libheif version
 */
extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_GetLibVersion(JNIEnv *env,
                                                        jclass clazz) {
    const char *version = heif_get_version();
    return env->NewStringUTF(version);
}

/**
 * Custom heif_writer for writing to memory buffer
 */
struct MemoryWriter {
    std::vector<uint8_t> data;
};

static heif_error memory_writer_write(heif_context *ctx, const void *data, size_t size, void *userdata) {
    MemoryWriter *writer = static_cast<MemoryWriter *>(userdata);
    const uint8_t *bytes = static_cast<const uint8_t *>(data);
    size_t oldSize = writer->data.size();
    writer->data.insert(writer->data.end(), bytes, bytes + size);
    LOGD_MP("[MemoryWriter] write: %zu bytes (buffer: %zu -> %zu)", size, oldSize,
            writer->data.size());
    heif_error err = {heif_error_Ok, heif_suberror_Unspecified, nullptr};
    return err;
}

static std::string GenerateHeicMotionPhotoXMP(size_t mp4VideoLength, size_t hdrDataSize);

/**
 * Write mpvd box (Motion Photo Video Data)
 *
 * mpvd box structure:
 * - 4 bytes: box size (big-endian)
 * - 4 bytes: box type 'mpvd'
 * - N bytes: MP4 video data
 */
static bool WriteMpvdBox(FILE *file, const std::vector<uint8_t> &mp4Data) {
    LOGD_MP("[WriteMpvdBox] START - mp4Data size=%zu", mp4Data.size());

    // mpvd box: size(4) + type(4) + data(N)
    uint32_t boxSize = static_cast<uint32_t>(8 + mp4Data.size());
    LOGD_MP("[WriteMpvdBox] boxSize=%u (header=8 + data=%zu)", boxSize,
            mp4Data.size());

    // Write box size (big-endian)
    uint8_t sizeBytes[4] = {static_cast<uint8_t>((boxSize >> 24) & 0xFF),
                            static_cast<uint8_t>((boxSize >> 16) & 0xFF),
                            static_cast<uint8_t>((boxSize >> 8) & 0xFF),
                            static_cast<uint8_t>(boxSize & 0xFF)};
    LOGD_MP("[WriteMpvdBox] size bytes (big-endian): [%02X %02X %02X %02X]",
            sizeBytes[0], sizeBytes[1], sizeBytes[2], sizeBytes[3]);

    if (fwrite(sizeBytes, 1, 4, file) != 4) {
        LOGE_MP("[WriteMpvdBox] FAILED to write box size");
        return false;
    }
    LOGD_MP("[WriteMpvdBox] wrote box size (4 bytes)");

    // Write box type 'mpvd'
    const char *boxType = "mpvd";
    if (fwrite(boxType, 1, 4, file) != 4) {
        LOGE_MP("[WriteMpvdBox] FAILED to write box type 'mpvd'");
        return false;
    }
    LOGD_MP("[WriteMpvdBox] wrote box type 'mpvd' (4 bytes)");

    // Print magic bytes at the beginning of MP4 file (ftyp)
    if (mp4Data.size() >= 12) {
        LOGD_MP("[WriteMpvdBox] MP4 header bytes: [%02X %02X %02X %02X] [%c%c%c%c] "
                "[%c%c%c%c]",
                mp4Data[0], mp4Data[1], mp4Data[2], mp4Data[3], mp4Data[4],
                mp4Data[5], mp4Data[6], mp4Data[7], mp4Data[8], mp4Data[9],
                mp4Data[10], mp4Data[11]);
    }

    // Write MP4 video data
    size_t written = fwrite(mp4Data.data(), 1, mp4Data.size(), file);
    if (written != mp4Data.size()) {
        LOGE_MP("[WriteMpvdBox] FAILED to write MP4 data (wrote %zu of %zu)",
                written, mp4Data.size());
        return false;
    }
    LOGD_MP("[WriteMpvdBox] wrote MP4 data (%zu bytes)", mp4Data.size());

    LOGI_MP("[WriteMpvdBox] SUCCESS - total mpvd box = %u bytes", boxSize);
    return true;
}

/**
 * Decode from JPEG data and encode as HEIC Primary Image (用于 Motion Photo)
 * Return image handle for subsequent XMP metadata addition
 */
static heif_image_handle *EncodePrimaryImageForMotionPhoto(
        const std::vector<uint8_t> &jpegBytes, heif_context *ctx) {
    LOGD_MP("[EncodePrimary] START - jpegBytes size=%zu, ctx=%p", jpegBytes.size(), ctx);

    if (jpegBytes.empty() || !ctx) {
        LOGE_MP("[EncodePrimary] Invalid input (empty=%d, ctx=%p)", jpegBytes.empty(), ctx);
        return nullptr;
    }

    // print JPEG head (should be FF D8 FF)
    if (jpegBytes.size() >= 4) {
        LOGD_MP("[EncodePrimary] JPEG header: [%02X %02X %02X %02X]",
                jpegBytes[0], jpegBytes[1], jpegBytes[2], jpegBytes[3]);
    }

    jpeg_decompress_struct cinfo;
    jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);
    LOGD_MP("[EncodePrimary] JPEG decompressor created");

    jpeg_mem_src(&cinfo, const_cast<unsigned char *>(jpegBytes.data()), jpegBytes.size());

    if (jpeg_read_header(&cinfo, TRUE) != JPEG_HEADER_OK) {
        jpeg_destroy_decompress(&cinfo);
        LOGE_MP("[EncodePrimary] Failed to read JPEG header");
        return nullptr;
    }
    LOGD_MP("[EncodePrimary] JPEG header read OK");

    cinfo.out_color_space = JCS_RGB;
    if (!jpeg_start_decompress(&cinfo)) {
        jpeg_destroy_decompress(&cinfo);
        LOGE_MP("[EncodePrimary] Failed to start JPEG decompression");
        return nullptr;
    }

    int width = static_cast<int>(cinfo.output_width);
    int height = static_cast<int>(cinfo.output_height);
    int comps = static_cast<int>(cinfo.output_components);

    LOGI_MP("[EncodePrimary] JPEG decoded: %dx%d, components=%d", width, height,
            comps);

    // Decode to RGBA format
    size_t src_row_stride = static_cast<size_t>(width * comps);
    std::vector<uint8_t> row(src_row_stride);
    size_t rgba_stride = static_cast<size_t>(width) * 4;
    std::vector<uint8_t> rgba(static_cast<size_t>(height) * rgba_stride);
    LOGD_MP("[EncodePrimary] Allocated RGBA buffer: %zu bytes (stride=%zu)",
            rgba.size(), rgba_stride);

    int scanlines = 0;
    while (cinfo.output_scanline < cinfo.output_height) {
        JSAMPROW rowptr = row.data();
        jpeg_read_scanlines(&cinfo, &rowptr, 1);
        size_t y = static_cast<size_t>(cinfo.output_scanline - 1);
        uint8_t *dst = rgba.data() + y * rgba_stride;

        if (comps == 3) {
            for (int x = 0; x < width; ++x) {
                dst[4 * x + 0] = row[3 * x + 0]; // R
                dst[4 * x + 1] = row[3 * x + 1]; // G
                dst[4 * x + 2] = row[3 * x + 2]; // B
                dst[4 * x + 3] = 255;            // A
            }
        } else if (comps == 1) {
            for (int x = 0; x < width; ++x) {
                uint8_t g = row[x];
                dst[4 * x + 0] = g;
                dst[4 * x + 1] = g;
                dst[4 * x + 2] = g;
                dst[4 * x + 3] = 255;
            }
        } else {
            jpeg_finish_decompress(&cinfo);
            jpeg_destroy_decompress(&cinfo);
            LOGE_MP("[EncodePrimary] Unsupported JPEG format: %d components", comps);
            return nullptr;
        }
        scanlines++;
    }
    LOGD_MP("[EncodePrimary] Decoded %d scanlines to RGBA", scanlines);

    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
    LOGD_MP("[EncodePrimary] JPEG decompression finished");

    LOGD_MP("[EncodePrimary] Creating heif_image from RGBA...");
    heif_image *image = CreateHeifImageFromRGBA(rgba.data(), width, height,
                                                static_cast<int>(rgba_stride));
    if (!image) {
        LOGE_MP("[EncodePrimary] Failed to create heif_image");
        return nullptr;
    }
    LOGD_MP("[EncodePrimary] heif_image created: %p", image);

    // Get HEVC encoder
    heif_encoder *encoder = nullptr;
    heif_error err = heif_context_get_encoder_for_format(ctx, heif_compression_HEVC, &encoder);
    if (err.code != heif_error_Ok || !encoder) {
        heif_image_release(image);
        LOGE_MP("[EncodePrimary] Failed to get HEVC encoder: %s (code=%d)", err.message, err.code);
        return nullptr;
    }

    LOGD_MP("[EncodePrimary] HEVC encoder obtained: %p", encoder);

    heif_encoder_set_lossy_quality(encoder, 90);
    LOGD_MP("[EncodePrimary] Encoder quality set to 90");

    // Disable alpha channel saving
    heif_encoding_options *enc_options = heif_encoding_options_alloc();
    if (enc_options) {
        enc_options->save_alpha_channel = 0;
        LOGD_MP("[EncodePrimary] Alpha channel disabled");
    }

    heif_image_handle *handle = nullptr;
    LOGD_MP("[EncodePrimary] Starting HEVC encoding...");
    err = heif_context_encode_image(ctx, image, encoder, enc_options, &handle);

    if (enc_options) {
        heif_encoding_options_free(enc_options);
    }

    if (err.code == heif_error_Ok && handle) {
        heif_context_set_primary_image(ctx, handle);
        LOGD_MP("[EncodePrimary] Primary image set in context");
    }

    heif_encoder_release(encoder);
    heif_image_release(image);
    LOGD_MP("[EncodePrimary] Encoder and image released");

    if (err.code != heif_error_Ok) {
        LOGE_MP("[EncodePrimary] HEVC encoding failed: %s (code=%d, subcode=%d)",
                err.message, err.code, err.subcode);
        if (handle)
            heif_image_handle_release(handle);
        return nullptr;
    }

    LOGI_MP("[EncodePrimary] SUCCESS - handle=%p, image=%dx%d", handle, width,
            height);
    return handle;
}

// JPEG error handler to avoid exit
struct my_error_mgr {
    struct jpeg_error_mgr pub;
    jmp_buf setjmp_buffer;
};

static void my_error_exit(j_common_ptr cinfo) {
    my_error_mgr *myerr = (my_error_mgr *) cinfo->err;
    (*cinfo->err->output_message)(cinfo);
    longjmp(myerr->setjmp_buffer, 1);
}

static heif_image *DecodeJpegToHeifImage(const std::vector<uint8_t> &data) {
    if (data.empty()) return nullptr;

    struct jpeg_decompress_struct cinfo;
    struct my_error_mgr jerr;

    cinfo.err = jpeg_std_error(&jerr.pub);
    jerr.pub.error_exit = my_error_exit;

    if (setjmp(jerr.setjmp_buffer)) {
        jpeg_destroy_decompress(&cinfo);
        return nullptr;
    }

    jpeg_create_decompress(&cinfo);
    jpeg_mem_src(&cinfo, data.data(), static_cast<unsigned long>(data.size()));

    if (jpeg_read_header(&cinfo, TRUE) != JPEG_HEADER_OK) {
        jpeg_destroy_decompress(&cinfo);
        return nullptr;
    }

    cinfo.out_color_space = JCS_RGB;
    jpeg_start_decompress(&cinfo);

    int width = cinfo.output_width;
    int height = cinfo.output_height;
    int channels = cinfo.output_components;

    if (channels != 3) {
        LOGE_MP("DecodeJpegToHeifImage: Only RGB JPEG is supported (components=%d)", channels);
        jpeg_finish_decompress(&cinfo);
        jpeg_destroy_decompress(&cinfo);
        return nullptr;
    }

    heif_image *image = nullptr;
    heif_error err = heif_image_create(width, height, heif_colorspace_RGB, heif_chroma_interleaved_RGB, &image);
    if (err.code != heif_error_Ok) {
        LOGE_MP("DecodeJpegToHeifImage: Failed to create heif_image");
        jpeg_finish_decompress(&cinfo);
        jpeg_destroy_decompress(&cinfo);
        return nullptr;
    }

    err = heif_image_add_plane(image, heif_channel_interleaved, width, height, 8);
    if (err.code != heif_error_Ok) {
        LOGE_MP("DecodeJpegToHeifImage: Failed to add plane");
        heif_image_release(image);
        jpeg_finish_decompress(&cinfo);
        jpeg_destroy_decompress(&cinfo);
        return nullptr;
    }

    int stride;
    uint8_t *p = heif_image_get_plane(image, heif_channel_interleaved, &stride);

    while (cinfo.output_scanline < cinfo.output_height) {
        uint8_t *row_pointer = p + cinfo.output_scanline * stride;
        jpeg_read_scanlines(&cinfo, &row_pointer, 1);
    }

    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
    return image;
}

/**
 * Generate Google HEIC motion photo
 *
 * Requirements:
 * Generate HEIC motion photo using Google Motion Photo format
 *
 * Reference:
 * https://developer.android.com/media/platform/motion-photo-format?hl=zh-cn#isobmff-image-specific-behavior
 *
 * Structure:
 * - HEIC motion photo file (container)
 *   - ftyp box
 *   - meta box
 *      - XMP must contain Google Motion Photo
 * Identifier fields: e.g.(G)Camera:MotionPhoto、(G)Camera:MotionPhotoVersion、(G)Camera:MotionPhotoPresentationTimestampUs
 *      - XMP Item must have Mime, Semantic, Length, Padding
 *      - ISOBMFF image XMP must also define Primary media item's Padding property value as 8.
 *   - mdat box: image contents
 *   - mpvd box: (MotionPhotoVideoData)
 *      - Store mp4 video raw byte stream.
 *      - Location: the "mpvd" box must come after all the HEIC image file's boxes.
 *
 *  Technical requirements:
 *  1、libheif provides XMP_metadata operation APIs, you can use them
 *  2、primaryImageBytes, mp4VideoBytes are raw byte array data
 *  3、Prefer using libheif's built-in APIs.
 *  4、HDR data
 *   - If HDR is available, use metadata item to implement GainMap binding. libheif has already implemented this capability.
 *  5、EXIF data
 *   - If EXIF data is available, you can use heif_context_add_exif_metadata interface to bind it.
 */
extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_GenHeicMotionPhoto(
        JNIEnv *env, jclass clazz, jbyteArray primaryImageBytes,
        jbyteArray hdrBytes,
        jbyteArray exifBytes,
        jbyteArray mp4VideoBytes, jstring outputPath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeGenHeicMotionPhoto: START");
    LOGI_MP("libheif version: %s", heif_get_version());
    LOGI_MP("============================================================");

    const char *outPath = env->GetStringUTFChars(outputPath, nullptr);
    if (!outPath) {
        LOGE_MP("Failed to get output path from JNI");
        return env->NewStringUTF("error: failed to get output path");
    }
    LOGD_MP("[JNI] Output path: %s", outPath);

    // Extract JPEG data
    std::vector<uint8_t> jpegData;
    if (primaryImageBytes) {
        jsize len = env->GetArrayLength(primaryImageBytes);
        LOGD_MP("[JNI] primaryImageBytes: array length = %d", len);
        if (len > 0) {
            jpegData.resize(len);
            jbyte *p = env->GetByteArrayElements(primaryImageBytes, nullptr);
            if (p) {
                memcpy(jpegData.data(), p, len);
                env->ReleaseByteArrayElements(primaryImageBytes, p, JNI_ABORT);
                LOGD_MP("[JNI] primaryImageBytes: copied %d bytes to vector", len);
            } else {
                LOGW_MP("[JNI] primaryImageBytes: GetByteArrayElements returned NULL");
            }
        }
    } else {
        LOGD_MP("[JNI] primaryImageBytes: NULL");
    }
    if (jpegData.empty()) {
        LOGE_MP("Primary image data is empty!");
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: primary image data is empty");
    }

    // HDR data
    std::vector<uint8_t> hdrData;
    if (hdrBytes) {
        jsize len = env->GetArrayLength(hdrBytes);
        LOGD_MP("[JNI] hdrBytes: array length = %d", len);
        if (len > 0) {
            hdrData.resize(len);
            jbyte *p = env->GetByteArrayElements(hdrBytes, nullptr);
            if (p) {
                hdrData.resize(len);
                memcpy(hdrData.data(), p, len);
                env->ReleaseByteArrayElements(hdrBytes, p, JNI_ABORT);
                LOGD_MP("[JNI] hdrBytes: copied %d bytes to vector", len);
            } else {
                LOGW_MP("[JNI] hdrBytes: GetByteArrayElements returned NULL");
            }
        }
    } else {
        LOGD_MP("[JNI] hdrBytes: NULL");
    }

    // Copy EXIF data
    std::vector<uint8_t> exifData;
    if (exifBytes) {
        jsize len = env->GetArrayLength(exifBytes);
        LOGD_MP("[JNI] exifBytes: array length = %d", len);
        if (len > 0) {
            exifData.resize(len);
            jbyte *p = env->GetByteArrayElements(exifBytes, nullptr);
            if (p) {
                memcpy(exifData.data(), p, len);
                env->ReleaseByteArrayElements(exifBytes, p, JNI_ABORT);
                LOGD_MP("[JNI] exifBytes: copied %d bytes to vector", len);
            } else {
                LOGW_MP("[JNI] exifBytes: GetByteArrayElements returned NULL");
            }
        }
    } else {
        LOGD_MP("[JNI] exifBytes: NULL");
    }

    // Copy MP4 video data
    std::vector<uint8_t> mp4Data;
    if (mp4VideoBytes) {
        jsize len = env->GetArrayLength(mp4VideoBytes);
        LOGD_MP("[JNI] mp4VideoBytes: array length = %d", len);
        if (len > 0) {
            mp4Data.resize(len);
            jbyte *p = env->GetByteArrayElements(mp4VideoBytes, nullptr);
            if (p) {
                memcpy(mp4Data.data(), p, len);
                env->ReleaseByteArrayElements(mp4VideoBytes, p, JNI_ABORT);
                LOGD_MP("[JNI] mp4VideoBytes: copied %d bytes to vector", len);
            } else {
                LOGW_MP("[JNI] mp4VideoBytes: GetByteArrayElements returned NULL");
            }
        }
    } else {
        LOGD_MP("[JNI] mp4VideoBytes: NULL");
    }

    if (mp4Data.empty()) {
        LOGE_MP("MP4 video data is empty!");
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: MP4 video data is empty");
    }

    LOGI_MP("------------------------------------------------------------");
    LOGI_MP("Input Summary:");
    LOGI_MP("  JPEG: %zu bytes (%.2f KB)", jpegData.size(), jpegData.size() / 1024.0);
    LOGI_MP("  HDR:  %zu bytes (%.2f KB)", hdrData.size(), hdrData.size() / 1024.0);
    LOGI_MP("  MP4:  %zu bytes (%.2f KB)", mp4Data.size(), mp4Data.size() / 1024.0);
    LOGI_MP("------------------------------------------------------------");

    // Create HEIF context
    LOGD_MP("[Step 1/6] Creating HEIF context...");
    heif_context *ctx = heif_context_alloc();
    if (!ctx) {
        LOGE_MP("[Step 1/6] FAILED to allocate HEIF context");
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to allocate HEIF context");
    }
    LOGD_MP("[Step 1/6] HEIF context created: %p", ctx);

    // Encode Primary Image and get handle
    // Note: Do not set brands before encoding, otherwise encoder acquisition will fail
    LOGI_MP("[Step 2/6] Encoding primary image...");
    heif_image_handle *primaryHandle = EncodePrimaryImageForMotionPhoto(jpegData, ctx);
    if (!primaryHandle) {
        LOGE_MP("[Step 2/6] FAILED to encode primary image");
        heif_context_free(ctx);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to encode primary image");
    }
    LOGI_MP("[Step 2/6] Primary image encoded successfully, handle=%p", primaryHandle);

    if (!hdrData.empty()) {
        if (!HasJpegSoi(hdrData)) {
            LOGW_MP("[Step 3/6] HDR GainMap data missing JPEG SOI, skipping");
        } else {
            heif_error hdrMetaErr = heif_context_add_generic_metadata(
                    ctx,
                    primaryHandle,
                    hdrData.data(),
                    static_cast<int>(hdrData.size()),
                    "mime",
                    "image/jpeg+gainmap"
            );
            if (hdrMetaErr.code != heif_error_Ok) {
                LOGW_MP("[Step 3/6] 添加 HDR GainMap 元数据失败: %s", hdrMetaErr.message);
            } else {
                LOGI_MP("[Step 3/6] HDR GainMap 元数据已写入 (%zu bytes)", hdrData.size());
            }
        }
    }

    // gen xml
    std::string xmpData = GenerateHeicMotionPhotoXMP(mp4Data.size(), hdrData.size());
    heif_context_add_XMP_metadata(ctx, primaryHandle, xmpData.data(), static_cast<int>(xmpData.size()));

    //
    if (!exifData.empty()) {
        heif_context_add_exif_metadata(ctx, primaryHandle, exifData.data(), static_cast<int>(exifData.size()));
    }

    heif_image_handle_release(primaryHandle);

    MemoryWriter memWriter2;
    heif_writer writer2;
    writer2.writer_api_version = 1;
    writer2.write = memory_writer_write;
    heif_error writeErr2 = heif_context_write(ctx, &writer2, &memWriter2);
    heif_context_free(ctx);
    if (writeErr2.code != heif_error_Ok) {
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to write HEIC to memory");
    }

    FILE *outFile = fopen(outPath, "wb");
    if (!outFile) {
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to open output file");
    }

    size_t heicWritten = fwrite(memWriter2.data.data(), 1, memWriter2.data.size(), outFile);
    if (heicWritten != memWriter2.data.size()) {
        fclose(outFile);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to write HEIC data to file");
    }

    if (!WriteMpvdBox(outFile, mp4Data)) {
        fclose(outFile);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to write mpvd box");
    }

    fclose(outFile);
    size_t totalSize = memWriter2.data.size() + 8 + mp4Data.size();
    env->ReleaseStringUTFChars(outputPath, outPath);
    char result[512];
    snprintf(result, sizeof(result),
             "success: HEIC=%zu bytes, MP4=%zu bytes, total=%zu bytes (%.2f MB)",
             memWriter2.data.size(), mp4Data.size(), totalSize,
             totalSize / (1024.0 * 1024.0));
    return env->NewStringUTF(result);
}

/**
 * Motion Photo XMP metadata parsing result
 */
struct MotionPhotoXmpInfo {
    bool isMotionPhoto = false; // Whether it is a Motion Photo
    int version = 0;            // MotionPhotoVersion
    long videoLength = 0;       // Video data length (Item:Length for MotionPhoto)
    long videoPadding = 0;      // Padding bytes before video (Item:Padding)
    long presentationTimestampUs = -1; // Position in video of the primary image frame
    long primaryPadding = 0;    // Primary image padding bytes
    long gainMapLength = 0;     // GainMap length (Item:Length)
    long gainMapPadding = 0;    // GainMap padding (Item:Padding)
    std::string videoMime;      // Video MIME type
    std::string xmpContent;     // Raw XMP content (for debugging)
    long videoOffset = -1;      // Video data offset (GCamera:MicroVideoOffset)
    std::string gainMapMime;    // GainMap MIME type
    struct Item {
        std::string semantic;
        std::string mime;
        long length = 0;
        long padding = 0;
    };
    std::vector<Item> items;    // Container:Directory items in order
};

static long SafeStol(const std::string &str, long defaultValue = 0) {
    try {
        return std::stol(str);
    } catch (...) {
        return defaultValue;
    }
}

static int SafeStoi(const std::string &str, int defaultValue = 0) {
    try {
        return std::stoi(str);
    } catch (...) {
        return defaultValue;
    }
}

/**
 * Extract the value of a specified tag from a string
 * Supports two formats:
 * 1. Attribute format: GCamera:MotionPhoto="1"
 * 2. Tag format: <Item:Length>12345</Item:Length>
 */
static std::string ExtractXmpValue(const std::string &xmp, const std::string &tagName) {
    // Try attribute format: tagName="value"
    std::string attrPattern = tagName + "=\"";
    size_t pos = xmp.find(attrPattern);
    if (pos != std::string::npos) {
        pos += attrPattern.length();
        size_t endPos = xmp.find('"', pos);
        if (endPos != std::string::npos) {
            return xmp.substr(pos, endPos - pos);
        }
    }

    // Try tag format: <tagName>value</tagName>
    std::string startTag = "<" + tagName + ">";
    std::string endTag = "</" + tagName + ">";
    pos = xmp.find(startTag);
    if (pos != std::string::npos) {
        pos += startTag.length();
        size_t endPos = xmp.find(endTag, pos);
        if (endPos != std::string::npos) {
            return xmp.substr(pos, endPos - pos);
        }
    }

    return "";
}

static std::vector<MotionPhotoXmpInfo::Item> ParseContainerItems(const std::string &xmpContent) {
    std::vector<MotionPhotoXmpInfo::Item> items;
    size_t pos = 0;
    while (true) {
        pos = xmpContent.find("<Container:Item", pos);
        if (pos == std::string::npos) break;
        size_t end = xmpContent.find("/>", pos);
        if (end == std::string::npos) {
            end = xmpContent.find(">", pos);
            if (end == std::string::npos) break;
        }
        std::string tag = xmpContent.substr(pos, end - pos);
        auto getAttr = [&](const std::string &s, const std::string &key) -> std::string {
            std::string pattern = key + "=\"";
            size_t kpos = s.find(pattern);
            if (kpos == std::string::npos) return "";
            size_t vstart = kpos + pattern.size();
            size_t vend = s.find("\"", vstart);
            if (vend == std::string::npos) return "";
            return s.substr(vstart, vend - vstart);
        };
        MotionPhotoXmpInfo::Item item;
        item.semantic = getAttr(tag, "Item:Semantic");
        item.mime = getAttr(tag, "Item:Mime");
        std::string lengthStr = getAttr(tag, "Item:Length");
        if (!lengthStr.empty()) {
            item.length = SafeStol(lengthStr);
        }
        std::string paddingStr = getAttr(tag, "Item:Padding");
        if (!paddingStr.empty()) {
            item.padding = SafeStol(paddingStr);
        }
        items.push_back(item);
        pos = end + 2;
    }
    return items;
}

extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoXMP(JNIEnv *env, jclass clazz, jstring inputFilePath);


static std::string ParseHeicMotionPhotoXmpWithLibheif2(const char *filePath);

extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoXMP(JNIEnv *env, jclass clazz, jstring inputFilePath) {
    if (inputFilePath == nullptr) {
        return nullptr;
    }
    const char *path = env->GetStringUTFChars(inputFilePath, nullptr);
    if (path == nullptr) {
        return nullptr;
    }

    std::string xmp = ParseHeicMotionPhotoXmpWithLibheif2(path);

    env->ReleaseStringUTFChars(inputFilePath, path);

    return env->NewStringUTF(xmp.c_str());
}

static MotionPhotoXmpInfo ParseMotionPhotoXmpContent(const std::string &xmpContent) {
    MotionPhotoXmpInfo info;
    info.xmpContent = xmpContent;
    info.items = ParseContainerItems(info.xmpContent);

    // 1. Parse GCamera:MotionPhoto (v2)
    std::string motionPhoto = ExtractXmpValue(info.xmpContent, "GCamera:MotionPhoto");
    if (motionPhoto == "1") {
        info.isMotionPhoto = true;
        info.version = 2;
        LOGD_MP("[ParseXMP] GCamera:MotionPhoto = 1 (confirmed Motion Photo)");
    }

    // Parse GCamera:MotionPhotoPresentationTimestampUs
    std::string tsStr = ExtractXmpValue(info.xmpContent, "GCamera:MotionPhotoPresentationTimestampUs");
    if (!tsStr.empty()) {
        info.presentationTimestampUs = SafeStol(tsStr);
        LOGD_MP("[ParseXMP] GCamera:MotionPhotoPresentationTimestampUs = %ld", info.presentationTimestampUs);
    }

    // Parse Container items (Primary/GainMap/MotionPhoto)
    for (const auto &item : info.items) {
        if (item.semantic == "MotionPhoto") {
            if (item.length > 0) {
                info.videoLength = item.length;
                LOGD_MP("[ParseXMP] MotionPhoto Item:Length = %ld", info.videoLength);
            }
            if (item.padding > 0) {
                info.videoPadding = item.padding;
                LOGD_MP("[ParseXMP] MotionPhoto Item:Padding = %ld", info.videoPadding);
            }
            if (!item.mime.empty()) {
                info.videoMime = item.mime;
                LOGD_MP("[ParseXMP] MotionPhoto Item:Mime = %s", info.videoMime.c_str());
            }
        } else if (item.semantic == "GainMap") {
            if (item.length > 0) {
                info.gainMapLength = item.length;
                LOGD_MP("[ParseXMP] GainMap Item:Length = %ld", info.gainMapLength);
            }
            if (item.padding > 0) {
                info.gainMapPadding = item.padding;
                LOGD_MP("[ParseXMP] GainMap Item:Padding = %ld", info.gainMapPadding);
            }
            if (!item.mime.empty()) {
                info.gainMapMime = item.mime;
                LOGD_MP("[ParseXMP] GainMap Item:Mime = %s", info.gainMapMime.c_str());
            }
        } else if (item.semantic == "Primary") {
            if (item.padding > 0) {
                info.primaryPadding = item.padding;
                LOGD_MP("[ParseXMP] Primary Item:Padding = %ld", info.primaryPadding);
            }
        }
    }
    if (info.videoLength == 0) {
        std::string vendorLen = ExtractXmpValue(info.xmpContent, "OpCamera:VideoLength");
        if (!vendorLen.empty()) {
            info.videoLength = SafeStol(vendorLen);
            LOGD_MP("[ParseXMP] Vendor VideoLength = %ld", info.videoLength);
        }
    }

    // 2. Check v1 version (MicroVideo)
    std::string microVideo = ExtractXmpValue(info.xmpContent, "GCamera:MicroVideo");
    if (microVideo == "1" && !info.isMotionPhoto) {
        info.isMotionPhoto = true;
        info.version = 1;
        LOGD_MP("[ParseXMP] GCamera:MicroVideo = 1 (v1 Motion Photo)");

        std::string tsStr = ExtractXmpValue(info.xmpContent, "GCamera:MicroVideoPresentationTimestampUs");
        if (!tsStr.empty()) {
            info.presentationTimestampUs = SafeStol(tsStr);
            LOGD_MP("[ParseXMP] GCamera:MicroVideoPresentationTimestampUs = %ld", info.presentationTimestampUs);
        }

        std::string videoOffsetStr = ExtractXmpValue(info.xmpContent, "GCamera:MicroVideoOffset");

        if (!videoOffsetStr.empty()) {
            info.videoLength = SafeStol(videoOffsetStr);
            LOGD_MP("[ParseXMP] GCamera:MicroVideoOffset = %ld", info.videoLength);
        }
        return info;
    }

    return info;
}


/**
 * Use libheif to read XMP metadata from HEIC file and parse Motion Photo information

  * Main namespaces:
 * GCAMERA = "http://ns.google.com/photos/1.0/camera/"
 * CONTAINER_NS = "http://ns.google.com/photos/1.0/container/"
 * RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 * ITEM_NS = "http://ns.google.com/photos/1.0/container/item/"
 *
 * Google XMP key fields:
 * v1 version:
 * - GCamera:MicroVideo="1"
 * - GCamera:MicroVideoOffset="1"
 * - GCamera:MicroVideoVersion="1"
 * - GCamera:MicroVideoPresentationTimestampUs="1"
 *
 * v2 version:
 * - GCamera:MotionPhoto="1" - Identifier for Motion Photo
 * - GCamera:MotionPhotoVersion="1" - Version number
 * - GCamera:MotionPhotoPresentationTimestampUs="1"
 * - Item:Semantic="MotionPhoto" 的 Item:Length - Video data length
 * - Item:Padding - Possibly existing padding bytes
 *
 * Reference:
 * https://developer.android.com/media/platform/motion-photo-format
 * @param filePath HEIC file path
 * @return Motion Photo XMP information
 */
static MotionPhotoXmpInfo ParseHeicMotionPhotoXmpWithLibheif(const char *filePath) {
    LOGD_MP("[ParseHeicXMP] Parsing XMP from HEIC using libheif: %s", filePath);
    // Parse XMP content
    std::string xmpXml = ParseHeicMotionPhotoXmpWithLibheif2(filePath);
    MotionPhotoXmpInfo info = ParseMotionPhotoXmpContent(xmpXml);
    if (!info.isMotionPhoto) {
        LOGD_MP("[ParseHeicXMP] No Motion Photo XMP metadata found");
    }

    return info;
}


static std::string ParseHeicMotionPhotoXmpWithLibheif2(const char *filePath) {
    LOGD_MP("[ParseHeicXMP] Parsing XMP from HEIC using libheif: %s", filePath);

    // Create HEIF context and read file
    heif_context *ctx = heif_context_alloc();
    if (!ctx) {
        LOGE_MP("[ParseHeicXMP] Failed to allocate HEIF context");
        return "";
    }

    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
    if (err.code != heif_error_Ok) {
        LOGE_MP("[ParseHeicXMP] Failed to read HEIC file: %s", err.message);
        heif_context_free(ctx);
        return "";
    }

    // Get primary image handle
    heif_image_handle *handle = nullptr;
    err = heif_context_get_primary_image_handle(ctx, &handle);
    if (err.code != heif_error_Ok || !handle) {
        LOGE_MP("[ParseHeicXMP] Failed to get primary image handle: %s", err.message);
        heif_context_free(ctx);
        return "";
    }

    // Get all metadata blocks
    int numMetadata = heif_image_handle_get_number_of_metadata_blocks(handle, nullptr);
    LOGD_MP("[ParseHeicXMP] Found %d metadata blocks", numMetadata);

    std::string xmpXml;
    std::string motionPhotoXmp;

    if (numMetadata > 0) {
        std::vector<heif_item_id> metadataIds(numMetadata);
        heif_image_handle_get_list_of_metadata_block_IDs(
                handle, nullptr, metadataIds.data(), numMetadata);

        for (int i = 0; i < numMetadata; i++) {
            heif_item_id id = metadataIds[i];
            const char *type = heif_image_handle_get_metadata_type(handle, id);
            const char *contentType = heif_image_handle_get_metadata_content_type(handle, id);

            LOGD_MP("[ParseHeicXMP] Metadata[%d]: id=%u, type='%s', contentType='%s'",
                    i, id, type ? type : "(null)",
                    contentType ? contentType : "(null)");

            // XMP 的 content_type 是 "application/rdf+xml"
            bool isXmp = (contentType && strcmp(contentType, "application/rdf+xml") == 0) ||
                         (type && strcmp(type, "mime") == 0 && contentType &&
                          strstr(contentType, "xmp") != nullptr);

            if (isXmp) {
                size_t metadataSize = heif_image_handle_get_metadata_size(handle, id);
                LOGD_MP("[ParseHeicXMP] Found XMP metadata, size=%zu bytes", metadataSize);

                if (metadataSize > 0) {
                    std::vector<uint8_t> xmpData(metadataSize);
                    err = heif_image_handle_get_metadata(handle, id, xmpData.data());
                    if (err.code == heif_error_Ok) {
                        std::string currentXmp = std::string(reinterpret_cast<const char *>(xmpData.data()), metadataSize);
                        LOGD_MP("[ParseHeicXMP] XMP content loaded, size=%zu", currentXmp.size());

                        // Check if this XMP contains Motion Photo fields
                        // Check for v2: GCamera:MotionPhoto="1"
                        bool hasMotionPhoto = currentXmp.find("GCamera:MotionPhoto=\"1\"") != std::string::npos;
                        // Check for v1: GCamera:MicroVideo="1"
                        bool hasMicroVideo = currentXmp.find("GCamera:MicroVideo=\"1\"") != std::string::npos;

                        if (hasMotionPhoto || hasMicroVideo) {
                            LOGI_MP("[ParseHeicXMP] Found XMP with Motion Photo fields (hasMotionPhoto=%d, hasMicroVideo=%d)",
                                    hasMotionPhoto, hasMicroVideo);
                            motionPhotoXmp = currentXmp;
                            break; // Found the correct XMP, stop searching
                        } else {
                            LOGD_MP("[ParseHeicXMP] XMP does not contain Motion Photo fields, continuing search...");
                            // Keep the first XMP as fallback in case no Motion Photo XMP is found
                            if (xmpXml.empty()) {
                                xmpXml = currentXmp;
                            }
                        }
                    } else {
                        LOGE_MP("[ParseHeicXMP] Failed to read XMP data: %s", err.message);
                    }
                }
            }
        }
    }

    // Return Motion Photo XMP if found, otherwise return the first XMP (fallback), or empty string
    if (!motionPhotoXmp.empty()) {
        LOGI_MP("[ParseHeicXMP] Using Motion Photo XMP (size=%zu)", motionPhotoXmp.size());
        return motionPhotoXmp;
    } else if (!xmpXml.empty()) {
        LOGW_MP("[ParseHeicXMP] No Motion Photo XMP found, using first XMP as fallback (size=%zu)", xmpXml.size());
        return xmpXml;
    } else {
        LOGW_MP("[ParseHeicXMP] No XMP metadata found");
        return "";
    }

    heif_image_handle_release(handle);
    heif_context_free(ctx);

    return xmpXml;
}

/**
 * Requirements:
 * Parse XMP metadata from JPEG file data and extract Motion Photo information
 *
 * XMP 在 JPEG 中的Location: 
 * - Stored in APP1 segment (0xFF 0xE1)
 *
 * Main namespaces:
 * GCAMERA = "http://ns.google.com/photos/1.0/camera/"
 * CONTAINER_NS = "http://ns.google.com/photos/1.0/container/"
 * RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 * ITEM_NS = "http://ns.google.com/photos/1.0/container/item/"
 *
 * Google XMP key fields:
 * v1 version:
 * - GCamera:MicroVideo="1"
 * - GCamera:MicroVideoOffset="1"
 * - GCamera:MicroVideoVersion="1"
 * - GCamera:MicroVideoPresentationTimestampUs="1"
 *
 * v2 version:
 * - GCamera:MotionPhoto="1" - Identifier for Motion Photo
 * - GCamera:MotionPhotoVersion="1" - Version number
 * - GCamera:MotionPhotoPresentationTimestampUs="1"
 * - Item:Semantic="MotionPhoto" 的 Item:Length - Video data length
 * - Item:Padding - Possibly existing padding bytes
 *
 * Reference:
 * https://developer.android.com/media/platform/motion-photo-format
 */
static MotionPhotoXmpInfo ParseJpegMotionPhotoXmp(const std::vector<uint8_t> &fileData) {
    MotionPhotoXmpInfo info;

    // XMP identifier
    const char *XMP_MARKER = "http://ns.adobe.com/xap/1.0/";
    const size_t XMP_MARKER_LEN = strlen(XMP_MARKER);

    // Search for APP1 segment (0xFF 0xE1)
    size_t pos = 2; // Skip JPEG SOI (FF D8)
    while (pos < fileData.size() - 4) {
        if (fileData[pos] != 0xFF) {
            pos++;
            continue;
        }

        uint8_t marker = fileData[pos + 1];

        // Check if it is APP1 segment (0xE1)
        if (marker == 0xE1) {
            // Read segment length (big-endian, including the 2 bytes of the length field itself)
            uint16_t segmentLen = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
                                  static_cast<uint16_t>(fileData[pos + 3]);

            size_t segmentDataStart = pos + 4;
            size_t segmentDataLen = segmentLen - 2;

            // Check if it is XMP segment
            if (segmentDataLen > XMP_MARKER_LEN + 1 &&
                memcmp(fileData.data() + segmentDataStart, XMP_MARKER,
                       XMP_MARKER_LEN) == 0) {

                // XMP content is after the identifier (skip the trailing \0)
                size_t xmpStart = segmentDataStart + XMP_MARKER_LEN + 1;
                size_t xmpLen = segmentDataLen - XMP_MARKER_LEN - 1;

                if (xmpStart + xmpLen <= fileData.size()) {
                    info.xmpContent = std::string(
                            reinterpret_cast<const char *>(fileData.data() + xmpStart),
                            xmpLen);

                    LOGD_MP("[ParseXMP] Found XMP segment at offset %zu, length %zu", pos,
                            xmpLen);

                    // Parse XMP content
                    info = ParseMotionPhotoXmpContent(info.xmpContent);
                    return info;
                }
            }
        }

        // If it is SOS (Start of Scan) 0xDA, image data follows, stop searching
        if (marker == 0xDA) {
            LOGD_MP("[ParseXMP] Reached SOS marker, stopping APP segment search");
            break;
        }

        // Skip current segment
        if (marker >= 0xE0 && marker <= 0xEF) { // APPn segment
            if (pos + 3 < fileData.size()) {
                uint16_t len = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
                               static_cast<uint16_t>(fileData[pos + 3]);
                pos += 2 + len;
            } else {
                break;
            }
        } else if (marker == 0xDB || marker == 0xC0 || marker == 0xC2 ||
                   marker == 0xC4 || marker == 0xDD || marker == 0xFE) {
            // Other segments with length: DQT, SOF0, SOF2, DHT, DRI, COM
            if (pos + 3 < fileData.size()) {
                uint16_t len = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
                               static_cast<uint16_t>(fileData[pos + 3]);
                pos += 2 + len;
            } else {
                break;
            }
        } else {
            pos++;
        }
    }

    LOGD_MP("[ParseXMP] No XMP Motion Photo metadata found");
    return info;
}

/**
 * Extract EXIF data from JPEG image.
 * */
static std::vector<uint8_t> ExtractJpegExif(const char *path) {
    std::vector<uint8_t> exifData;
    FILE *file = fopen(path, "rb");
    if (!file) {
        LOGE_MP("[ExtractExif] Failed to open file: %s", path);
        return exifData;
    }

    uint8_t header[2];
    if (fread(header, 1, 2, file) != 2 || header[0] != 0xFF || header[1] != 0xD8) {
        LOGE_MP("[ExtractExif] Not a JPEG file");
        fclose(file);
        return exifData;
    }

    while (true) {
        int byte = fgetc(file);
        if (byte == EOF) break;
        if (byte != 0xFF) continue;

        int type = fgetc(file);
        if (type == EOF) break;
        while (type == 0xFF) {
            type = fgetc(file);
        }

        if (type == 0x00) continue; // FF 00 is not a marker (stuffed byte)
        if (type == 0xDA || type == 0xD9) break; // SOS or EOI

        // Segments without length
        if (type == 0x01 || (type >= 0xD0 && type <= 0xD7)) continue;

        // Read length
        uint8_t lenBytes[2];
        if (fread(lenBytes, 1, 2, file) != 2) break;
        uint16_t length = (lenBytes[0] << 8) | lenBytes[1];
        if (length < 2) break;

        if (type == 0xE1) { // APP1
            size_t payloadLen = length - 2;
            std::vector<uint8_t> buffer(payloadLen);
            if (fread(buffer.data(), 1, payloadLen, file) == payloadLen) {
                // Check for "Exif\0\0"
                if (payloadLen >= 6 &&
                    buffer[0] == 'E' && buffer[1] == 'x' && buffer[2] == 'i' &&
                    buffer[3] == 'f' && buffer[4] == 0x00 && buffer[5] == 0x00) {

                    exifData.assign(buffer.begin() + 6, buffer.end());
                    LOGD_MP("[ExtractExif] Found Exif data: %zu bytes", exifData.size());
                    fclose(file);
                    return exifData;
                }
            }
        } else {
            fseek(file, length - 2, SEEK_CUR);
        }
    }

    fclose(file);
    return exifData;
}


/**
 * Extract video data from Google HEIC Motion Photo
 * @param inputFilePath HEIC Motion Photo file path
 * @return MP4 video data byte array, returns null on failure
 */
extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideoByMpvdCC(
        JNIEnv *env, jclass clazz, jstring inputFilePath);

extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideoByXMP(
        JNIEnv *env, jclass clazz, jstring inputFilePath);

extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideo(
        JNIEnv *env, jclass clazz, jstring inputFilePath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeExtractGoogleHeicMotionPhotoVideo: START");
    LOGI_MP("============================================================");
    jbyteArray byMpvd = Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideoByMpvdCC(
            env, clazz, inputFilePath);
    bool isOk = false;
    if (byMpvd) {
        jsize len = env->GetArrayLength(byMpvd);
        if (len >= 12) {
            jbyte *p = env->GetByteArrayElements(byMpvd, nullptr);
            if (p) {
                isOk = (p[4] == 'f' && p[5] == 't' && p[6] == 'y' && p[7] == 'p');
                env->ReleaseByteArrayElements(byMpvd, p, JNI_ABORT);
            }
        }
        if (isOk) {
            return byMpvd;
        }
        env->DeleteLocalRef(byMpvd);
    }

    return Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideoByXMP(
            env, clazz, inputFilePath);
}

static bool ReadFileFully(const char *filePath, std::vector<uint8_t> &outData, long &outSize) {
    outData.clear();
    outSize = 0;
    if (!filePath) return false;
    FILE *file = fopen(filePath, "rb");
    if (!file) return false;
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (fileSize < 0) {
        fclose(file);
        return false;
    }
    outData.resize(static_cast<size_t>(fileSize));
    size_t bytesRead = fread(outData.data(), 1, static_cast<size_t>(fileSize), file);
    fclose(file);
    if (bytesRead != static_cast<size_t>(fileSize)) {
        outData.clear();
        return false;
    }
    outSize = fileSize;
    return true;
}

extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideoByMpvdCC(
        JNIEnv *env, jclass clazz, jstring inputFilePath) {
    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[ExtractHEIC][mpvd] Failed to get input file path");
        return nullptr;
    }
    LOGD_MP("[ExtractHEIC][mpvd] Input file: %s", filePath);

    std::vector<uint8_t> fileData;
    long fileSize = 0;
    if (!ReadFileFully(filePath, fileData, fileSize)) {
        LOGE_MP("[ExtractHEIC][mpvd] Failed to read file");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    LOGD_MP("[ExtractHEIC][mpvd] File size: %ld bytes (%.2f MB)", fileSize,
            fileSize / (1024.0 * 1024.0));

    if (fileSize < 16) {
        LOGE_MP("[ExtractHEIC][mpvd] File too small");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    const uint8_t mpvdSignature[4] = {0x6D, 0x70, 0x76, 0x64};
    long mpvdPos = -1;
    for (long i = fileSize - 4; i >= 4; --i) {
        if (fileData[i] == mpvdSignature[0] &&
            fileData[i + 1] == mpvdSignature[1] &&
            fileData[i + 2] == mpvdSignature[2] &&
            fileData[i + 3] == mpvdSignature[3]) {
            mpvdPos = i;
            LOGD_MP("[ExtractHEIC][mpvd] Found 'mpvd' signature at offset %ld", mpvdPos);
            break;
        }
    }

    if (mpvdPos < 4) {
        LOGW_MP("[ExtractHEIC][mpvd] 'mpvd' box not found");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    long boxSizePos = mpvdPos - 4;
    uint32_t boxSize = (static_cast<uint32_t>(fileData[boxSizePos]) << 24) |
                       (static_cast<uint32_t>(fileData[boxSizePos + 1]) << 16) |
                       (static_cast<uint32_t>(fileData[boxSizePos + 2]) << 8) |
                       static_cast<uint32_t>(fileData[boxSizePos + 3]);
    LOGD_MP("[ExtractHEIC][mpvd] box size = %u bytes", boxSize);

    if (boxSize < 8) {
        LOGE_MP("[ExtractHEIC][mpvd] Invalid box size: %u", boxSize);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    uint32_t mp4Size = boxSize - 8;
    long mp4StartPos = mpvdPos + 4;
    if (mp4StartPos < 0 || mp4Size == 0) {
        LOGE_MP("[ExtractHEIC][mpvd] Invalid mp4 range");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    if (mp4StartPos + mp4Size > static_cast<unsigned long>(fileSize)) {
        LOGE_MP("[ExtractHEIC][mpvd] MP4 data exceeds file bounds (offset=%ld, size=%u, fileSize=%ld)",
                mp4StartPos, mp4Size, fileSize);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Size));
    if (!result) {
        LOGE_MP("[ExtractHEIC][mpvd] Failed to allocate Java byte array for %u bytes", mp4Size);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    env->SetByteArrayRegion(result, 0, static_cast<jsize>(mp4Size),
                            reinterpret_cast<const jbyte *>(fileData.data() + mp4StartPos));

    env->ReleaseStringUTFChars(inputFilePath, filePath);
    return result;
}

extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideoByXMP(
        JNIEnv *env, jclass clazz, jstring inputFilePath) {
    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[ExtractHEIC][XMP] Failed to get input file path");
        return nullptr;
    }
    LOGD_MP("[ExtractHEIC][XMP] Input file: %s", filePath);

    std::vector<uint8_t> fileData;
    long fileSize = 0;
    if (!ReadFileFully(filePath, fileData, fileSize)) {
        LOGE_MP("[ExtractHEIC][XMP] Failed to read file");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    LOGD_MP("[ExtractHEIC][XMP] File size: %ld bytes (%.2f MB)", fileSize, fileSize / (1024.0 * 1024.0));
    if (fileSize < 16) {
        LOGE_MP("[ExtractHEIC][XMP] File too small");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    MotionPhotoXmpInfo xmpInfo = ParseHeicMotionPhotoXmpWithLibheif(filePath);
    if (!xmpInfo.isMotionPhoto) {
        LOGD_MP("[ExtractHEIC][XMP] Not a Motion Photo");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    long mp4Len = xmpInfo.videoLength;
    long mp4StartPos = -1;
    if (mp4Len > 0) {
        mp4StartPos = fileSize - mp4Len;
    } else if (xmpInfo.videoOffset > 0) {
        mp4StartPos = xmpInfo.videoOffset;
        mp4Len = static_cast<long>(fileSize - mp4StartPos);
    }

    if (mp4StartPos < 0 || mp4Len <= 0 || mp4StartPos + mp4Len > fileSize) {
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Len));
    if (!result) {
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    env->SetByteArrayRegion(result, 0, static_cast<jsize>(mp4Len),
                            reinterpret_cast<const jbyte *>(fileData.data() + mp4StartPos));

    env->ReleaseStringUTFChars(inputFilePath, filePath);
    return result;
}


/**
 * Extract video data from Google JPEG format Motion Photo
 *
 * Requirements:提取 Google JPEG 结构的动态照片的视频数据，以供 app 播放动态效果。
 *
 * JPEG Motion Photo 文件Structure:
 * - JPEG image data (starts with FF D8, ends with FF D9)
 * - [Optional] Padding bytes
 * - MP4 video data (starts with ftyp box)
 *
 * Supports two positioning methods:
 *
 * Method 1: Google XMP method (priority)
 * - Read XMP metadata from JPEG APP1 segment
 * - Find GCamera:MotionPhoto="1" to confirm it's a Motion Photo
 * - Use Item:Length to calculate video position from end of file
 * - Video start position = file size - Item:Length
 *
 * Method 2: Huawei ftyp search method (fallback)
 * - Search for "ftyp" identifier in file (beginning of MP4 file)
 * - 4 bytes before ftyp is box size, locate MP4 start position
 * - From MP4 start position to end of file is complete MP4 data
 *
 * @param inputFilePath JPEG Motion Photo 文件路径
 * @return MP4 video data byte array, returns null on failure
 */
extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ExtractJpegMotionPhotoVideo(
        JNIEnv *env, jclass clazz, jstring inputFilePath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeExtractGoogleJpegMotionPhotoVideo: START");
    LOGI_MP("============================================================");

    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[ExtractJPEGVideo] Failed to get input file path");
        return nullptr;
    }
    LOGD_MP("[ExtractJPEGVideo] Input file: %s", filePath);

    // Open file
    FILE *file = fopen(filePath, "rb");
    if (!file) {
        LOGE_MP("[ExtractJPEGVideo] Failed to open file: %s (errno=%d)", filePath, errno);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    LOGD_MP("[ExtractJPEGVideo] File size: %ld bytes (%.2f MB)", fileSize, fileSize / (1024.0 * 1024.0));

    if (fileSize < 16) {
        LOGE_MP("[ExtractJPEGVideo] File too small to be a valid Motion Photo");
        fclose(file);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // Read entire file into memory
    std::vector<uint8_t> fileData(fileSize);
    size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
    fclose(file);

    if (bytesRead != static_cast<size_t>(fileSize)) {
        LOGE_MP("[ExtractJPEGVideo] Failed to read file (read %zu of %ld)", bytesRead, fileSize);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    LOGD_MP("[ExtractJPEGVideo] File loaded into memory: %zu bytes", bytesRead);

    // Verify JPEG file header (FF D8 FF)
    if (fileData[0] != 0xFF || fileData[1] != 0xD8 || fileData[2] != 0xFF) {
        LOGE_MP("[ExtractJPEGVideo] Not a valid JPEG file (header: %02X %02X %02X)",
                fileData[0], fileData[1], fileData[2]);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    LOGD_MP("[ExtractJPEGVideo] Valid JPEG header detected (FF D8 FF)");

    long mp4StartPos = -1;
    uint32_t mp4Size = 0;
    const char *locateMethod = "unknown";

    // ==================== 方式一：Try XMP method ====================
    LOGI_MP("[ExtractJPEGVideo] Trying XMP method (Google style)...");
    MotionPhotoXmpInfo xmpInfo = ParseJpegMotionPhotoXmp(fileData);

    if (xmpInfo.isMotionPhoto && xmpInfo.videoLength > 0) {
        LOGI_MP(
                "[ExtractJPEGVideo] XMP method: Motion Photo confirmed, video length = %ld",
                xmpInfo.videoLength);

        // Calculate video start position
        mp4StartPos = fileSize - xmpInfo.videoLength;

        if (mp4StartPos > 0 && mp4StartPos < fileSize) {
            mp4Size = static_cast<uint32_t>(xmpInfo.videoLength);
            locateMethod = "XMP (Google)";

            // Verify if video data starts with ftyp
            if (mp4StartPos + 8 <= fileSize) {
                // Check if offset+4 position is "ftyp"
                if (fileData[mp4StartPos + 4] == 'f' &&
                    fileData[mp4StartPos + 5] == 't' &&
                    fileData[mp4StartPos + 6] == 'y' &&
                    fileData[mp4StartPos + 7] == 'p') {
                    LOGD_MP(
                            "[ExtractJPEGVideo] XMP method: Verified ftyp at calculated position");
                } else {
                    LOGW_MP("[ExtractJPEGVideo] XMP method: No ftyp at calculated position, "
                            "data may be invalid");
                    LOGW_MP("[ExtractJPEGVideo]   Expected 'ftyp', got: %02X %02X %02X %02X",
                            fileData[mp4StartPos + 4], fileData[mp4StartPos + 5],
                            fileData[mp4StartPos + 6], fileData[mp4StartPos + 7]);
                }
            }

            LOGD_MP("[ExtractJPEGVideo] XMP method: MP4 starts at offset %ld",
                    mp4StartPos);
        } else {
            LOGW_MP("[ExtractJPEGVideo] XMP method: Invalid calculated position %ld",
                    mp4StartPos);
            mp4StartPos = -1;
        }
    } else {
        if (!xmpInfo.isMotionPhoto) {
            LOGD_MP("[ExtractJPEGVideo] XMP method: Not a Google Motion Photo "
                    "(GCamera:MotionPhoto != 1)");
        } else {
            LOGD_MP("[ExtractJPEGVideo] XMP method: No video length in XMP (Item:Length "
                    "not found)");
        }
    }

    // ==================== 方式二：Fallback to ftyp search method ====================
    if (mp4StartPos < 0) {
        LOGI_MP("[ExtractJPEGVideo] Falling back to ftyp search method (Huawei style)...");

        const uint8_t ftypSignature[4] = {0x66, 0x74, 0x79, 0x70}; // "ftyp"
        long ftypPos = -1;

        // Search from middle of file onwards (because JPEG data is in front)
        long searchStart = (fileSize > 1024) ? 1024 : 0;

        for (long i = searchStart; i < fileSize - 4; ++i) {
            if (fileData[i] == ftypSignature[0] &&
                fileData[i + 1] == ftypSignature[1] &&
                fileData[i + 2] == ftypSignature[2] &&
                fileData[i + 3] == ftypSignature[3]) {
                ftypPos = i;
                LOGD_MP("[ExtractJPEGVideo] ftyp method: Found 'ftyp' signature at offset %ld", ftypPos);
                break;
            }
        }

        if (ftypPos >= 4) {
            // 4 bytes before ftyp is ftyp box size
            mp4StartPos = ftypPos - 4;
            mp4Size = static_cast<uint32_t>(fileSize - mp4StartPos);
            locateMethod = "ftyp search (Huawei)";

            // Read ftyp box size to verify
            uint32_t ftypBoxSize =
                    (static_cast<uint32_t>(fileData[mp4StartPos]) << 24) |
                    (static_cast<uint32_t>(fileData[mp4StartPos + 1]) << 16) |
                    (static_cast<uint32_t>(fileData[mp4StartPos + 2]) << 8) |
                    static_cast<uint32_t>(fileData[mp4StartPos + 3]);

            LOGD_MP("[ExtractJPEGVideo] ftyp method: ftyp box size = %u bytes", ftypBoxSize);

            // Verify if ftyp box size is reasonable
            if (ftypBoxSize < 8 || ftypBoxSize > 256) {
                LOGW_MP("[ExtractJPEGVideo] ftyp method: Unusual ftyp box size: %u "
                        "(expected 8-256)",
                        ftypBoxSize);
            }

            LOGD_MP("[ExtractJPEGVideo] ftyp method: MP4 starts at offset %ld",
                    mp4StartPos);
        } else {
            LOGE_MP(
                    "[ExtractJPEGVideo] ftyp method: 'ftyp' not found or invalid position");
        }
    }

    // ==================== Extraction result check ====================
    if (mp4StartPos < 0 || mp4Size == 0) {
        LOGE_MP("[ExtractJPEGVideo] Failed to locate MP4 video data");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    LOGD_MP("[ExtractJPEGVideo] MP4 data: offset %ld, size %u bytes, method: %s",
            mp4StartPos, mp4Size, locateMethod);

    // Print MP4 file header information
    if (mp4Size >= 12 && mp4StartPos + 12 <= fileSize) {
        uint32_t firstBoxSize =
                (static_cast<uint32_t>(fileData[mp4StartPos]) << 24) |
                (static_cast<uint32_t>(fileData[mp4StartPos + 1]) << 16) |
                (static_cast<uint32_t>(fileData[mp4StartPos + 2]) << 8) |
                static_cast<uint32_t>(fileData[mp4StartPos + 3]);

        LOGD_MP("[ExtractJPEGVideo] MP4 first box: size=%u, type='%c%c%c%c', "
                "brand='%c%c%c%c'",
                firstBoxSize, fileData[mp4StartPos + 4], fileData[mp4StartPos + 5],
                fileData[mp4StartPos + 6], fileData[mp4StartPos + 7],
                fileData[mp4StartPos + 8], fileData[mp4StartPos + 9],
                fileData[mp4StartPos + 10], fileData[mp4StartPos + 11]);
    }

    // Calculate JPEG image size
    long jpegSize = mp4StartPos;
    LOGD_MP("[ExtractJPEGVideo] JPEG image size: %ld bytes (%.2f KB)", jpegSize, jpegSize / 1024.0);

    // Create Java byte array
    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Size));
    if (!result) {
        LOGE_MP("[ExtractJPEGVideo] Failed to allocate Java byte array for %u bytes", mp4Size);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // Copy MP4 data to Java byte array
    env->SetByteArrayRegion(result, 0, static_cast<jsize>(mp4Size),
                            reinterpret_cast<const jbyte *>(fileData.data() + mp4StartPos));

    LOGI_MP("============================================================");
    LOGI_MP("SUCCESS!");
    LOGI_MP("  Input file: %s", filePath);
    LOGI_MP("  File size: %ld bytes", fileSize);
    LOGI_MP("  Locate method: %s", locateMethod);
    if (xmpInfo.isMotionPhoto) {
        LOGI_MP("  XMP info: MotionPhoto=1, version=%d, videoLength=%ld",
                xmpInfo.version, xmpInfo.videoLength);
    }
    LOGI_MP("  JPEG size: %ld bytes (%.2f KB)", jpegSize, jpegSize / 1024.0);
    LOGI_MP("  MP4 offset: %ld", mp4StartPos);
    LOGI_MP("  MP4 extracted: %u bytes (%.2f MB)", mp4Size,
            mp4Size / (1024.0 * 1024.0));
    LOGI_MP("============================================================");

    env->ReleaseStringUTFChars(inputFilePath, filePath);
    return result;
}

/**
 * Requirements:根据输入的图片地址，检查图片是否为哪种 Motion Photo。
 *
 * Logic:
 * 1. Read file raw bytes to determine the actual format is JPEG or HEIC.
 *
 * 2. If format is JPEG
 * 2.1 Read XMP, if MotionPhoto="1" or MicroVideo="1" field exists, determine as JPEG_MOTION_PHOTO
 * 2.2 如果上述不成立，搜索 ftyp 标识（MP4 文件头），如果存在特定的“mp4”的视频字符，则确定为 JPEG_MOTION_PHOTO
 * 2.3 Otherwise return MOTION_PHOTO_TYPE_NONE
 *
 * 3. If format is HEIC
 * 3.1 Use libheif to read XMP, if MotionPhoto="1" or MicroVideo="1" field exists, determine as HEIC_MOTION_PHOTO
 * 3.2 如果上述不成立，则检索 “mpvd” 字符，如果存在，则确定为 HEIC_MOTION_PHOTO
 * 3.3 Otherwise return MOTION_PHOTO_TYPE_NONE
 *
 * 4. If file is neither JPEG nor HEIC, directly return MOTION_PHOTO_TYPE_NONE
 *
 * Return: Motion Photo Type enum:
 * JPEG_MOTION_PHOTO(0): JPEG format motion photo
 * HEIC_MOTION_PHOTO(1): HEIC format motion photo
 * MOTION_PHOTO_TYPE_NONE(2): Not a motion photo
 * */

// Motion Photo Type enum values
#define MOTION_PHOTO_TYPE_JPEG 0
#define MOTION_PHOTO_TYPE_HEIC 1
#define MOTION_PHOTO_TYPE_NONE 2

extern "C" JNIEXPORT jint

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_CheckMotionPhotoType(
        JNIEnv *env, jclass clazz, jstring inputFilePath) {
    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[CheckType] Failed to get input file path");
        return MOTION_PHOTO_TYPE_NONE;
    }

    // Open file
    FILE *file = fopen(filePath, "rb");
    if (!file) {
        LOGE_MP("[CheckType] Failed to open file: %s", filePath);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return MOTION_PHOTO_TYPE_NONE;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (fileSize < 12) {
        LOGD_MP("[CheckType] File too small: %ld bytes", fileSize);
        fclose(file);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return MOTION_PHOTO_TYPE_NONE;
    }

    // 读取文件头（足够识别格式）
    uint8_t header[12];
    if (fread(header, 1, 12, file) != 12) {
        LOGE_MP("[CheckType] Failed to read file header");
        fclose(file);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return MOTION_PHOTO_TYPE_NONE;
    }

    // Determine file format
    bool isJpeg = (header[0] == 0xFF && header[1] == 0xD8 && header[2] == 0xFF);
    bool isHeic = (header[4] == 'f' && header[5] == 't' && header[6] == 'y' && header[7] == 'p');

    LOGD_MP("[CheckType] File: %s, size: %ld, isJpeg: %d, isHeic: %d",
            filePath, fileSize, isJpeg, isHeic);

    if (isJpeg) {
        // ==================== JPEG format processing ====================
        LOGD_MP("[CheckType] Detected JPEG format");

        // Read entire file
        fseek(file, 0, SEEK_SET);
        std::vector<uint8_t> fileData(fileSize);
        size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
        fclose(file);
        file = nullptr;

        if (bytesRead != static_cast<size_t>(fileSize)) {
            LOGE_MP("[CheckType] Failed to read JPEG file");
            env->ReleaseStringUTFChars(inputFilePath, filePath);
            return MOTION_PHOTO_TYPE_NONE;
        }

        // Method 1: Check XMP metadata
        MotionPhotoXmpInfo xmpInfo = ParseJpegMotionPhotoXmp(fileData);
        LOGD_MP("[CheckType] JPEG XMP check: isMotionPhoto=%d",
                xmpInfo.isMotionPhoto);

        // If XMP confirms it's a motion photo, return directly
        if (xmpInfo.isMotionPhoto) {
            LOGI_MP("[CheckType] Result: JPEG_MOTION_PHOTO (XMP=1)");
            env->ReleaseStringUTFChars(inputFilePath, filePath);
            return MOTION_PHOTO_TYPE_JPEG;
        }

        // Method 2: XMP not confirmed, search for ftyp identifier (MP4 file header)
        const uint8_t ftypSignature[4] = {0x66, 0x74, 0x79, 0x70}; // "ftyp"
        // Start searching after JPEG data (skip first 1KB JPEG header area)
        long searchStart = (fileSize > 1024) ? 1024 : 0;
        for (long i = searchStart; i < fileSize - 4; ++i) {
            if (fileData[i] == ftypSignature[0] &&
                fileData[i + 1] == ftypSignature[1] &&
                fileData[i + 2] == ftypSignature[2] &&
                fileData[i + 3] == ftypSignature[3]) {
                LOGD_MP("[CheckType] JPEG ftyp found at offset %ld", i);
                LOGI_MP("[CheckType] Result: JPEG_MOTION_PHOTO (ftyp=1)");
                env->ReleaseStringUTFChars(inputFilePath, filePath);
                return MOTION_PHOTO_TYPE_JPEG;
            }
        }

        LOGD_MP("[CheckType] JPEG is not a Motion Photo");

    } else if (isHeic) {
        // ==================== HEIC format processing ====================
        LOGD_MP("[CheckType] Detected HEIC format");
        fclose(file);
        file = nullptr;

        // Method 1: Use libheif to check XMP metadata
        MotionPhotoXmpInfo xmpInfo = ParseHeicMotionPhotoXmpWithLibheif(filePath);
        LOGD_MP("[CheckType] HEIC XMP check: isMotionPhoto=%d",
                xmpInfo.isMotionPhoto);

        // If XMP confirms it's a motion photo, return directly
        if (xmpInfo.isMotionPhoto) {
            LOGI_MP("[CheckType] Result: HEIC_MOTION_PHOTO (XMP=1)");
            env->ReleaseStringUTFChars(inputFilePath, filePath);
            return MOTION_PHOTO_TYPE_HEIC;
        }

        // Method 2: XMP not confirmed, search for ftyp identifier in mpvd box
        FILE *heicFile = fopen(filePath, "rb");
        if (heicFile) {
            std::vector<uint8_t> fileData(fileSize);
            fread(fileData.data(), 1, fileSize, heicFile);
            fclose(heicFile);

            // Search for mpvd box
            const uint8_t mpvdSignature[4] = {0x6D, 0x70, 0x76, 0x64}; // "mpvd"
            for (long i = fileSize - 4; i >= 4; --i) {
                if (fileData[i] == mpvdSignature[0] &&
                    fileData[i + 1] == mpvdSignature[1] &&
                    fileData[i + 2] == mpvdSignature[2] &&
                    fileData[i + 3] == mpvdSignature[3]) {
                    // Found mpvd box, check if it contains ftyp
                    long mpvdDataStart = i + 4;
                    if (mpvdDataStart + 8 <= fileSize) {
                        // Check if mpvd data area starts with ftyp
                        if (fileData[mpvdDataStart + 4] == 'f' &&
                            fileData[mpvdDataStart + 5] == 't' &&
                            fileData[mpvdDataStart + 6] == 'y' &&
                            fileData[mpvdDataStart + 7] == 'p') {
                            LOGD_MP("[CheckType] HEIC mpvd+ftyp found at offset %ld", i);
                            LOGI_MP("[CheckType] Result: HEIC_MOTION_PHOTO (ftyp=1)");
                            env->ReleaseStringUTFChars(inputFilePath, filePath);
                            return MOTION_PHOTO_TYPE_HEIC;
                        }
                    }
                    break;
                }
            }
        }

        LOGD_MP("[CheckType] HEIC is not a Motion Photo");

    } else {
        // Unknown format
        LOGD_MP("[CheckType] Unknown format (header: %02X %02X %02X %02X %02X %02X "
                "%02X %02X)",
                header[0], header[1], header[2], header[3], header[4], header[5],
                header[6], header[7]);
        fclose(file);
        file = nullptr;
    }

    // Cleanup and return not a motion photo
    if (file) {
        fclose(file);
    }
    env->ReleaseStringUTFChars(inputFilePath, filePath);

    LOGI_MP("[CheckType] Final result: NONE (not a Motion Photo)");
    return MOTION_PHOTO_TYPE_NONE;
}

static std::vector<uint8_t> JByteArrayToVector(JNIEnv *env, jbyteArray arr) {
    std::vector<uint8_t> out;
    if (!env || !arr) return out;
    jsize len = env->GetArrayLength(arr);
    if (len <= 0) return out;
    out.resize(static_cast<size_t>(len));
    jbyte *p = env->GetByteArrayElements(arr, nullptr);
    if (!p) {
        out.clear();
        return out;
    }
    memcpy(out.data(), p, static_cast<size_t>(len));
    env->ReleaseByteArrayElements(arr, p, JNI_ABORT);
    return out;
}

static std::vector<uint8_t> ExtractHeicExifTiff(const char *filePath) {
    std::vector<uint8_t> out;
    if (!filePath) return out;

    heif_context *ctx = heif_context_alloc();
    if (!ctx) return out;

    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
    if (err.code != heif_error_Ok) {
        LOGE_MP("[ExtractHeicExif] Failed to read HEIC file: %s", err.message);
        heif_context_free(ctx);
        return out;
    }

    heif_image_handle *handle = nullptr;
    err = heif_context_get_primary_image_handle(ctx, &handle);
    if (err.code != heif_error_Ok || !handle) {
        LOGE_MP("[ExtractHeicExif] Failed to get primary image handle: %s", err.message);
        heif_context_free(ctx);
        return out;
    }

    // Get all metadata blocks and filter manually to be more robust
    int numMetadata = heif_image_handle_get_number_of_metadata_blocks(handle, nullptr);
    LOGD_MP("[ExtractHeicExif] Found %d metadata blocks", numMetadata);

    if (numMetadata > 0) {
        std::vector<heif_item_id> metadataIds(numMetadata);
        heif_image_handle_get_list_of_metadata_block_IDs(handle, nullptr, metadataIds.data(),
                                                         numMetadata);
        for (int i = 0; i < numMetadata; i++) {
            heif_item_id id = metadataIds[i];
            const char *type = heif_image_handle_get_metadata_type(handle, id);
            const char *contentType = heif_image_handle_get_metadata_content_type(handle, id);

            LOGD_MP("[ExtractHeicExif] Metadata[%d]: id=%u, type='%s', contentType='%s'",
                    i, id, type ? type : "(null)", contentType ? contentType : "(null)");

            // Check if it is Exif (case-insensitive check for robustness)
            // Standard type is "Exif", but some might use "exif"
            if (!type || strcasecmp(type, "Exif") != 0) continue;

            size_t metadataSize = heif_image_handle_get_metadata_size(handle, id);
            if (metadataSize == 0) {
                LOGD_MP("[ExtractHeicExif] Skipping empty Exif block");
                continue;
            }

            std::vector<uint8_t> buf(metadataSize);
            err = heif_image_handle_get_metadata(handle, id, buf.data());
            if (err.code != heif_error_Ok) {
                LOGE_MP("[ExtractHeicExif] Failed to read Exif metadata: %s", err.message);
                continue;
            }

            // 1. Direct TIFF header (II/MM)
            if (buf.size() >= 2 &&
                ((buf[0] == 'I' && buf[1] == 'I') || (buf[0] == 'M' && buf[1] == 'M'))) {
                out = std::move(buf);
                LOGD_MP("[ExtractHeicExif] Found Exif data (direct TIFF header), size=%zu", out.size());
                break;
            }

            // 2. Skipped 4 bytes (e.g. offset size or unknown header)
            if (buf.size() >= 6 &&
                ((buf[4] == 'I' && buf[5] == 'I') || (buf[4] == 'M' && buf[5] == 'M'))) {
                out.assign(buf.begin() + 4, buf.end());
                LOGD_MP("[ExtractHeicExif] Found Exif data (skipped 4 bytes), size=%zu", out.size());
                break;
            }

            // 3. Offset specified in first 4 bytes (Big Endian)
            if (buf.size() >= 8) {
                uint32_t offset = (static_cast<uint32_t>(buf[0]) << 24) |
                                  (static_cast<uint32_t>(buf[1]) << 16) |
                                  (static_cast<uint32_t>(buf[2]) << 8) |
                                  static_cast<uint32_t>(buf[3]);
                if (offset + 2 <= buf.size() &&
                    ((buf[offset] == 'I' && buf[offset + 1] == 'I') ||
                     (buf[offset] == 'M' && buf[offset + 1] == 'M'))) {
                    out.assign(buf.begin() + static_cast<long>(offset), buf.end());
                    LOGD_MP("[ExtractHeicExif] Found Exif data (offset %u), size=%zu", offset, out.size());
                    break;
                }
            }

            LOGD_MP("[ExtractHeicExif] Exif block found but no valid TIFF header detected");
        }
    } else {
        LOGD_MP("[ExtractHeicExif] No metadata blocks found");
    }

    heif_image_handle_release(handle);
    heif_context_free(ctx);
    return out;
}

static bool IsLikelyJpegBytes(const std::vector<uint8_t> &data) {
    if (data.size() < 4) return false;
    if (data[0] != 0xFF || data[1] != 0xD8) return false;
    if (data[data.size() - 2] != 0xFF || data[data.size() - 1] != 0xD9) return false;
    return true;
}

static bool HasJpegSoi(const std::vector<uint8_t> &data) {
    return data.size() >= 2 && data[0] == 0xFF && data[1] == 0xD8;
}

static bool IsLikelyJpegSlice(const std::vector<uint8_t> &data, long start, long len) {
    if (start < 0 || len < 4) return false;
    if (static_cast<size_t>(start + len) > data.size()) return false;
    if (data[static_cast<size_t>(start)] != 0xFF || data[static_cast<size_t>(start + 1)] != 0xD8) {
        return false;
    }
    if (data[static_cast<size_t>(start + len - 2)] != 0xFF ||
        data[static_cast<size_t>(start + len - 1)] != 0xD9) {
        return false;
    }
    return true;
}

static bool IsLikelyMp4Slice(const std::vector<uint8_t> &data, long start, long len) {
    if (start < 0 || len < 12) return false;
    if (static_cast<size_t>(start + len) > data.size()) return false;
    size_t ftypPos = static_cast<size_t>(start + 4);
    return data[ftypPos] == 'f' && data[ftypPos + 1] == 't' &&
           data[ftypPos + 2] == 'y' && data[ftypPos + 3] == 'p';
}

static long CalcPrimaryJpegLength(const std::vector<uint8_t> &jpegBytes) {
    if (jpegBytes.size() < 4) {
        return -1;
    }
    if (jpegBytes[0] != 0xFF || jpegBytes[1] != 0xD8) {
        return -1;
    }

    size_t offset = 2;
    const size_t size = jpegBytes.size();

    while (offset < size) {
        if (jpegBytes[offset] != 0xFF) {
            offset++;
            continue;
        }

        while (offset < size && jpegBytes[offset] == 0xFF) {
            offset++;
        }
        if (offset >= size) break;

        uint8_t marker = jpegBytes[offset];
        offset++;

        if ((marker >= 0xD0 && marker <= 0xD7) || marker == 0xD8 || marker == 0xD9 || marker == 0x01) {
            if (marker == 0xD9) {
                return static_cast<long>(offset);
            }
            continue;
        }

        if (marker == 0xDA) {
            if (offset + 2 > size) return -1;
            uint16_t len = (static_cast<uint16_t>(jpegBytes[offset]) << 8) |
                           static_cast<uint16_t>(jpegBytes[offset + 1]);
            if (len < 2) return -1;
            offset += len;

            while (offset + 1 < size) {
                uint8_t b = jpegBytes[offset];
                if (b == 0xFF) {
                    uint8_t next = jpegBytes[offset + 1];
                    if (next == 0x00) {
                        offset += 2;
                        continue;
                    }
                    if (next == 0xD9) {
                        return static_cast<long>(offset + 2);
                    }
                }
                offset++;
            }
            break;
        }

        if (offset + 2 > size) return -1;
        uint16_t len = (static_cast<uint16_t>(jpegBytes[offset]) << 8) |
                       static_cast<uint16_t>(jpegBytes[offset + 1]);
        if (len < 2) return -1;
        offset += len;
    }

    return -1;
}

static long CalcJpegLengthInSlice(const std::vector<uint8_t> &data, long start, long len) {
    if (start < 0 || len < 4) return -1;
    if (static_cast<size_t>(start + len) > data.size()) return -1;
    if (data[static_cast<size_t>(start)] != 0xFF || data[static_cast<size_t>(start + 1)] != 0xD8) {
        return -1;
    }

    size_t offset = static_cast<size_t>(start + 2);
    const size_t end = static_cast<size_t>(start + len);

    while (offset < end) {
        if (data[offset] != 0xFF) {
            offset++;
            continue;
        }

        while (offset < end && data[offset] == 0xFF) {
            offset++;
        }
        if (offset >= end) break;

        uint8_t marker = data[offset];
        offset++;

        if ((marker >= 0xD0 && marker <= 0xD7) || marker == 0xD8 || marker == 0xD9 || marker == 0x01) {
            if (marker == 0xD9) {
                return static_cast<long>(offset - static_cast<size_t>(start));
            }
            continue;
        }

        if (marker == 0xDA) {
            if (offset + 2 > end) return -1;
            uint16_t l = (static_cast<uint16_t>(data[offset]) << 8) |
                         static_cast<uint16_t>(data[offset + 1]);
            if (l < 2) return -1;
            offset += l;

            while (offset + 1 < end) {
                uint8_t b = data[offset];
                if (b == 0xFF) {
                    uint8_t next = data[offset + 1];
                    if (next == 0x00) {
                        offset += 2;
                        continue;
                    }
                    if (next == 0xD9) {
                        return static_cast<long>(offset + 2 - static_cast<size_t>(start));
                    }
                }
                offset++;
            }
            break;
        }

        if (offset + 2 > end) return -1;
        uint16_t l = (static_cast<uint16_t>(data[offset]) << 8) |
                     static_cast<uint16_t>(data[offset + 1]);
        if (l < 2) return -1;
        offset += l;
    }

    return -1;
}

static std::vector<uint8_t> SliceBytes(const std::vector<uint8_t> &data, long start, long len) {
    std::vector<uint8_t> out;
    if (start < 0 || len <= 0) return out;
    if (static_cast<size_t>(start + len) > data.size()) return out;
    out.insert(out.end(), data.begin() + start, data.begin() + start + len);
    return out;
}

static std::vector<uint8_t> ExtractPrimaryJpegFromMotionPhotoBytes(const std::vector<uint8_t> &data,
                                                                   long start, long len, long padding) {
    if (start < 0 || len < 4) return {};
    if (static_cast<size_t>(start + len) > data.size()) return {};
    if (data[static_cast<size_t>(start)] != 0xFF || data[static_cast<size_t>(start + 1)] != 0xD8) {
        return {};
    }
    long actualLen = CalcJpegLengthInSlice(data, start, len);
    if (actualLen <= 0 || actualLen > len) {
        return {};
    }
    if (actualLen != len) {
        if (padding < 0 || len - actualLen != padding) {
            LOGE_MP("[Convert] Primary JPEG padding mismatch: sliceLen=%ld actualLen=%ld padding=%ld",
                    len, actualLen, padding);
            return {};
        }
        LOGD_MP("[Convert] Primary JPEG trimmed from %ld to %ld bytes (padding=%ld)",
                len, actualLen, padding);
    } else if (padding != 0) {
        LOGE_MP("[Convert] Primary JPEG padding must match XMP (padding=%ld but no trimming)", padding);
        return {};
    }
    return SliceBytes(data, start, actualLen);
}

static std::vector<uint8_t> ExtractGainMapJpegFromMotionPhotoBytes(const std::vector<uint8_t> &data,
                                                                   long start, long len, long padding) {
    if (start < 0 || len < 4) return {};
    if (static_cast<size_t>(start + len) > data.size()) return {};
    if (data[static_cast<size_t>(start)] != 0xFF || data[static_cast<size_t>(start + 1)] != 0xD8) {
        return {};
    }
    if (padding < 0) return {};
    return SliceBytes(data, start, len);
}

static std::vector<uint8_t> ExtractMp4FromMotionPhotoBytes(const std::vector<uint8_t> &data,
                                                           long start, long len, long padding) {
    if (start < 0 || len < 12) return {};
    if (static_cast<size_t>(start + len) > data.size()) return {};
    if (!IsLikelyMp4Slice(data, start, len)) return {};
    long actualLen = len;
    if (padding > 0) {
        if (padding > len) return {};
        actualLen = len - padding;
    } else if (padding < 0) {
        return {};
    }
    if (actualLen <= 12) return {};
    if (actualLen != len) {
        LOGD_MP("[Convert] MP4 trimmed from %ld to %ld bytes (padding=%ld)",
                len, actualLen, padding);
    } else if (padding != 0) {
        LOGE_MP("[Convert] MP4 padding must match XMP (padding=%ld but no trimming)", padding);
        return {};
    }
    return SliceBytes(data, start, actualLen);
}

static std::vector<uint8_t> EncodeHeifHandleToJpeg(heif_image_handle *handle, int quality) {
    std::vector<uint8_t> out;
    if (!handle) return out;

    heif_image *image = nullptr;
    heif_error err = heif_decode_image(handle, &image, heif_colorspace_RGB,
                                       heif_chroma_interleaved_RGB, nullptr);
    if (err.code != heif_error_Ok || !image) {
        return out;
    }

    int stride = 0;
    const uint8_t *plane = heif_image_get_plane_readonly(image, heif_channel_interleaved, &stride);
    int width = heif_image_get_width(image, heif_channel_interleaved);
    int height = heif_image_get_height(image, heif_channel_interleaved);

    if (!plane || width <= 0 || height <= 0 || stride <= 0) {
        heif_image_release(image);
        return out;
    }

    jpeg_compress_struct cinfo;
    jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_compress(&cinfo);

    unsigned char *jpegBuf = nullptr;
    unsigned long jpegSize = 0;
    jpeg_mem_dest(&cinfo, &jpegBuf, &jpegSize);

    cinfo.image_width = width;
    cinfo.image_height = height;
    cinfo.input_components = 3;
    cinfo.in_color_space = JCS_RGB;

    jpeg_set_defaults(&cinfo);
    jpeg_set_quality(&cinfo, quality, TRUE);
    jpeg_start_compress(&cinfo, TRUE);

    std::vector<uint8_t> row(static_cast<size_t>(width) * 3);
    JSAMPROW rowPtr[1];
    while (cinfo.next_scanline < cinfo.image_height) {
        const uint8_t *src = plane + static_cast<size_t>(cinfo.next_scanline) * stride;
        memcpy(row.data(), src, row.size());
        rowPtr[0] = row.data();
        jpeg_write_scanlines(&cinfo, rowPtr, 1);
    }

    jpeg_finish_compress(&cinfo);
    jpeg_destroy_compress(&cinfo);

    if (jpegBuf && jpegSize > 0) {
        out.assign(jpegBuf, jpegBuf + jpegSize);
        free(jpegBuf);
    } else if (jpegBuf) {
        free(jpegBuf);
    }

    heif_image_release(image);
    return out;
}

static std::vector<uint8_t> ExtractHeicHdrData(const char *filePath) {
    std::vector<uint8_t> out;
    if (!filePath) return out;

    heif_context *ctx = heif_context_alloc();
    if (!ctx) return out;

    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
    if (err.code != heif_error_Ok) {
        LOGE_MP("[ExtractHeicHdr] Failed to read HEIC file: %s", err.message);
        heif_context_free(ctx);
        return out;
    }

    heif_image_handle *handle = nullptr;
    err = heif_context_get_primary_image_handle(ctx, &handle);
    if (err.code != heif_error_Ok || !handle) {
        LOGE_MP("[ExtractHeicHdr] Failed to get primary image handle: %s", err.message);
        heif_context_free(ctx);
        return out;
    }

    int numMetadata = heif_image_handle_get_number_of_metadata_blocks(handle, nullptr);
    LOGD_MP("[ExtractHeicHdr] Found %d metadata blocks", numMetadata);

    if (numMetadata > 0) {
        std::vector<heif_item_id> metadataIds(numMetadata);
        heif_image_handle_get_list_of_metadata_block_IDs(handle, nullptr, metadataIds.data(), numMetadata);

        for (int i = 0; i < numMetadata; i++) {
            heif_item_id id = metadataIds[i];
            const char *type = heif_image_handle_get_metadata_type(handle, id);
            const char *contentType = heif_image_handle_get_metadata_content_type(handle, id);

            LOGD_MP("[ExtractHeicHdr] Metadata[%d]: id=%u, type='%s', contentType='%s'",
                    i, id, type ? type : "(null)", contentType ? contentType : "(null)");

            if (!type || strcmp(type, "mime") != 0) continue;

            bool isGainMapType = false;
            if (contentType) {
                std::string ct(contentType);
                std::transform(ct.begin(), ct.end(), ct.begin(),
                               [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
                if (ct.find("jpeg+gainmap") != std::string::npos || ct.find("gainmap") != std::string::npos) {
                    isGainMapType = true;
                }
            }

            size_t metadataSize = heif_image_handle_get_metadata_size(handle, id);
            if (metadataSize == 0) {
                LOGD_MP("[ExtractHeicHdr] Skipping empty mime block");
                continue;
            }

            std::vector<uint8_t> buf(metadataSize);
            err = heif_image_handle_get_metadata(handle, id, buf.data());
            if (err.code != heif_error_Ok) {
                LOGE_MP("[ExtractHeicHdr] Failed to read mime metadata: %s", err.message);
                continue;
            }

            if (!HasJpegSoi(buf)) {
                LOGD_MP("[ExtractHeicHdr] Skipping non-JPEG mime metadata");
                continue;
            }

            if (!isGainMapType) {
                LOGD_MP("[ExtractHeicHdr] Skipping JPEG mime metadata without gainmap content-type");
                continue;
            }

            if (!IsLikelyJpegBytes(buf)) {
                LOGW_MP("[ExtractHeicHdr] GainMap JPEG missing EOI, accepting by SOI only");
            }

            out = std::move(buf);
            LOGD_MP("[ExtractHeicHdr] Found HDR gainmap metadata, size=%zu", out.size());
            break;
        }
    } else {
        LOGD_MP("[ExtractHeicHdr] No metadata blocks found");
    }

    heif_image_handle_release(handle);
    heif_context_free(ctx);
    return out;
}

static std::vector<uint8_t> ExtractPrimaryJpegFromHeic(const char *filePath, int quality) {
    std::vector<uint8_t> out;
    if (!filePath) return out;

    heif_context *ctx = heif_context_alloc();
    if (!ctx) return out;

    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
    if (err.code != heif_error_Ok) {
        heif_context_free(ctx);
        return out;
    }

    heif_image_handle *handle = nullptr;
    err = heif_context_get_primary_image_handle(ctx, &handle);
    if (err.code != heif_error_Ok || !handle) {
        heif_context_free(ctx);
        return out;
    }

    out = EncodeHeifHandleToJpeg(handle, quality);
    heif_image_handle_release(handle);
    heif_context_free(ctx);
    return out;
}


static size_t FindJpegAppInsertPos(const std::vector<uint8_t> &jpeg) {
    if (jpeg.size() < 4) return 0;
    if (jpeg[0] != 0xFF || jpeg[1] != 0xD8) return 0;

    size_t pos = 2;
    while (pos + 4 <= jpeg.size()) {
        if (jpeg[pos] != 0xFF) break;
        uint8_t marker = jpeg[pos + 1];
        if (marker == 0xDA || marker == 0xD9) break;
        if (marker >= 0xE0 && marker <= 0xEF) {
            uint16_t len = (static_cast<uint16_t>(jpeg[pos + 2]) << 8) |
                           static_cast<uint16_t>(jpeg[pos + 3]);
            if (len < 2) break;
            size_t next = pos + 2 + static_cast<size_t>(len);
            if (next > jpeg.size()) break;
            pos = next;
            continue;
        }
        break;
    }
    return pos;
}

static bool AppendApp1Segment(std::vector<uint8_t> &out, const uint8_t *payload, size_t payloadLen) {
    if (!payload || payloadLen == 0) return true;
    size_t segLen = payloadLen + 2;
    if (segLen > 0xFFFF) return false;
    out.push_back(0xFF);
    out.push_back(0xE1);
    out.push_back(static_cast<uint8_t>((segLen >> 8) & 0xFF));
    out.push_back(static_cast<uint8_t>(segLen & 0xFF));
    out.insert(out.end(), payload, payload + payloadLen);
    return true;
}

static std::vector<uint8_t> BuildJpegMotionPhotoBytes(
        const std::vector<uint8_t> &primaryJpeg,
        const std::string &xmpXml,
        const std::vector<uint8_t> &exifTiff,
        const std::vector<uint8_t> &hdrData,
        const std::vector<uint8_t> &mp4,
        size_t primaryPadding,
        size_t hdrPadding,
        size_t videoPadding) {
    std::vector<uint8_t> out;
    if (primaryJpeg.size() < 4 || primaryJpeg[0] != 0xFF || primaryJpeg[1] != 0xD8) {
        return out;
    }
    if (mp4.empty()) {
        return out;
    }

    size_t insertPos = FindJpegAppInsertPos(primaryJpeg);
    if (insertPos == 0 || insertPos > primaryJpeg.size()) return out;

    size_t reserveSize = primaryJpeg.size() + mp4.size() + exifTiff.size() + xmpXml.size() +
                         hdrData.size() + primaryPadding + hdrPadding + videoPadding + 256;
    out.reserve(reserveSize);

    out.insert(out.end(), primaryJpeg.begin(), primaryJpeg.begin() + static_cast<long>(insertPos));

    if (!exifTiff.empty()) {
        const uint8_t exifHeader[] = {'E', 'x', 'i', 'f', 0x00, 0x00};
        std::vector<uint8_t> exifPayload;
        exifPayload.reserve(sizeof(exifHeader) + exifTiff.size());
        exifPayload.insert(exifPayload.end(), exifHeader, exifHeader + sizeof(exifHeader));
        exifPayload.insert(exifPayload.end(), exifTiff.begin(), exifTiff.end());
        if (!AppendApp1Segment(out, exifPayload.data(), exifPayload.size())) {
            out.clear();
            return out;
        }
    }

    if (!xmpXml.empty()) {
        const char *xmpMarker = "http://ns.adobe.com/xap/1.0/";
        size_t markerLen = strlen(xmpMarker);
        std::vector<uint8_t> xmpPayload;
        xmpPayload.reserve(markerLen + 1 + xmpXml.size());
        xmpPayload.insert(xmpPayload.end(),
                          reinterpret_cast<const uint8_t *>(xmpMarker),
                          reinterpret_cast<const uint8_t *>(xmpMarker) + markerLen);
        xmpPayload.push_back(0x00);
        xmpPayload.insert(xmpPayload.end(),
                          reinterpret_cast<const uint8_t *>(xmpXml.data()),
                          reinterpret_cast<const uint8_t *>(xmpXml.data() + xmpXml.size()));
        if (!AppendApp1Segment(out, xmpPayload.data(), xmpPayload.size())) {
            out.clear();
            return out;
        }
    }

    // primary image
    out.insert(out.end(), primaryJpeg.begin() + static_cast<long>(insertPos), primaryJpeg.end());
    if (primaryPadding > 0) {
        out.insert(out.end(), primaryPadding, 0x00);
    }

    //hdr
    if (!hdrData.empty()) {
        out.insert(out.end(), hdrData.begin(), hdrData.end());
        if (hdrPadding > 0) {
            out.insert(out.end(), hdrPadding, 0x00);
        }
    } else if (hdrPadding > 0) {
        out.insert(out.end(), hdrPadding, 0x00);
    }

    //mp4
    out.insert(out.end(), mp4.begin(), mp4.end());
    if (videoPadding > 0) {
        out.insert(out.end(), videoPadding, 0x00);
    }
    return out;
}

static bool WriteBytesToFile(const char *path, const std::vector<uint8_t> &bytes) {
    if (!path) return false;
    FILE *f = fopen(path, "wb");
    if (!f) return false;
    size_t written = fwrite(bytes.data(), 1, bytes.size(), f);
    fflush(f);
    fclose(f);
    return written == bytes.size();
}


/**
 * Generate Google Heic Motion Photo format XMP metadata
 */
static std::string GenerateHeicMotionPhotoXMP(size_t mp4VideoLength, size_t hdrDataSize) {
    LOGD_MP("[GenerateXMP] START - mp4VideoLength=%zu, hdrDataSize=%zu", mp4VideoLength, hdrDataSize);

    std::string xmp = R"(<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="Adobe XMP Core 5.1.0-jc003">
  <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <rdf:Description rdf:about=""
        xmlns:GCamera="http://ns.google.com/photos/1.0/camera/"
        xmlns:Container="http://ns.google.com/photos/1.0/container/"
        xmlns:Item="http://ns.google.com/photos/1.0/container/item/"
        GCamera:MicroVideo="1"
        GCamera:MicroVideoVersion="1"
        GCamera:MicroVideoOffset=")" + std::to_string(mp4VideoLength) + R"("
        GCamera:MicroVideoPresentationTimestampUs="0"
        GCamera:MotionPhoto="1"
        GCamera:MotionPhotoVersion="1"
        GCamera:MotionPhotoPresentationTimestampUs="0">
      <Container:Directory>
        <rdf:Seq>
          <rdf:li rdf:parseType="Resource">
            <Container:Item
              Item:Mime="image/jpeg"
              Item:Semantic="Primary"
              Item:Length="0"
              Item:Padding="0"/>
          </rdf:li>)";

    if (hdrDataSize > 0) {
        xmp += R"(
          <rdf:li rdf:parseType="Resource">
            <Container:Item
              Item:Mime="image/jpeg"
              Item:Semantic="GainMap"
              Item:Length=")" + std::to_string(hdrDataSize) + R"("
              Item:Padding="0"/>
          </rdf:li>)";
    }

    xmp += R"(
          <rdf:li rdf:parseType="Resource">
            <Container:Item
              Item:Mime="video/mp4"
              Item:Semantic="MotionPhoto"
              Item:Padding="8"
              Item:Length=")" + std::to_string(mp4VideoLength) + R"("/>
          </rdf:li>
        </rdf:Seq>
      </Container:Directory>
    </rdf:Description>
  </rdf:RDF>
</x:xmpmeta>)";

    LOGD_MP("[GenerateXMP] XMP size=%zu bytes", xmp.size());
    LOGD_MP("[GenerateXMP] XMP fields: MotionPhoto=1, Version=1, TimestampUs=0");
    LOGD_MP("[GenerateXMP] Primary: Mime=image/heic, Semantic=Primary, Padding=8");
    if (hdrDataSize > 0) {
        LOGD_MP("[GenerateXMP] GainMap: Mime=image/jpeg+gainmap, Semantic=GainMap, Length=%zu", hdrDataSize);
    }
    LOGD_MP("[GenerateXMP] Video: Mime=video/mp4, Semantic=MotionPhoto, Length=%zu", mp4VideoLength);
    return xmp;
}

/**
 * Different Motion photo XMP characters are obtained according to the manufacturer
 * */
static std::string GetSupportedVendorsMotionPhotoXMPCharacters(const std::string &vendor, std::int32_t videoLength) {
    std::string xmp = "GCamera:MotionPhoto=\"1\"\n"
                      "GCamera:MotionPhotoVersion=\"1\"\n"
                      "GCamera:MotionPhotoPresentationTimestampUs=\"0\"\n";
    if (vendor.empty()) {
        return xmp;
    }

    std::string v = vendor;
    LOGD_MP("[GetSupportedVendorsMotionPhotoXMPCharacters] vendor=%s", v.c_str());
    std::transform(v.begin(), v.end(), v.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    if (v == "oppo" || v == "oplus" || v == "realme") {
        xmp += "OpCamera:MotionPhotoPresentationTimestampUs=\"0\"\n"
               "OpCamera:MotionPhotoOwner=\"oplus\"\n"
               "OpCamera:OLivePhotoVersion=\"2\"\n"
               "OpCamera:VideoLength=\"" + std::to_string(videoLength) + "\"\n";
        return xmp;
    }
    if (v == "vivo" || v == "huawei") {
        return "";
    }
    if (v == "xiaomi") {
        xmp += "GCamera:MicroVideo=\"1\"\n"
               "GCamera:MicroVideoVersion=\"1\"\n"
               "GCamera:MicroVideoOffset=\"" + std::to_string(videoLength) + "\"\n"
                                                                             "GCamera:MicroVideoPresentationTimestampUs=\"0\"\n";
        return xmp;
    }
    return xmp;
}

static std::string GenerateJpegMotionPhotoXMP(size_t mp4VideoLength, size_t hdrDataSize,
                                              std::string vendor, size_t primaryPadding,
                                              size_t hdrPadding, size_t videoPadding) {
    std::string nameXmp = GetSupportedVendorsMotionPhotoXMPCharacters(vendor, static_cast<std::int32_t>(mp4VideoLength));
    std::string xmp;
    xmp += "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmptk=\"Adobe XMP Core 5.1.0-jc003\">\n";
    xmp += "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n";
    xmp += "    <rdf:Description rdf:about=\"\"\n";
    xmp += "        xmlns:GCamera=\"http://ns.google.com/photos/1.0/camera/\"\n";
    xmp += "        xmlns:Container=\"http://ns.google.com/photos/1.0/container/\"\n";
    xmp += "        xmlns:Item=\"http://ns.google.com/photos/1.0/container/item/\"\n";
    xmp += "        xmlns:OpCamera=\"http://ns.oplus.com/photos/1.0/camera/\"\n";
    if (!nameXmp.empty()) {
        size_t start = 0;
        while (true) {
            size_t end = nameXmp.find('\n', start);
            std::string line = end == std::string::npos ? nameXmp.substr(start) : nameXmp.substr(start, end - start);
            if (!line.empty()) {
                xmp += "        " + line + "\n";
            }
            if (end == std::string::npos) break;
            start = end + 1;
        }
    }
    xmp += "        >\n";
    xmp += "      <Container:Directory>\n";
    xmp += "        <rdf:Seq>\n";
    xmp += "          <rdf:li rdf:parseType=\"Resource\">\n";
    xmp += "            <Container:Item\n";
    xmp += "                Item:Mime=\"image/jpeg\"\n";
    xmp += "                Item:Semantic=\"Primary\"\n";
    xmp += "                Item:Length=\"0\"\n";
    xmp += "                Item:Padding=\"" + std::to_string(primaryPadding) + "\" />\n";
    xmp += "          </rdf:li>\n";

    if (hdrDataSize > 0) {
        xmp += "          <rdf:li rdf:parseType=\"Resource\">\n";
        xmp += "            <Container:Item\n";
        xmp += "                Item:Mime=\"image/jpeg\"\n";
        xmp += "                Item:Semantic=\"GainMap\"\n";
        xmp += "                Item:Length=\"" + std::to_string(hdrDataSize) + "\"\n";
        xmp += "                Item:Padding=\"" + std::to_string(hdrPadding) + "\" />\n";
        xmp += "          </rdf:li>\n";
    }

    xmp += "          <rdf:li rdf:parseType=\"Resource\">\n";
    xmp += "            <Container:Item\n";
    xmp += "                Item:Mime=\"video/mp4\"\n";
    xmp += "                Item:Semantic=\"MotionPhoto\"\n";
    xmp += "                Item:Length=\"" + std::to_string(mp4VideoLength) + "\"\n";
    xmp += "                Item:Padding=\"" + std::to_string(videoPadding) + "\" />\n";
    xmp += "          </rdf:li>\n";
    xmp += "        </rdf:Seq>\n";
    xmp += "      </Container:Directory>\n";
    xmp += "    </rdf:Description>\n";
    xmp += "  </rdf:RDF>\n";
    xmp += "</x:xmpmeta>";
    return xmp;
}



/**
 * Requirements:
 * Convert JPEG Motion Photo to HEIC Motion Photo.
 *
 * Logic:
 * 1. Extract JPEG image bytes and VIDEO video bytes from JPEG file
 * 2. Generate HEIC file via nativeGenGoogleMotionPhotoWithHeic() method
 * 3. Write generated file to outPath. And return outPath.
 *
 * XMP-based extraction:
 * 1. Validate JPEG container and compute primary image length
 * 2. Parse XMP container items (Primary/GainMap/MotionPhoto)
 * 3. Extract Primary JPEG, optional GainMap JPEG, and MP4 video
 */
extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ConvertJpeg2Heic(JNIEnv *env, jclass clazz,
                                                           jstring inputJpegFilePath,
                                                           jstring outPath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeConvertJpegMotionPhotoToHeic: START");
    LOGI_MP("============================================================");

    const char *filePath = env->GetStringUTFChars(inputJpegFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[ExtractJPEG] Failed to get input file path");
        return nullptr;
    }
    LOGD_MP("[ExtractJPEG] Input file: %s", filePath);

    // Open file
    FILE *file = fopen(filePath, "rb");
    if (!file) {
        LOGE_MP("[ExtractJPEG] Failed to open file: %s (errno=%d)", filePath,
                errno);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return nullptr;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    LOGD_MP("[Convert] File size: %ld bytes (%.2f MB)", fileSize,
            fileSize / (1024.0 * 1024.0));

    if (fileSize < 16) {
        LOGE_MP("[Convert] File too small to be a valid Motion Photo");
        fclose(file);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: input file too small");
    }

    // Read entire file into memory
    std::vector<uint8_t> fileData(fileSize);
    size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
    fclose(file);
    if (bytesRead != static_cast<size_t>(fileSize)) {
        LOGE_MP("[Convert] Failed to read file (read %zu of %ld)", bytesRead, fileSize);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: failed to read input file");
    }

    // Verify JPEG file header (FF D8 FF)
    if (fileData[0] != 0xFF || fileData[1] != 0xD8 || fileData[2] != 0xFF) {
        LOGE_MP("[Convert] Not a valid JPEG file (header: %02X %02X %02X)", fileData[0], fileData[1], fileData[2]);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: not a valid JPEG file");
    }
    LOGD_MP("[Convert] Valid JPEG header detected (FF D8 FF)");

    long primaryLen = CalcPrimaryJpegLength(fileData);
    if (primaryLen <= 0 || primaryLen > fileSize) {
        LOGE_MP("[Convert] Invalid primary JPEG length: %ld (fileSize=%ld)", primaryLen, fileSize);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: invalid primary JPEG length");
    }

    MotionPhotoXmpInfo xmpInfo = ParseJpegMotionPhotoXmp(fileData);
    if (!xmpInfo.isMotionPhoto) {
        LOGE_MP("[Convert] No Motion Photo XMP metadata found");
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: no motion photo metadata");
    }

    long jpegStart = 0;
    long jpegLen = primaryLen;
    long hdrStart = -1;
    long hdrLen = 0;
    long videoStart = -1;
    long videoLen = 0;
    long primaryPadding = 0;
    long hdrPadding = 0;
    long videoPadding = 0;

    if (xmpInfo.version == 1) {
        if (xmpInfo.videoLength <= 0) {
            LOGE_MP("[Convert] Missing MicroVideoOffset for v1 Motion Photo");
            env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
            return env->NewStringUTF("error: missing video length");
        }
        videoLen = xmpInfo.videoLength;
        videoStart = fileSize - videoLen;
        if (videoStart < primaryLen) {
            LOGE_MP("[Convert] Video overlaps primary JPEG: videoStart=%ld primaryLen=%ld",
                    videoStart, primaryLen);
            env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
            return env->NewStringUTF("error: video overlaps primary JPEG");
        }
    } else {
        if (xmpInfo.items.empty()) {
            LOGE_MP("[Convert] Empty Container:Directory items in XMP");
            env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
            return env->NewStringUTF("error: empty container items");
        }
        long cursor = 0;
        for (const auto &item : xmpInfo.items) {
            long len = item.length;
            if (item.semantic == "Primary" && len <= 0) {
                len = primaryLen;
            }
            if (len <= 0) {
                LOGE_MP("[Convert] Invalid item length for semantic=%s", item.semantic.c_str());
                env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
                return env->NewStringUTF("error: invalid item length");
            }
            if (item.semantic == "Primary") {
                primaryPadding = item.padding;
                jpegStart = cursor;
                jpegLen = len;
            } else if (item.semantic == "GainMap") {
                hdrPadding = item.padding;
                hdrStart = cursor;
                hdrLen = len;
            } else if (item.semantic == "MotionPhoto") {
                videoPadding = item.padding;
                videoStart = cursor;
                videoLen = len;
            }
            if (item.padding > 0) {
                cursor += len + item.padding;
            } else {
                cursor += len;
            }
        }
    }

    if (jpegStart != 0) {
        LOGE_MP("[Convert] Invalid JPEG start offset: %ld (expected 0)", jpegStart);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: invalid JPEG start offset");
    }

    if (videoStart < 0 || videoLen <= 0) {
        LOGE_MP("[Convert] Invalid video offset/length: offset=%ld len=%ld", videoStart, videoLen);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: invalid video offset/length");
    }

    if (videoStart + videoLen != fileSize) {
        LOGE_MP("[Convert] Video must be at end of file: videoEnd=%ld fileSize=%ld",
                videoStart + videoLen, fileSize);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: video is not at end of file");
    }

    if (jpegStart < 0 || jpegLen < 4 || static_cast<size_t>(jpegStart + jpegLen) > fileData.size() ||
        fileData[static_cast<size_t>(jpegStart)] != 0xFF ||
        fileData[static_cast<size_t>(jpegStart + 1)] != 0xD8) {
        LOGE_MP("[Convert] Primary JPEG missing SOI");
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: invalid primary JPEG slice");
    }
    std::vector<uint8_t> primaryJpeg = SliceBytes(fileData, jpegStart, jpegLen);

    std::vector<uint8_t> hdrJpeg;
    if (hdrStart >= 0 && hdrLen > 0) {
        hdrJpeg = ExtractGainMapJpegFromMotionPhotoBytes(fileData, hdrStart, hdrLen, hdrPadding);
        if (hdrJpeg.empty()) {
            LOGE_MP("[Convert] HDR GainMap slice not valid JPEG");
            env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
            return env->NewStringUTF("error: invalid HDR gainmap JPEG slice");
        }
    } else {
        hdrLen = 0;
    }

    if (!IsLikelyMp4Slice(fileData, videoStart, videoLen)) {
        LOGE_MP("[Convert] MP4 slice not valid (missing ftyp)");
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: invalid MP4 slice");
    }
    std::vector<uint8_t> mp4Bytes = SliceBytes(fileData, videoStart, videoLen);

    // Construct Java byte arrays
    jbyteArray jpegArray = env->NewByteArray(static_cast<jsize>(primaryJpeg.size()));
    if (!jpegArray) {
        LOGE_MP("[Convert] Failed to allocate JPEG byte array");
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: failed to allocate JPEG array");
    }
    env->SetByteArrayRegion(jpegArray, 0, static_cast<jsize>(primaryJpeg.size()),
                            reinterpret_cast<const jbyte *>(primaryJpeg.data()));

    // hdr
    jbyteArray hdrArray = nullptr;
    if (!hdrJpeg.empty()) {
        hdrArray = env->NewByteArray(static_cast<jsize>(hdrJpeg.size()));
        if (!hdrArray) {
            LOGE_MP("[Convert] Failed to allocate HDR byte array");
            env->DeleteLocalRef(jpegArray);
            env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
            return env->NewStringUTF("error: failed to allocate HDR array");
        }
        env->SetByteArrayRegion(hdrArray, 0, static_cast<jsize>(hdrJpeg.size()),
                                reinterpret_cast<const jbyte *>(hdrJpeg.data()));
    }

    // mp4
    jbyteArray mp4Array = env->NewByteArray(static_cast<jsize>(mp4Bytes.size()));
    if (!mp4Array) {
        LOGE_MP("[Convert] Failed to allocate MP4 byte array");
        env->DeleteLocalRef(jpegArray);
        if (hdrArray) env->DeleteLocalRef(hdrArray);
        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
        return env->NewStringUTF("error: failed to allocate MP4 array");
    }
    env->SetByteArrayRegion(
            mp4Array, 0, static_cast<jsize>(mp4Bytes.size()),
            reinterpret_cast<const jbyte *>(mp4Bytes.data()));

    // exif data
    jbyteArray exifs = nullptr;
    std::vector<uint8_t> exifDataVec = ExtractJpegExif(filePath);
    if (!exifDataVec.empty()) {
        exifs = env->NewByteArray(static_cast<jsize>(exifDataVec.size()));
        if (exifs) {
            env->SetByteArrayRegion(exifs, 0, static_cast<jsize>(exifDataVec.size()),
                                    reinterpret_cast<const jbyte *>(exifDataVec.data()));
            LOGD_MP("[Convert] Extracted Exif data: %zu bytes", exifDataVec.size());
        }
    } else {
        LOGD_MP("[Convert] No Exif data found in JPEG");
    }

    // Call method to generate HEIC Motion Photo
    LOGI_MP("[Convert] Generating HEIC Motion Photo with nativeGenHeicMotionPhoto()");
    jstring genResult = Java_com_seafile_seadroid2_jni_HeicNative_GenHeicMotionPhoto(
            env, clazz, jpegArray, hdrArray, exifs, mp4Array, outPath);

    if (exifs) env->DeleteLocalRef(exifs);

    // Release local references of large arrays
    env->DeleteLocalRef(jpegArray);
    if (hdrArray) env->DeleteLocalRef(hdrArray);
    env->DeleteLocalRef(mp4Array);

    // Release input path
    env->ReleaseStringUTFChars(inputJpegFilePath, filePath);

    if (!genResult) {
        LOGE_MP("[Convert] HEIC generation returned NULL");
        return env->NewStringUTF("error: HEIC generation failed");
    }

    const char *genStr = env->GetStringUTFChars(genResult, nullptr);
    if (!genStr) {
        LOGW_MP("[Convert] Failed to get generation result string");
        return env->NewStringUTF("error: unknown generation result");
    }
    LOGI_MP("[Convert] Generation result: %s", genStr);

    bool success = (strncmp(genStr, "success", 7) == 0);
    env->ReleaseStringUTFChars(genResult, genStr);
    if (success) {
        LOGI_MP("[Convert] SUCCESS - returning output path");
        return outPath;
    } else {
        LOGE_MP("[Convert] FAILED - returning error string");
        return genResult;
    }
}



/**
 * Requirements:
 * Convert HEIC Motion Photo to JPEG Motion Photo.
 *
 * Logic:
 * 1、Extract data from HEIC file
 *   1.1、Extract Primary main image data from JPEG Motion Photo
 *   1.2、Extract video data via nativeExtractGoogleHeicMotionPhotoVideo() method
 *   1.3、Extract HEIC XMP data
 *   1.4、Extract HEIC EXIF data
 * 2、Add a local method to generate JPEG Motion Photo file, write data from step 1 into JPEG file.
 * 3、Use outputFilePath for generated JPEG file location. And finally return outputFilePath.
 *
 * */
extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_ConvertHeic2Jpeg(
        JNIEnv *env, jclass clazz, jstring inputFilePath, jstring vendor, jstring outputFilePath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeConvertHeicMotionPhotoToJpeg: START");
    LOGI_MP("============================================================");

    const char *inPath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!inPath) {
        LOGE_MP("[ConvertHeic2Jpeg] Failed to get input path");
        return nullptr;
    }

    const char *outPath = env->GetStringUTFChars(outputFilePath, nullptr);
    if (!outPath) {
        LOGE_MP("[ConvertHeic2Jpeg] Failed to get output path");
        env->ReleaseStringUTFChars(inputFilePath, inPath);
        return nullptr;
    }

    // extract primary image
    std::vector<uint8_t> primaryJpeg = ExtractPrimaryJpegFromHeic(inPath, 95);
    if (primaryJpeg.empty()) {
        LOGE_MP("[ConvertHeic2Jpeg] Failed to extract primary JPEG from HEIC");
        env->ReleaseStringUTFChars(outputFilePath, outPath);
        env->ReleaseStringUTFChars(inputFilePath, inPath);
        return env->NewStringUTF("error: failed to extract primary image");
    }
    if (!HasJpegSoi(primaryJpeg)) {
        LOGE_MP("[ConvertHeic2Jpeg] Primary JPEG missing SOI");
        env->ReleaseStringUTFChars(outputFilePath, outPath);
        env->ReleaseStringUTFChars(inputFilePath, inPath);
        return env->NewStringUTF("error: invalid primary JPEG data");
    }

    // extract exif data
    std::vector<uint8_t> exifTiff = ExtractHeicExifTiff(inPath);
    if (!exifTiff.empty()) {
        LOGD_MP("[ConvertHeic2Jpeg] Extracted Exif(TIFF): %zu bytes", exifTiff.size());
    } else {
        LOGD_MP("[ConvertHeic2Jpeg] No Exif extracted from HEIC");
    }

    const char *vendorChars = env->GetStringUTFChars(vendor, nullptr);
    std::string vendorStr = vendorChars ? std::string(vendorChars) : std::string();
    if (vendorChars) {
        env->ReleaseStringUTFChars(vendor, vendorChars);
    }

    // extract HDR data
    std::vector<uint8_t> hdrData = ExtractHeicHdrData(inPath);
    if (!hdrData.empty()) {
        if (!HasJpegSoi(hdrData)) {
            LOGE_MP("[ConvertHeic2Jpeg] HDR gainmap data missing JPEG SOI");
            env->ReleaseStringUTFChars(outputFilePath, outPath);
            env->ReleaseStringUTFChars(inputFilePath, inPath);
            return env->NewStringUTF("error: invalid HDR gainmap data");
        }
        LOGD_MP("[ConvertHeic2Jpeg] Extracted HDR data: %zu bytes", hdrData.size());
    } else {
        LOGD_MP("[ConvertHeic2Jpeg] No HDR data extracted from HEIC");
    }

    // extract video data
    jbyteArray mp4Arr = Java_com_seafile_seadroid2_jni_HeicNative_ExtractHeicMotionPhotoVideo(
            env, clazz, inputFilePath);
    std::vector<uint8_t> mp4Data = JByteArrayToVector(env, mp4Arr);
    if (mp4Arr) env->DeleteLocalRef(mp4Arr);

    if (mp4Data.empty()) {
        LOGE_MP("[ConvertHeic2Jpeg] MP4 video data is empty");
        env->ReleaseStringUTFChars(outputFilePath, outPath);
        env->ReleaseStringUTFChars(inputFilePath, inPath);
        return env->NewStringUTF("error: MP4 video data is empty");
    }
    if (!IsLikelyMp4Slice(mp4Data, 0, static_cast<long>(mp4Data.size()))) {
        LOGE_MP("[ConvertHeic2Jpeg] MP4 video data is not valid (missing ftyp)");
        env->ReleaseStringUTFChars(outputFilePath, outPath);
        env->ReleaseStringUTFChars(inputFilePath, inPath);
        return env->NewStringUTF("error: invalid MP4 video data");
    }

    // gen xml
    std::string xmpXml = GenerateJpegMotionPhotoXMP(mp4Data.size(), hdrData.size(), vendorStr, 0, 0, 0);

    // build jpeg motion photo
    std::vector<uint8_t> outBytes = BuildJpegMotionPhotoBytes(primaryJpeg, xmpXml, exifTiff, hdrData, mp4Data, 0, 0, 0);

    if (outBytes.empty()) {
        LOGE_MP("[ConvertHeic2Jpeg] Failed to build JPEG Motion Photo bytes");
        env->ReleaseStringUTFChars(outputFilePath, outPath);
        env->ReleaseStringUTFChars(inputFilePath, inPath);
        return env->NewStringUTF("error: failed to build jpeg motion photo");
    }

    if (!WriteBytesToFile(outPath, outBytes)) {
        LOGE_MP("[ConvertHeic2Jpeg] Failed to write output file: %s", outPath);
        env->ReleaseStringUTFChars(outputFilePath, outPath);
        env->ReleaseStringUTFChars(inputFilePath, inPath);
        return env->NewStringUTF("error: failed to write output file");
    }

    LOGD_MP("[ConvertHeic2Jpeg] Output written: %s (%.2f MB)", outPath,
            outBytes.size() / (1024.0 * 1024.0));

    env->ReleaseStringUTFChars(outputFilePath, outPath);
    env->ReleaseStringUTFChars(inputFilePath, inPath);
    return outputFilePath;
}
