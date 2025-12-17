#include "android/log.h"
#include "cstdio"
#include "cstring"
#include "jni.h"
#include "memory"
#include "string"
#include "vector"

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

/**
 * 将 RGBA 像素数据编码为单张 HEVC still image（带 alpha）
 *
 * 关键：这会创建一个完全独立的 HEVC intra image，可以独立解码
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
            dst_row[x * 4 + 3] = 255;                // A = 完全不透明
        }
    }

    // 设置 NCLX 颜色配置文件（BT.709，limited range - 与 rally_burst 一致）
    heif_color_profile_nclx *nclx = heif_nclx_color_profile_alloc();
    if (nclx) {
        nclx->color_primaries = heif_color_primaries_ITU_R_BT_709_5;
        nclx->transfer_characteristics =
                heif_transfer_characteristic_ITU_R_BT_709_5;
        nclx->matrix_coefficients = heif_matrix_coefficients_ITU_R_BT_709_5;
        nclx->full_range_flag = 0; // limited range (tv)，与 rally_burst 一致
        heif_image_set_nclx_color_profile(image, nclx);
        heif_nclx_color_profile_free(nclx);
    }

    return image;
}

/**
 * 从 JPEG 数据解码并编码为 HEIC Primary Image
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

    // 解码到 RGBA 格式
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

    // 禁用 alpha 通道保存，避免生成 alpha 辅助轨道导致图像显示为黑色
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
 * 生成静态 HEIC
 */
extern "C" JNIEXPORT jboolean

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_nativeGenStillHeicSeq(
        JNIEnv *env, jobject, jbyteArray jpegBytes, jstring outputPath) {
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
 * 获取 libheif 版本
 */
extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_nativeGetLibVersion(JNIEnv *env,
                                                                                  jobject) {
    const char *version = heif_get_version();
    return env->NewStringUTF(version);
}

/**
 * 自定义 heif_writer 用于写入内存缓冲区
 */
struct MemoryWriter {
    std::vector<uint8_t> data;
};

static heif_error memory_writer_write(heif_context *ctx, const void *data,
                                      size_t size, void *userdata) {
    MemoryWriter *writer = static_cast<MemoryWriter *>(userdata);
    const uint8_t *bytes = static_cast<const uint8_t *>(data);
    size_t oldSize = writer->data.size();
    writer->data.insert(writer->data.end(), bytes, bytes + size);
    LOGD_MP("[MemoryWriter] write: %zu bytes (buffer: %zu -> %zu)", size, oldSize,
            writer->data.size());
    heif_error err = {heif_error_Ok, heif_suberror_Unspecified, nullptr};
    return err;
}

/**
 * 生成 Google Motion Photo 格式的 XMP metadata
 *
 * 根据 Android Motion Photo 规范:
 * - GCamera:MotionPhoto = 1 表示这是动态照片
 * - GCamera:MotionPhotoVersion = 1 表示版本
 * - GCamera:MotionPhotoPresentationTimestampUs = 0 表示呈现时间戳
 * - Container:Directory 包含媒体项目信息
 * - 主图像项目需要 Padding = 8
 * - 视频项目需要 Mime、Semantic、Length
 */
static std::string GenerateMotionPhotoXMP(size_t mp4VideoLength) {
    LOGD_MP("[GenerateXMP] START - mp4VideoLength=%zu", mp4VideoLength);

    std::string xmp = R"(<?xpacket begin="" id="W5M0MpCehiHzreSzNTczkc9d"?>
<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="Adobe XMP Core 5.1.0-jc003">
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
            <Item:Mime>image/heic</Item:Mime>
            <Item:Semantic>Primary</Item:Semantic>
            <Item:Length>0</Item:Length>
            <Item:Padding>8</Item:Padding>
          </rdf:li>
          <rdf:li rdf:parseType="Resource">
            <Item:Mime>video/mp4</Item:Mime>
            <Item:Semantic>MotionPhoto</Item:Semantic>
            <Item:Length>)" + std::to_string(mp4VideoLength) + R"(</Item:Length>
          </rdf:li>
        </rdf:Seq>
      </Container:Directory>
    </rdf:Description>
  </rdf:RDF>
</x:xmpmeta>
<?xpacket end="w"?>)";

    LOGD_MP("[GenerateXMP] XMP size=%zu bytes", xmp.size());
    LOGD_MP("[GenerateXMP] XMP fields: MotionPhoto=1, Version=1, TimestampUs=0");
    LOGD_MP(
            "[GenerateXMP] Primary: Mime=image/heic, Semantic=Primary, Padding=8");
    LOGD_MP(
            "[GenerateXMP] Video: Mime=video/mp4, Semantic=MotionPhoto, Length=%zu",
            mp4VideoLength);
    return xmp;
}

/**
 * 写入 mpvd box (Motion Photo Video Data)
 *
 * mpvd box 结构:
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

    // 写入 box size (big-endian)
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

    // 写入 box type 'mpvd'
    const char *boxType = "mpvd";
    if (fwrite(boxType, 1, 4, file) != 4) {
        LOGE_MP("[WriteMpvdBox] FAILED to write box type 'mpvd'");
        return false;
    }
    LOGD_MP("[WriteMpvdBox] wrote box type 'mpvd' (4 bytes)");

    // 打印 MP4 文件开头的 magic bytes (ftyp)
    if (mp4Data.size() >= 12) {
        LOGD_MP("[WriteMpvdBox] MP4 header bytes: [%02X %02X %02X %02X] [%c%c%c%c] "
                "[%c%c%c%c]",
                mp4Data[0], mp4Data[1], mp4Data[2], mp4Data[3], mp4Data[4],
                mp4Data[5], mp4Data[6], mp4Data[7], mp4Data[8], mp4Data[9],
                mp4Data[10], mp4Data[11]);
    }

    // 写入 MP4 视频数据
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
 * 从 JPEG 数据解码并编码为 HEIC Primary Image (用于 Motion Photo)
 * 返回 image handle 用于后续添加 XMP metadata
 */
static heif_image_handle *
EncodePrimaryImageForMotionPhoto(const std::vector<uint8_t> &jpegBytes,
                                 heif_context *ctx) {
    LOGD_MP("[EncodePrimary] START - jpegBytes size=%zu, ctx=%p",
            jpegBytes.size(), ctx);

    if (jpegBytes.empty() || !ctx) {
        LOGE_MP("[EncodePrimary] Invalid input (empty=%d, ctx=%p)",
                jpegBytes.empty(), ctx);
        return nullptr;
    }

    // 打印 JPEG 文件头 (应该是 FF D8 FF)
    if (jpegBytes.size() >= 4) {
        LOGD_MP("[EncodePrimary] JPEG header: [%02X %02X %02X %02X]", jpegBytes[0],
                jpegBytes[1], jpegBytes[2], jpegBytes[3]);
    }

    jpeg_decompress_struct cinfo;
    jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);
    LOGD_MP("[EncodePrimary] JPEG decompressor created");

    jpeg_mem_src(&cinfo, const_cast<unsigned char *>(jpegBytes.data()),
                 jpegBytes.size());

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

    // 解码到 RGBA 格式
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

    heif_encoder *encoder = nullptr;
    heif_error err =
            heif_context_get_encoder_for_format(ctx, heif_compression_HEVC, &encoder);
    if (err.code != heif_error_Ok || !encoder) {
        heif_image_release(image);
        LOGE_MP("[EncodePrimary] Failed to get HEVC encoder: %s (code=%d)",
                err.message, err.code);
        return nullptr;
    }
    LOGD_MP("[EncodePrimary] HEVC encoder obtained: %p", encoder);

    heif_encoder_set_lossy_quality(encoder, 90);
    LOGD_MP("[EncodePrimary] Encoder quality set to 90");

    // 禁用 alpha 通道保存
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

/**
 * 生成 Google Motion Photo 格式的 HEIC 动态照片
 *
 * 需求：
 * 使用 Google Motion Photo 格式生成 HEIC 动态照片
 *
 * 参考：
 * https://developer.android.com/media/platform/motion-photo-format?hl=zh-cn#isobmff-image-specific-behavior
 *
 * 结构：
 * - HEIC motion photo file (container)
 *   - ftyp box
 *   - meta box
 *      - XMP 里须包含 Google Motion Photo
 * 标识字段：例如(G)Camera:MotionPhoto、(G)Camera:MotionPhotoVersion、(G)Camera:MotionPhotoPresentationTimestampUs
 *      - XMP 的 Item 里必须Mime、Semantic、Length、Padding
 *      - ISOBMFF 图片的 XMP 还必须定义主媒体项目的 Padding 属性值为 8。
 *   - mdat box: image contents
 *   - mpvd box: (MotionPhotoVideoData)
 *      - 存放 mp4 视频原始字节流。
 *      - 位置： the "mpvd" box must come after all the HEIC image file's boxes.
 *
 *  技术要求：
 *  1、libheif 提供了 XMP_metadata的操作 api，可以使用他们来操作
 *  2、primaryImageBytes、mp4VideoBytes分别是原始字节数组数据
 *  3、优先使用 libheif 自带 api。
 */
extern "C" JNIEXPORT jstring

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_nativeGenGoogleMotionPhotoWithHeic(
        JNIEnv *env, jobject, jbyteArray primaryImageBytes,
        jbyteArray mp4VideoBytes, jstring outputPath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeGenGoogleMotionPhotoWithHeic: START");
    LOGI_MP("libheif version: %s", heif_get_version());
    LOGI_MP("============================================================");

    const char *outPath = env->GetStringUTFChars(outputPath, nullptr);
    if (!outPath) {
        LOGE_MP("Failed to get output path from JNI");
        return env->NewStringUTF("error: failed to get output path");
    }
    LOGD_MP("[JNI] Output path: %s", outPath);

    // 提取 JPEG 数据
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

    // 提取 MP4 视频数据
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

    if (jpegData.empty()) {
        LOGE_MP("Primary image data is empty!");
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: primary image data is empty");
    }

    if (mp4Data.empty()) {
        LOGE_MP("MP4 video data is empty!");
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: MP4 video data is empty");
    }

    LOGI_MP("------------------------------------------------------------");
    LOGI_MP("Input Summary:");
    LOGI_MP("  JPEG: %zu bytes (%.2f KB)", jpegData.size(),
            jpegData.size() / 1024.0);
    LOGI_MP("  MP4:  %zu bytes (%.2f KB)", mp4Data.size(),
            mp4Data.size() / 1024.0);
    LOGI_MP("------------------------------------------------------------");

    // 创建 HEIF 上下文
    LOGD_MP("[Step 1/6] Creating HEIF context...");
    heif_context *ctx = heif_context_alloc();
    if (!ctx) {
        LOGE_MP("[Step 1/6] FAILED to allocate HEIF context");
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to allocate HEIF context");
    }
    LOGD_MP("[Step 1/6] HEIF context created: %p", ctx);

    // 设置 brands (使用 heic 作为 major brand)
    LOGD_MP("[Step 2/6] Setting brands...");
    heif_context_set_major_brand(ctx, heif_brand2_heic);
    heif_context_add_compatible_brand(ctx, heif_brand2_mif1);
    heif_context_add_compatible_brand(ctx, heif_brand2_miaf);
    LOGD_MP("[Step 2/6] Brands set: major=heic, compatible=[mif1, miaf]");

    // 编码 Primary Image 并获取 handle
    LOGI_MP("[Step 3/6] Encoding primary image...");
    heif_image_handle *primaryHandle =
            EncodePrimaryImageForMotionPhoto(jpegData, ctx);
    if (!primaryHandle) {
        LOGE_MP("[Step 3/6] FAILED to encode primary image");
        heif_context_free(ctx);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to encode primary image");
    }
    LOGI_MP("[Step 3/6] Primary image encoded successfully, handle=%p",
            primaryHandle);

    // 生成 XMP metadata
    LOGI_MP("[Step 4/6] Generating XMP metadata...");
    std::string xmpData = GenerateMotionPhotoXMP(mp4Data.size());
    LOGI_MP("[Step 4/6] XMP metadata generated: %zu bytes", xmpData.size());

    // 添加 XMP metadata 到 primary image
    LOGD_MP("[Step 4/6] Adding XMP to primary image handle...");
    heif_error xmpErr = heif_context_add_XMP_metadata(
            ctx, primaryHandle, xmpData.data(), static_cast<int>(xmpData.size()));
    if (xmpErr.code != heif_error_Ok) {
        LOGE_MP("[Step 4/6] FAILED to add XMP metadata: %s (code=%d, subcode=%d)",
                xmpErr.message, xmpErr.code, xmpErr.subcode);
        heif_image_handle_release(primaryHandle);
        heif_context_free(ctx);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to add XMP metadata");
    }
    LOGD_MP("[Step 4/6] XMP metadata added to context");

    heif_image_handle_release(primaryHandle);
    LOGD_MP("[Step 4/6] Primary handle released");

    // 使用自定义 writer 将 HEIC 数据写入内存
    LOGI_MP("[Step 5/6] Writing HEIC to memory buffer...");
    MemoryWriter memWriter;
    heif_writer writer;
    writer.writer_api_version = 1;
    writer.write = memory_writer_write;

    heif_error writeErr = heif_context_write(ctx, &writer, &memWriter);
    heif_context_free(ctx);
    LOGD_MP("[Step 5/6] HEIF context freed");

    if (writeErr.code != heif_error_Ok) {
        LOGE_MP("[Step 5/6] FAILED to write HEIC to memory: %s (code=%d)",
                writeErr.message, writeErr.code);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to write HEIC to memory");
    }
    LOGI_MP("[Step 5/6] HEIC written to memory: %zu bytes",
            memWriter.data.size());

    // 打印 HEIC 文件开头
    if (memWriter.data.size() >= 16) {
        LOGD_MP(
                "[Step 5/6] HEIC header: [%02X %02X %02X %02X] [%c%c%c%c] [%c%c%c%c]",
                memWriter.data[0], memWriter.data[1], memWriter.data[2],
                memWriter.data[3], memWriter.data[4], memWriter.data[5],
                memWriter.data[6], memWriter.data[7], memWriter.data[8],
                memWriter.data[9], memWriter.data[10], memWriter.data[11]);
    }

    // 写入文件：HEIC 数据 + mpvd box
    LOGI_MP("[Step 6/6] Writing to file: %s", outPath);
    FILE *outFile = fopen(outPath, "wb");
    if (!outFile) {
        LOGE_MP("[Step 6/6] FAILED to open output file: %s (errno=%d)", outPath,
                errno);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to open output file");
    }
    LOGD_MP("[Step 6/6] Output file opened");

    // 写入 HEIC 数据
    size_t heicWritten =
            fwrite(memWriter.data.data(), 1, memWriter.data.size(), outFile);
    if (heicWritten != memWriter.data.size()) {
        LOGE_MP("[Step 6/6] FAILED to write HEIC data (wrote %zu of %zu)",
                heicWritten, memWriter.data.size());
        fclose(outFile);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to write HEIC data to file");
    }
    LOGD_MP("[Step 6/6] HEIC data written: %zu bytes at offset 0", heicWritten);

    // 写入 mpvd box (MP4 视频数据)
    LOGD_MP("[Step 6/6] Writing mpvd box at offset %zu...", heicWritten);
    if (!WriteMpvdBox(outFile, mp4Data)) {
        LOGE_MP("[Step 6/6] FAILED to write mpvd box");
        fclose(outFile);
        env->ReleaseStringUTFChars(outputPath, outPath);
        return env->NewStringUTF("error: failed to write mpvd box");
    }

    fclose(outFile);
    LOGD_MP("[Step 6/6] Output file closed");

    size_t totalSize = memWriter.data.size() + 8 + mp4Data.size();

    LOGI_MP("============================================================");
    LOGI_MP("SUCCESS!");
    LOGI_MP("  Output file: %s", outPath);
    LOGI_MP("  HEIC data:   %zu bytes (offset 0)", memWriter.data.size());
    LOGI_MP("  mpvd box:    %zu bytes (offset %zu)", 8 + mp4Data.size(),
            memWriter.data.size());
    LOGI_MP("    - header:  8 bytes");
    LOGI_MP("    - MP4:     %zu bytes", mp4Data.size());
    LOGI_MP("  Total size:  %zu bytes (%.2f MB)", totalSize,
            totalSize / (1024.0 * 1024.0));
    LOGI_MP("============================================================");

    env->ReleaseStringUTFChars(outputPath, outPath);

    // 返回成功信息
    char result[512];
    snprintf(result, sizeof(result),
             "success: HEIC=%zu bytes, MP4=%zu bytes, total=%zu bytes (%.2f MB)",
             memWriter.data.size(), mp4Data.size(), totalSize,
             totalSize / (1024.0 * 1024.0));
    return env->NewStringUTF(result);
}

/**
 * Motion Photo XMP 元数据解析结果
 */
struct MotionPhotoXmpInfo {
    bool isMotionPhoto = false; // 是否为 Motion Photo
    int version = 0;            // MotionPhotoVersion
    long videoLength = 0;       // 视频数据长度 (Item:Length for MotionPhoto)
    long videoPadding = 0;      // 视频前的填充字节 (Item:Padding)
    long primaryPadding = 0;    // Primary 图片的填充字节
    std::string videoMime;      // 视频 MIME 类型
    std::string xmpContent;     // 原始 XMP 内容（用于调试）
};

/**
 * 从字符串中提取指定标签的值
 * 支持两种格式：
 * 1. 属性格式: GCamera:MotionPhoto="1"
 * 2. 标签格式: <Item:Length>12345</Item:Length>
 */
static std::string ExtractXmpValue(const std::string &xmp, const std::string &tagName) {
    // 尝试属性格式: tagName="value"
    std::string attrPattern = tagName + "=\"";
    size_t pos = xmp.find(attrPattern);
    if (pos != std::string::npos) {
        pos += attrPattern.length();
        size_t endPos = xmp.find('"', pos);
        if (endPos != std::string::npos) {
            return xmp.substr(pos, endPos - pos);
        }
    }

    // 尝试标签格式: <tagName>value</tagName>
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

/**
 * 使用 libheif 从 HEIC 文件中读取 XMP 元数据，解析 Motion Photo 信息
 *
 * @param filePath HEIC 文件路径
 * @return Motion Photo XMP 信息
 */
static MotionPhotoXmpInfo ParseHeicMotionPhotoXmpWithLibheif(const char *filePath) {
    MotionPhotoXmpInfo info;

    LOGD_MP("[ParseHeicXMP] Parsing XMP from HEIC using libheif: %s", filePath);

    // 创建 HEIF context 并读取文件
    heif_context *ctx = heif_context_alloc();
    if (!ctx) {
        LOGE_MP("[ParseHeicXMP] Failed to allocate HEIF context");
        return info;
    }

    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
    if (err.code != heif_error_Ok) {
        LOGE_MP("[ParseHeicXMP] Failed to read HEIC file: %s", err.message);
        heif_context_free(ctx);
        return info;
    }

    // 获取 primary image handle
    heif_image_handle *handle = nullptr;
    err = heif_context_get_primary_image_handle(ctx, &handle);
    if (err.code != heif_error_Ok || !handle) {
        LOGE_MP("[ParseHeicXMP] Failed to get primary image handle: %s",
                err.message);
        heif_context_free(ctx);
        return info;
    }

    // 获取所有 metadata blocks
    int numMetadata =
            heif_image_handle_get_number_of_metadata_blocks(handle, nullptr);
    LOGD_MP("[ParseHeicXMP] Found %d metadata blocks", numMetadata);

    if (numMetadata > 0) {
        std::vector<heif_item_id> metadataIds(numMetadata);
        heif_image_handle_get_list_of_metadata_block_IDs(
                handle, nullptr, metadataIds.data(), numMetadata);

        for (int i = 0; i < numMetadata; i++) {
            heif_item_id id = metadataIds[i];
            const char *type = heif_image_handle_get_metadata_type(handle, id);
            const char *contentType =
                    heif_image_handle_get_metadata_content_type(handle, id);

            LOGD_MP("[ParseHeicXMP] Metadata[%d]: id=%u, type='%s', contentType='%s'",
                    i, id, type ? type : "(null)",
                    contentType ? contentType : "(null)");

            // XMP 的 content_type 是 "application/rdf+xml"
            bool isXmp =
                    (contentType && strcmp(contentType, "application/rdf+xml") == 0) ||
                    (type && strcmp(type, "mime") == 0 && contentType &&
                     strstr(contentType, "xmp") != nullptr);

            if (isXmp) {
                size_t metadataSize = heif_image_handle_get_metadata_size(handle, id);
                LOGD_MP("[ParseHeicXMP] Found XMP metadata, size=%zu bytes",
                        metadataSize);

                if (metadataSize > 0) {
                    std::vector<uint8_t> xmpData(metadataSize);
                    err = heif_image_handle_get_metadata(handle, id, xmpData.data());
                    if (err.code == heif_error_Ok) {
                        info.xmpContent = std::string(
                                reinterpret_cast<const char *>(xmpData.data()), metadataSize);
                        LOGD_MP("[ParseHeicXMP] XMP content loaded, size=%zu",
                                info.xmpContent.size());

                        // 解析 GCamera:MotionPhoto
                        std::string motionPhoto =
                                ExtractXmpValue(info.xmpContent, "GCamera:MotionPhoto");
                        if (motionPhoto == "1") {
                            info.isMotionPhoto = true;
                            LOGD_MP("[ParseHeicXMP] GCamera:MotionPhoto = 1 (confirmed "
                                    "Motion Photo)");
                        }

                        // 解析 GCamera:MotionPhotoVersion
                        std::string version =
                                ExtractXmpValue(info.xmpContent, "GCamera:MotionPhotoVersion");
                        if (!version.empty()) {
                            info.version = std::stoi(version);
                            LOGD_MP("[ParseHeicXMP] GCamera:MotionPhotoVersion = %d",
                                    info.version);
                        }

                        // 查找 MotionPhoto Item 的 Length
                        size_t motionPhotoItemPos =
                                info.xmpContent.find("MotionPhoto</Item:Semantic>");
                        if (motionPhotoItemPos == std::string::npos) {
                            motionPhotoItemPos =
                                    info.xmpContent.find("Item:Semantic=\"MotionPhoto\"");
                        }

                        if (motionPhotoItemPos != std::string::npos) {
                            size_t searchStart =
                                    (motionPhotoItemPos > 200) ? motionPhotoItemPos - 200 : 0;
                            size_t searchEnd =
                                    std::min(motionPhotoItemPos + 500, info.xmpContent.size());
                            std::string itemContext =
                                    info.xmpContent.substr(searchStart, searchEnd - searchStart);

                            std::string lengthStr =
                                    ExtractXmpValue(itemContext, "Item:Length");
                            if (!lengthStr.empty()) {
                                info.videoLength = std::stol(lengthStr);
                                LOGD_MP("[ParseHeicXMP] MotionPhoto Item:Length = %ld",
                                        info.videoLength);
                            }

                            std::string paddingStr =
                                    ExtractXmpValue(itemContext, "Item:Padding");
                            if (!paddingStr.empty()) {
                                info.videoPadding = std::stol(paddingStr);
                                LOGD_MP("[ParseHeicXMP] MotionPhoto Item:Padding = %ld",
                                        info.videoPadding);
                            }

                            info.videoMime = ExtractXmpValue(itemContext, "Item:Mime");
                            if (!info.videoMime.empty()) {
                                LOGD_MP("[ParseHeicXMP] MotionPhoto Item:Mime = %s",
                                        info.videoMime.c_str());
                            }
                        }

                        break; // 找到 XMP 后停止
                    } else {
                        LOGE_MP("[ParseHeicXMP] Failed to read XMP data: %s", err.message);
                    }
                }
            }
        }
    }

    heif_image_handle_release(handle);
    heif_context_free(ctx);

    if (!info.isMotionPhoto) {
        LOGD_MP("[ParseHeicXMP] No Motion Photo XMP metadata found");
    }

    return info;
}

/**
 * 提取 Google HEIC Motion Photo 动态照片中的视频数据
 *
 * 需求：提取 heic 动态照片的视频数据，以供 app 播放动态效果。
 *
 * Motion Photo 文件结构：
 * - HEIC 图像数据 (ftyp + meta + mdat)
 * - mpvd box (Motion Photo Video Data)
 *   - 4 bytes: box size (big-endian)
 *   - 4 bytes: box type 'mpvd'
 *   - N bytes: MP4 video data
 *
 * 支持两种定位方式：
 *
 * 方式一：XMP 方式（优先，使用 libheif API）
 * - 使用 libheif 读取 HEIC 文件中的 XMP 元数据
 * - 查找 GCamera:MotionPhoto="1" 确认是 Motion Photo
 * - 使用 Item:Length 从文件末尾计算视频位置
 * - 需要考虑 Item:Padding（mpvd box 头部大小）
 *
 * 方式二：mpvd box 搜索方式（回退）
 * - 从文件末尾往前搜索 "mpvd" 标识
 * - 读取 box size 确定视频数据范围
 * - 提取 MP4 数据并返回字节数组
 *
 * @param inputFilePath HEIC Motion Photo 文件路径
 * @return MP4 视频数据的字节数组，失败返回 null
 */
extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractGoogleHeicMotionPhotoVideo(
        JNIEnv *env, jobject, jstring inputFilePath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeExtractGoogleHeicMotionPhotoVideo: START");
    LOGI_MP("============================================================");

    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[ExtractHEIC] Failed to get input file path");
        return nullptr;
    }
    LOGD_MP("[ExtractHEIC] Input file: %s", filePath);

    // 打开文件
    FILE *file = fopen(filePath, "rb");
    if (!file) {
        LOGE_MP("[ExtractHEIC] Failed to open file: %s (errno=%d)", filePath,
                errno);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 获取文件大小
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    LOGD_MP("[ExtractHEIC] File size: %ld bytes (%.2f MB)", fileSize,
            fileSize / (1024.0 * 1024.0));

    if (fileSize < 16) {
        LOGE_MP("[ExtractHEIC] File too small to be a valid Motion Photo");
        fclose(file);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 读取整个文件到内存
    std::vector<uint8_t> fileData(fileSize);
    size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
    fclose(file);

    if (bytesRead != static_cast<size_t>(fileSize)) {
        LOGE_MP("[ExtractHEIC] Failed to read file (read %zu of %ld)", bytesRead,
                fileSize);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    LOGD_MP("[ExtractHEIC] File loaded into memory: %zu bytes", bytesRead);

    long mp4StartPos = -1;
    uint32_t mp4Size = 0;
    const char *locateMethod = "unknown";
    MotionPhotoXmpInfo xmpInfo;

    // ==================== 方式一：使用 libheif 读取 XMP ====================
    LOGI_MP("[ExtractHEIC] Trying XMP method using libheif API...");
    xmpInfo = ParseHeicMotionPhotoXmpWithLibheif(filePath);

    if (xmpInfo.isMotionPhoto && xmpInfo.videoLength > 0) {
        LOGI_MP(
                "[ExtractHEIC] XMP method: Motion Photo confirmed, video length = %ld",
                xmpInfo.videoLength);

        // 对于 HEIC Motion Photo，视频数据在 mpvd box 中
        // Item:Length 表示视频数据长度（不含 mpvd box 头部 8 字节）
        // Item:Padding 通常是 8（mpvd box 的 size + type 共 8 字节）
        // 视频起始位置 = 文件大小 - Item:Length - Item:Padding
        long padding = xmpInfo.videoPadding > 0 ? xmpInfo.videoPadding
                                                : 8; // 默认 8 字节 mpvd 头
        mp4StartPos = fileSize - xmpInfo.videoLength;

        // 验证位置是否有效
        if (mp4StartPos > 0 && mp4StartPos < fileSize) {
            mp4Size = static_cast<uint32_t>(xmpInfo.videoLength);
            locateMethod = "XMP (libheif)";

            // 验证 ftyp
            if (mp4StartPos + 8 <= fileSize) {
                if (fileData[mp4StartPos + 4] == 'f' &&
                    fileData[mp4StartPos + 5] == 't' &&
                    fileData[mp4StartPos + 6] == 'y' &&
                    fileData[mp4StartPos + 7] == 'p') {
                    LOGD_MP("[ExtractHEIC] XMP method: Verified ftyp at calculated "
                            "position (padding=%ld)",
                            padding);
                } else {
                    LOGW_MP("[ExtractHEIC] XMP method: No ftyp at position %ld, trying "
                            "with padding adjustment",
                            mp4StartPos);
                    // 尝试使用 padding 调整
                    long adjustedPos = fileSize - xmpInfo.videoLength - padding;
                    if (adjustedPos > 0 && adjustedPos + 8 <= fileSize &&
                        fileData[adjustedPos + 4] == 'f' &&
                        fileData[adjustedPos + 5] == 't' &&
                        fileData[adjustedPos + 6] == 'y' &&
                        fileData[adjustedPos + 7] == 'p') {
                        mp4StartPos = adjustedPos;
                        // mp4Size 可能需要从 mpvd box size 重新计算
                        LOGD_MP(
                                "[ExtractHEIC] XMP method: Found ftyp at adjusted position %ld",
                                adjustedPos);
                    } else {
                        // XMP 方式失败，让回退到 mpvd 搜索
                        LOGW_MP("[ExtractHEIC] XMP method: Cannot locate video, will try "
                                "mpvd search");
                        mp4StartPos = -1;
                    }
                }
            }

            if (mp4StartPos > 0) {
                LOGD_MP("[ExtractHEIC] XMP method: MP4 starts at offset %ld, size %u",
                        mp4StartPos, mp4Size);
            }
        } else {
            LOGW_MP("[ExtractHEIC] XMP method: Invalid calculated position %ld",
                    mp4StartPos);
            mp4StartPos = -1;
        }
    } else {
        if (!xmpInfo.isMotionPhoto) {
            LOGD_MP("[ExtractHEIC] XMP method: Not a Google Motion Photo "
                    "(GCamera:MotionPhoto != 1)");
        } else {
            LOGD_MP("[ExtractHEIC] XMP method: No video length in XMP (Item:Length "
                    "not found)");
        }
    }

    // ==================== 方式二：回退到 mpvd box 搜索 ====================
    if (mp4StartPos < 0) {
        LOGI_MP("[ExtractHEIC] Falling back to mpvd box search method...");

        const uint8_t mpvdSignature[4] = {0x6D, 0x70, 0x76, 0x64}; // "mpvd"
        long mpvdPos = -1;

        // 从文件末尾往前搜索 mpvd
        for (long i = fileSize - 4; i >= 4; --i) {
            if (fileData[i] == mpvdSignature[0] &&
                fileData[i + 1] == mpvdSignature[1] &&
                fileData[i + 2] == mpvdSignature[2] &&
                fileData[i + 3] == mpvdSignature[3]) {
                mpvdPos = i;
                LOGD_MP(
                        "[ExtractHEIC] mpvd method: Found 'mpvd' signature at offset %ld",
                        mpvdPos);
                break;
            }
        }

        if (mpvdPos >= 4) {
            // mpvd box 的 size 在签名前 4 字节
            long boxSizePos = mpvdPos - 4;

            // 读取 box size (big-endian)
            uint32_t boxSize =
                    (static_cast<uint32_t>(fileData[boxSizePos]) << 24) |
                    (static_cast<uint32_t>(fileData[boxSizePos + 1]) << 16) |
                    (static_cast<uint32_t>(fileData[boxSizePos + 2]) << 8) |
                    static_cast<uint32_t>(fileData[boxSizePos + 3]);

            LOGD_MP("[ExtractHEIC] mpvd method: box size = %u bytes", boxSize);

            if (boxSize >= 8) {
                // MP4 数据大小 = box size - 8 (size + type)
                mp4Size = boxSize - 8;
                mp4StartPos = mpvdPos + 4; // 跳过 'mpvd' 签名
                locateMethod = "mpvd box search";

                LOGD_MP("[ExtractHEIC] mpvd method: MP4 starts at offset %ld, size %u",
                        mp4StartPos, mp4Size);
            } else {
                LOGE_MP("[ExtractHEIC] mpvd method: Invalid box size: %u", boxSize);
            }
        } else {
            LOGE_MP("[ExtractHEIC] mpvd method: 'mpvd' box not found");
        }
    }

    // ==================== 提取结果检查 ====================
    if (mp4StartPos < 0 || mp4Size == 0) {
        LOGE_MP("[ExtractHEIC] Failed to locate MP4 video data");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 验证范围
    if (mp4StartPos + mp4Size > static_cast<unsigned long>(fileSize)) {
        LOGE_MP("[ExtractHEIC] MP4 data exceeds file bounds (offset=%ld, size=%u, "
                "fileSize=%ld)",
                mp4StartPos, mp4Size, fileSize);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 打印 MP4 文件头信息
    if (mp4Size >= 12 && mp4StartPos + 12 <= fileSize) {
        uint32_t firstBoxSize =
                (static_cast<uint32_t>(fileData[mp4StartPos]) << 24) |
                (static_cast<uint32_t>(fileData[mp4StartPos + 1]) << 16) |
                (static_cast<uint32_t>(fileData[mp4StartPos + 2]) << 8) |
                static_cast<uint32_t>(fileData[mp4StartPos + 3]);

        LOGD_MP("[ExtractHEIC] MP4 first box: size=%u, type='%c%c%c%c', "
                "brand='%c%c%c%c'",
                firstBoxSize, fileData[mp4StartPos + 4], fileData[mp4StartPos + 5],
                fileData[mp4StartPos + 6], fileData[mp4StartPos + 7],
                fileData[mp4StartPos + 8], fileData[mp4StartPos + 9],
                fileData[mp4StartPos + 10], fileData[mp4StartPos + 11]);

        // 验证是否为 ftyp
        if (fileData[mp4StartPos + 4] == 'f' && fileData[mp4StartPos + 5] == 't' &&
            fileData[mp4StartPos + 6] == 'y' && fileData[mp4StartPos + 7] == 'p') {
            LOGD_MP("[ExtractHEIC] MP4 ftyp box detected - valid MP4 format");
        } else {
            LOGW_MP("[ExtractHEIC] Warning: MP4 data does not start with ftyp box");
        }
    }

    // 创建 Java byte array
    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Size));
    if (!result) {
        LOGE_MP("[ExtractHEIC] Failed to allocate Java byte array for %u bytes",
                mp4Size);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 复制 MP4 数据到 Java byte array
    env->SetByteArrayRegion(
            result, 0, static_cast<jsize>(mp4Size),
            reinterpret_cast<const jbyte *>(fileData.data() + mp4StartPos));

    LOGI_MP("============================================================");
    LOGI_MP("SUCCESS!");
    LOGI_MP("  Input file: %s", filePath);
    LOGI_MP("  File size: %ld bytes", fileSize);
    LOGI_MP("  Locate method: %s", locateMethod);
    if (xmpInfo.isMotionPhoto) {
        LOGI_MP(
                "  XMP info: MotionPhoto=1, version=%d, videoLength=%ld, padding=%ld",
                xmpInfo.version, xmpInfo.videoLength, xmpInfo.videoPadding);
    }
    LOGI_MP("  MP4 offset: %ld", mp4StartPos);
    LOGI_MP("  MP4 extracted: %u bytes (%.2f MB)", mp4Size,
            mp4Size / (1024.0 * 1024.0));
    LOGI_MP("============================================================");

    env->ReleaseStringUTFChars(inputFilePath, filePath);
    return result;
}

/**
 * 从 JPEG 文件数据中解析 XMP 元数据，提取 Motion Photo 信息
 *
 * XMP 在 JPEG 中的位置：
 * - 存储在 APP1 段 (0xFF 0xE1)
 * - XMP 数据以 "http://ns.adobe.com/xap/1.0/\0" 开头
 *
 * Google Motion Photo XMP 关键字段：
 * - GCamera:MotionPhoto="1" - 标识这是一个 Motion Photo
 * - GCamera:MotionPhotoVersion="1" - 版本号
 * - Item:Semantic="MotionPhoto" 的 Item:Length - 视频数据的长度
 * - Item:Padding - 可能存在的填充字节
 */
static MotionPhotoXmpInfo ParseMotionPhotoXmp(const std::vector<uint8_t> &fileData) {
    MotionPhotoXmpInfo info;

    // XMP 标识符
    const char *XMP_MARKER = "http://ns.adobe.com/xap/1.0/";
    const size_t XMP_MARKER_LEN = strlen(XMP_MARKER);

    // 搜索 APP1 段 (0xFF 0xE1)
    size_t pos = 2; // 跳过 JPEG SOI (FF D8)
    while (pos < fileData.size() - 4) {
        if (fileData[pos] != 0xFF) {
            pos++;
            continue;
        }

        uint8_t marker = fileData[pos + 1];

        // 检查是否是 APP1 段 (0xE1)
        if (marker == 0xE1) {
            // 读取段长度 (big-endian, 包含长度字段本身的 2 字节)
            uint16_t segmentLen = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
                                  static_cast<uint16_t>(fileData[pos + 3]);

            size_t segmentDataStart = pos + 4;
            size_t segmentDataLen = segmentLen - 2;

            // 检查是否是 XMP 段
            if (segmentDataLen > XMP_MARKER_LEN + 1 &&
                memcmp(fileData.data() + segmentDataStart, XMP_MARKER,
                       XMP_MARKER_LEN) == 0) {

                // XMP 内容在标识符后面（跳过结尾的 \0）
                size_t xmpStart = segmentDataStart + XMP_MARKER_LEN + 1;
                size_t xmpLen = segmentDataLen - XMP_MARKER_LEN - 1;

                if (xmpStart + xmpLen <= fileData.size()) {
                    info.xmpContent = std::string(
                            reinterpret_cast<const char *>(fileData.data() + xmpStart),
                            xmpLen);

                    LOGD_MP("[ParseXMP] Found XMP segment at offset %zu, length %zu", pos,
                            xmpLen);

                    // 解析 GCamera:MotionPhoto
                    std::string motionPhoto =
                            ExtractXmpValue(info.xmpContent, "GCamera:MotionPhoto");
                    if (motionPhoto == "1") {
                        info.isMotionPhoto = true;
                        LOGD_MP(
                                "[ParseXMP] GCamera:MotionPhoto = 1 (confirmed Motion Photo)");
                    }

                    // 解析 GCamera:MotionPhotoVersion
                    std::string version =
                            ExtractXmpValue(info.xmpContent, "GCamera:MotionPhotoVersion");
                    if (!version.empty()) {
                        info.version = std::stoi(version);
                        LOGD_MP("[ParseXMP] GCamera:MotionPhotoVersion = %d", info.version);
                    }

                    // 查找 MotionPhoto Item 的 Length
                    // 结构: <rdf:li
                    // ...><Item:Semantic>MotionPhoto</Item:Semantic><Item:Length>xxx</Item:Length>...</rdf:li>
                    size_t motionPhotoItemPos =
                            info.xmpContent.find("MotionPhoto</Item:Semantic>");
                    if (motionPhotoItemPos == std::string::npos) {
                        // 也尝试查找属性格式
                        motionPhotoItemPos =
                                info.xmpContent.find("Item:Semantic=\"MotionPhoto\"");
                    }

                    if (motionPhotoItemPos != std::string::npos) {
                        // 在 MotionPhoto Item 附近搜索 Item:Length
                        size_t searchStart =
                                (motionPhotoItemPos > 200) ? motionPhotoItemPos - 200 : 0;
                        size_t searchEnd =
                                std::min(motionPhotoItemPos + 500, info.xmpContent.size());
                        std::string itemContext =
                                info.xmpContent.substr(searchStart, searchEnd - searchStart);

                        std::string lengthStr = ExtractXmpValue(itemContext, "Item:Length");
                        if (!lengthStr.empty()) {
                            info.videoLength = std::stol(lengthStr);
                            LOGD_MP("[ParseXMP] MotionPhoto Item:Length = %ld",
                                    info.videoLength);
                        }

                        // 解析 Item:Padding（如果存在）
                        std::string paddingStr =
                                ExtractXmpValue(itemContext, "Item:Padding");
                        if (!paddingStr.empty()) {
                            info.videoPadding = std::stol(paddingStr);
                            LOGD_MP("[ParseXMP] MotionPhoto Item:Padding = %ld",
                                    info.videoPadding);
                        }

                        // 解析视频 MIME 类型
                        info.videoMime = ExtractXmpValue(itemContext, "Item:Mime");
                        if (!info.videoMime.empty()) {
                            LOGD_MP("[ParseXMP] MotionPhoto Item:Mime = %s",
                                    info.videoMime.c_str());
                        }
                    }

                    // 查找 Primary Item 的 Padding
                    size_t primaryItemPos =
                            info.xmpContent.find("Primary</Item:Semantic>");
                    if (primaryItemPos == std::string::npos) {
                        primaryItemPos = info.xmpContent.find("Item:Semantic=\"Primary\"");
                    }
                    if (primaryItemPos != std::string::npos) {
                        size_t searchStart = primaryItemPos;
                        size_t searchEnd =
                                std::min(primaryItemPos + 300, info.xmpContent.size());
                        std::string primaryContext =
                                info.xmpContent.substr(searchStart, searchEnd - searchStart);

                        std::string primaryPaddingStr =
                                ExtractXmpValue(primaryContext, "Item:Padding");
                        if (!primaryPaddingStr.empty()) {
                            info.primaryPadding = std::stol(primaryPaddingStr);
                            LOGD_MP("[ParseXMP] Primary Item:Padding = %ld",
                                    info.primaryPadding);
                        }
                    }

                    return info;
                }
            }
        }

        // 如果是 SOS (Start of Scan) 0xDA，后面是图像数据，停止搜索
        if (marker == 0xDA) {
            LOGD_MP("[ParseXMP] Reached SOS marker, stopping APP segment search");
            break;
        }

        // 跳过当前段
        if (marker >= 0xE0 && marker <= 0xEF) { // APPn 段
            if (pos + 3 < fileData.size()) {
                uint16_t len = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
                               static_cast<uint16_t>(fileData[pos + 3]);
                pos += 2 + len;
            } else {
                break;
            }
        } else if (marker == 0xDB || marker == 0xC0 || marker == 0xC2 ||
                   marker == 0xC4 || marker == 0xDD || marker == 0xFE) {
            // 其他有长度的段: DQT, SOF0, SOF2, DHT, DRI, COM
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
 * 提取 Google JPEG 格式 Motion Photo 动态照片中的视频数据
 *
 * 需求：提取 Google JPEG 结构的动态照片的视频数据，以供 app 播放动态效果。
 *
 * JPEG Motion Photo 文件结构：
 * - JPEG 图片数据 (以 FF D8 开始，以 FF D9 结束)
 * - [可选] Padding 填充字节
 * - MP4 视频数据 (以 ftyp box 开始)
 *
 * 支持两种定位方式：
 *
 * 方式一：Google XMP 方式（优先）
 * - 从 JPEG APP1 段读取 XMP 元数据
 * - 查找 GCamera:MotionPhoto="1" 确认是 Motion Photo
 * - 使用 Item:Length 从文件末尾计算视频位置
 * - 视频起始位置 = 文件大小 - Item:Length
 *
 * 方式二：华为 ftyp 搜索方式（回退）
 * - 从文件中搜索 "ftyp" 标识（MP4 文件的开头）
 * - ftyp 前 4 字节是 box size，定位 MP4 起始位置
 * - 从 MP4 起始位置到文件末尾就是完整的 MP4 数据
 *
 * @param inputFilePath JPEG Motion Photo 文件路径
 * @return MP4 视频数据的字节数组，失败返回 null
 */
extern "C" JNIEXPORT jbyteArray

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractGoogleJpegMotionPhotoVideo(
        JNIEnv *env, jobject, jstring inputFilePath) {
    LOGI_MP("============================================================");
    LOGI_MP("nativeExtractGoogleJpegMotionPhotoVideo: START");
    LOGI_MP("============================================================");

    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[ExtractJPEG] Failed to get input file path");
        return nullptr;
    }
    LOGD_MP("[ExtractJPEG] Input file: %s", filePath);

    // 打开文件
    FILE *file = fopen(filePath, "rb");
    if (!file) {
        LOGE_MP("[ExtractJPEG] Failed to open file: %s (errno=%d)", filePath,
                errno);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 获取文件大小
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    LOGD_MP("[ExtractJPEG] File size: %ld bytes (%.2f MB)", fileSize,
            fileSize / (1024.0 * 1024.0));

    if (fileSize < 16) {
        LOGE_MP("[ExtractJPEG] File too small to be a valid Motion Photo");
        fclose(file);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 读取整个文件到内存
    std::vector<uint8_t> fileData(fileSize);
    size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
    fclose(file);

    if (bytesRead != static_cast<size_t>(fileSize)) {
        LOGE_MP("[ExtractJPEG] Failed to read file (read %zu of %ld)", bytesRead,
                fileSize);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    LOGD_MP("[ExtractJPEG] File loaded into memory: %zu bytes", bytesRead);

    // 验证 JPEG 文件头 (FF D8 FF)
    if (fileData[0] != 0xFF || fileData[1] != 0xD8 || fileData[2] != 0xFF) {
        LOGE_MP("[ExtractJPEG] Not a valid JPEG file (header: %02X %02X %02X)",
                fileData[0], fileData[1], fileData[2]);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }
    LOGD_MP("[ExtractJPEG] Valid JPEG header detected (FF D8 FF)");

    long mp4StartPos = -1;
    uint32_t mp4Size = 0;
    const char *locateMethod = "unknown";

    // ==================== 方式一：尝试 XMP 方式 ====================
    LOGI_MP("[ExtractJPEG] Trying XMP method (Google style)...");
    MotionPhotoXmpInfo xmpInfo = ParseMotionPhotoXmp(fileData);

    if (xmpInfo.isMotionPhoto && xmpInfo.videoLength > 0) {
        LOGI_MP(
                "[ExtractJPEG] XMP method: Motion Photo confirmed, video length = %ld",
                xmpInfo.videoLength);

        // 计算视频起始位置
        mp4StartPos = fileSize - xmpInfo.videoLength;

        if (mp4StartPos > 0 && mp4StartPos < fileSize) {
            mp4Size = static_cast<uint32_t>(xmpInfo.videoLength);
            locateMethod = "XMP (Google)";

            // 验证视频数据是否以 ftyp 开头
            if (mp4StartPos + 8 <= fileSize) {
                // 检查偏移+4位置是否为 "ftyp"
                if (fileData[mp4StartPos + 4] == 'f' &&
                    fileData[mp4StartPos + 5] == 't' &&
                    fileData[mp4StartPos + 6] == 'y' &&
                    fileData[mp4StartPos + 7] == 'p') {
                    LOGD_MP(
                            "[ExtractJPEG] XMP method: Verified ftyp at calculated position");
                } else {
                    LOGW_MP("[ExtractJPEG] XMP method: No ftyp at calculated position, "
                            "data may be invalid");
                    LOGW_MP("[ExtractJPEG]   Expected 'ftyp', got: %02X %02X %02X %02X",
                            fileData[mp4StartPos + 4], fileData[mp4StartPos + 5],
                            fileData[mp4StartPos + 6], fileData[mp4StartPos + 7]);
                }
            }

            LOGD_MP("[ExtractJPEG] XMP method: MP4 starts at offset %ld",
                    mp4StartPos);
        } else {
            LOGW_MP("[ExtractJPEG] XMP method: Invalid calculated position %ld",
                    mp4StartPos);
            mp4StartPos = -1;
        }
    } else {
        if (!xmpInfo.isMotionPhoto) {
            LOGD_MP("[ExtractJPEG] XMP method: Not a Google Motion Photo "
                    "(GCamera:MotionPhoto != 1)");
        } else {
            LOGD_MP("[ExtractJPEG] XMP method: No video length in XMP (Item:Length "
                    "not found)");
        }
    }

    // ==================== 方式二：回退到 ftyp 搜索方式 ====================
    if (mp4StartPos < 0) {
        LOGI_MP(
                "[ExtractJPEG] Falling back to ftyp search method (Huawei style)...");

        const uint8_t ftypSignature[4] = {0x66, 0x74, 0x79, 0x70}; // "ftyp"
        long ftypPos = -1;

        // 从文件中间往后搜索（因为 JPEG 数据在前面）
        long searchStart = (fileSize > 1024) ? 1024 : 0;

        for (long i = searchStart; i < fileSize - 4; ++i) {
            if (fileData[i] == ftypSignature[0] &&
                fileData[i + 1] == ftypSignature[1] &&
                fileData[i + 2] == ftypSignature[2] &&
                fileData[i + 3] == ftypSignature[3]) {
                ftypPos = i;
                LOGD_MP(
                        "[ExtractJPEG] ftyp method: Found 'ftyp' signature at offset %ld",
                        ftypPos);
                break;
            }
        }

        if (ftypPos >= 4) {
            // ftyp 前 4 字节是 ftyp box 的 size
            mp4StartPos = ftypPos - 4;
            mp4Size = static_cast<uint32_t>(fileSize - mp4StartPos);
            locateMethod = "ftyp search (Huawei)";

            // 读取 ftyp box size 来验证
            uint32_t ftypBoxSize =
                    (static_cast<uint32_t>(fileData[mp4StartPos]) << 24) |
                    (static_cast<uint32_t>(fileData[mp4StartPos + 1]) << 16) |
                    (static_cast<uint32_t>(fileData[mp4StartPos + 2]) << 8) |
                    static_cast<uint32_t>(fileData[mp4StartPos + 3]);

            LOGD_MP("[ExtractJPEG] ftyp method: ftyp box size = %u bytes",
                    ftypBoxSize);

            // 验证 ftyp box size 是否合理
            if (ftypBoxSize < 8 || ftypBoxSize > 256) {
                LOGW_MP("[ExtractJPEG] ftyp method: Unusual ftyp box size: %u "
                        "(expected 8-256)",
                        ftypBoxSize);
            }

            LOGD_MP("[ExtractJPEG] ftyp method: MP4 starts at offset %ld",
                    mp4StartPos);
        } else {
            LOGE_MP(
                    "[ExtractJPEG] ftyp method: 'ftyp' not found or invalid position");
        }
    }

    // ==================== 提取结果检查 ====================
    if (mp4StartPos < 0 || mp4Size == 0) {
        LOGE_MP("[ExtractJPEG] Failed to locate MP4 video data");
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    LOGD_MP("[ExtractJPEG] MP4 data: offset %ld, size %u bytes, method: %s",
            mp4StartPos, mp4Size, locateMethod);

    // 打印 MP4 文件头信息
    if (mp4Size >= 12 && mp4StartPos + 12 <= fileSize) {
        uint32_t firstBoxSize =
                (static_cast<uint32_t>(fileData[mp4StartPos]) << 24) |
                (static_cast<uint32_t>(fileData[mp4StartPos + 1]) << 16) |
                (static_cast<uint32_t>(fileData[mp4StartPos + 2]) << 8) |
                static_cast<uint32_t>(fileData[mp4StartPos + 3]);

        LOGD_MP("[ExtractJPEG] MP4 first box: size=%u, type='%c%c%c%c', "
                "brand='%c%c%c%c'",
                firstBoxSize, fileData[mp4StartPos + 4], fileData[mp4StartPos + 5],
                fileData[mp4StartPos + 6], fileData[mp4StartPos + 7],
                fileData[mp4StartPos + 8], fileData[mp4StartPos + 9],
                fileData[mp4StartPos + 10], fileData[mp4StartPos + 11]);
    }

    // 计算 JPEG 图片大小
    long jpegSize = mp4StartPos;
    LOGD_MP("[ExtractJPEG] JPEG image size: %ld bytes (%.2f KB)", jpegSize,
            jpegSize / 1024.0);

    // 创建 Java byte array
    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Size));
    if (!result) {
        LOGE_MP("[ExtractJPEG] Failed to allocate Java byte array for %u bytes",
                mp4Size);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return nullptr;
    }

    // 复制 MP4 数据到 Java byte array
    env->SetByteArrayRegion(
            result, 0, static_cast<jsize>(mp4Size),
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
 * 需求：根据输入的图片路径，检查该图片是否为 Motion Photo。
 *
 * 逻辑：
 * 1、需要通过读取文件的原始字节来确定当前图片是否为 JPEG 或 HEIC。
 * 2、如果格式为 JPEG，则通过读取 XMP 与检索 "ftyp" 字符来确定是否为JPEG_MOTION_PHOTO。
 * 2.1、XMP与 ftyp 满足一个即为动态照片
 * 2.2、如果两个条件都不满足，则确定为非动态照片
 * 3、如果格式为 HEIC，则 libheif 读取 XMP 与 "ftyp" 字符来确定是否为HEIC_MOTION_PHOTO
 * 3.1、XMP与 ftyp 满足一个即为动态照片
 * 3.2、如果两个条件都不满足，则确定为非动态照片
 * 4、如果不是第 2 步和第3步都不满足，则该图片为非动态照片。
 *
 * 返回：Motion Photo Type.
 *
 * Motion Photo Type枚举：
 * JPEG_MOTION_PHOTO(0)：JPEG 格式的动态照片
 * HEIC_MOTION_PHOTO(1)：HEIC 格式的动态照片
 * NO(2)：非动态照片
 * */

// Motion Photo Type 枚举值
#define MOTION_PHOTO_TYPE_JPEG 0
#define MOTION_PHOTO_TYPE_HEIC 1
#define MOTION_PHOTO_TYPE_NONE 2

extern "C" JNIEXPORT jint

JNICALL
Java_com_seafile_seadroid2_jni_HeicNative_nativeCheckMotionPhotoType(JNIEnv *env, jobject, jstring inputFilePath) {
    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
    if (!filePath) {
        LOGE_MP("[CheckType] Failed to get input file path");
        return MOTION_PHOTO_TYPE_NONE;
    }

    // 打开文件
    FILE *file = fopen(filePath, "rb");
    if (!file) {
        LOGE_MP("[CheckType] Failed to open file: %s", filePath);
        env->ReleaseStringUTFChars(inputFilePath, filePath);
        return MOTION_PHOTO_TYPE_NONE;
    }

    // 获取文件大小
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

    // 判断文件格式
    bool isJpeg = (header[0] == 0xFF && header[1] == 0xD8 && header[2] == 0xFF);
    bool isHeic = (header[4] == 'f' && header[5] == 't' && header[6] == 'y' && header[7] == 'p');

    LOGD_MP("[CheckType] File: %s, size: %ld, isJpeg: %d, isHeic: %d", filePath, fileSize, isJpeg, isHeic);

    if (isJpeg) {
        // ==================== JPEG 格式处理 ====================
        LOGD_MP("[CheckType] Detected JPEG format");

        // 读取整个文件
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

        // 方式1: 检查 XMP 元数据
        MotionPhotoXmpInfo xmpInfo = ParseMotionPhotoXmp(fileData);
        LOGD_MP("[CheckType] JPEG XMP check: isMotionPhoto=%d", xmpInfo.isMotionPhoto);

        // 如果 XMP 确认是动态照片，直接返回
        if (xmpInfo.isMotionPhoto) {
            LOGI_MP("[CheckType] Result: JPEG_MOTION_PHOTO (XMP=1)");
            env->ReleaseStringUTFChars(inputFilePath, filePath);
            return MOTION_PHOTO_TYPE_JPEG;
        }

        // 方式2: XMP 未确认，搜索 ftyp 标识（MP4 文件头）
        const uint8_t ftypSignature[4] = {0x66, 0x74, 0x79, 0x70};  // "ftyp"
        // 从 JPEG 数据之后开始搜索（跳过前 1KB 的 JPEG 头部区域）
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
        // ==================== HEIC 格式处理 ====================
        LOGD_MP("[CheckType] Detected HEIC format");
        fclose(file);
        file = nullptr;

        // 方式1: 使用 libheif 检查 XMP 元数据
        MotionPhotoXmpInfo xmpInfo = ParseHeicMotionPhotoXmpWithLibheif(filePath);
        LOGD_MP("[CheckType] HEIC XMP check: isMotionPhoto=%d", xmpInfo.isMotionPhoto);

        // 如果 XMP 确认是动态照片，直接返回
        if (xmpInfo.isMotionPhoto) {
            LOGI_MP("[CheckType] Result: HEIC_MOTION_PHOTO (XMP=1)");
            env->ReleaseStringUTFChars(inputFilePath, filePath);
            return MOTION_PHOTO_TYPE_HEIC;
        }

        // 方式2: XMP 未确认，搜索 mpvd box 中的 ftyp 标识
        FILE *heicFile = fopen(filePath, "rb");
        if (heicFile) {
            std::vector<uint8_t> fileData(fileSize);
            fread(fileData.data(), 1, fileSize, heicFile);
            fclose(heicFile);

            // 搜索 mpvd box
            const uint8_t mpvdSignature[4] = {0x6D, 0x70, 0x76, 0x64};  // "mpvd"
            for (long i = fileSize - 4; i >= 4; --i) {
                if (fileData[i] == mpvdSignature[0] &&
                    fileData[i + 1] == mpvdSignature[1] &&
                    fileData[i + 2] == mpvdSignature[2] &&
                    fileData[i + 3] == mpvdSignature[3]) {
                    // 找到 mpvd box，检查其中是否有 ftyp
                    long mpvdDataStart = i + 4;
                    if (mpvdDataStart + 8 <= fileSize) {
                        // 检查 mpvd 数据区域是否以 ftyp 开头
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
        // 未知格式
        LOGD_MP("[CheckType] Unknown format (header: %02X %02X %02X %02X %02X %02X %02X %02X)",
                header[0], header[1], header[2], header[3],
                header[4], header[5], header[6], header[7]);
        fclose(file);
        file = nullptr;
    }

    // 清理并返回非动态照片
    if (file) {
        fclose(file);
    }
    env->ReleaseStringUTFChars(inputFilePath, filePath);

    LOGI_MP("[CheckType] Final result: NONE (not a Motion Photo)");
    return MOTION_PHOTO_TYPE_NONE;
}
