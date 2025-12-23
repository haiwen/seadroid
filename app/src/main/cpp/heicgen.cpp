//#include "android/log.h"
//#include "cstdio"
//#include "cstring"
//#include "jni.h"
//#include "memory"
//#include "string"
//#include "vector"
//#include <setjmp.h>
//
//// libjpeg for JPEG decoding
//extern "C" {
//#include "libjpeg/jpeglib.h"
//}
//
//// libheif for HEIC encoding
//#include "libheif/heif.h"
//#include "libheif/heif_brands.h"
//#include "libheif/heif_sequences.h"
//
//#define TAG "HeicSeq"
//#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG, TAG, __VA_ARGS__)
//#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR, TAG, __VA_ARGS__)
//#define LOGI(...) __android_log_print(ANDROID_LOG_INFO, TAG, __VA_ARGS__)
//
//#define TAG_MP "MotionPhotoNative"
//#define LOGD_MP(...) __android_log_print(ANDROID_LOG_DEBUG, TAG_MP, __VA_ARGS__)
//#define LOGI_MP(...) __android_log_print(ANDROID_LOG_INFO, TAG_MP, __VA_ARGS__)
//#define LOGE_MP(...) __android_log_print(ANDROID_LOG_ERROR, TAG_MP, __VA_ARGS__)
//#define LOGW_MP(...) __android_log_print(ANDROID_LOG_WARN, TAG_MP, __VA_ARGS__)
//
///**
// * 将 RGBA 像素数据编码为单张 HEVC still image（带 alpha）
// *
// * 关键：这会创建一个完全独立的 HEVC intra image，可以独立解码
// */
//static heif_image *CreateHeifImageFromRGBA(const uint8_t *rgba, int width,
//                                           int height, int stride) {
//    heif_image *image = nullptr;
//    heif_error err = heif_image_create(width, height, heif_colorspace_RGB,
//                                       heif_chroma_interleaved_RGBA, &image);
//    if (err.code != heif_error_Ok || !image) {
//        LOGE("Failed to create heif_image: %s", err.message);
//        return nullptr;
//    }
//
//    err = heif_image_add_plane(image, heif_channel_interleaved, width, height, 8);
//    if (err.code != heif_error_Ok) {
//        heif_image_release(image);
//        LOGE("Failed to add image plane: %s", err.message);
//        return nullptr;
//    }
//
//    int plane_stride = 0;
//    uint8_t *plane =
//            heif_image_get_plane(image, heif_channel_interleaved, &plane_stride);
//
//    for (int y = 0; y < height; ++y) {
//        const uint8_t *src_row = rgba + y * stride;
//        uint8_t *dst_row = plane + y * plane_stride;
//        for (int x = 0; x < width; ++x) {
//            dst_row[x * 4 + 0] = src_row[x * 4 + 0]; // R
//            dst_row[x * 4 + 1] = src_row[x * 4 + 1]; // G
//            dst_row[x * 4 + 2] = src_row[x * 4 + 2]; // B
//            dst_row[x * 4 + 3] = 255;                // A = 完全不透明
//        }
//    }
//
//    // 设置 NCLX 颜色配置文件（BT.709，limited range - 与 rally_burst 一致）
//    heif_color_profile_nclx *nclx = heif_nclx_color_profile_alloc();
//    if (nclx) {
//        nclx->color_primaries = heif_color_primaries_ITU_R_BT_709_5;
//        nclx->transfer_characteristics =
//                heif_transfer_characteristic_ITU_R_BT_709_5;
//        nclx->matrix_coefficients = heif_matrix_coefficients_ITU_R_BT_709_5;
//        nclx->full_range_flag = 0; // limited range (tv)，与 rally_burst 一致
//        heif_image_set_nclx_color_profile(image, nclx);
//        heif_nclx_color_profile_free(nclx);
//    }
//
//    return image;
//}
//
///**
// * 从 JPEG 数据解码并编码为 HEIC Primary Image
// */
//static bool EncodePrimaryImageFromJpeg(const std::vector<uint8_t> &jpegBytes,
//                                       heif_context *ctx) {
//    if (jpegBytes.empty() || !ctx) {
//        LOGE("Invalid input for primary image encoding");
//        return false;
//    }
//
//    jpeg_decompress_struct cinfo;
//    jpeg_error_mgr jerr;
//    cinfo.err = jpeg_std_error(&jerr);
//    jpeg_create_decompress(&cinfo);
//
//    jpeg_mem_src(&cinfo, const_cast<unsigned char *>(jpegBytes.data()),
//                 jpegBytes.size());
//
//    if (jpeg_read_header(&cinfo, TRUE) != JPEG_HEADER_OK) {
//        jpeg_destroy_decompress(&cinfo);
//        LOGE("Failed to read JPEG header");
//        return false;
//    }
//
//    cinfo.out_color_space = JCS_RGB;
//    if (!jpeg_start_decompress(&cinfo)) {
//        jpeg_destroy_decompress(&cinfo);
//        LOGE("Failed to start JPEG decompression");
//        return false;
//    }
//
//    int width = static_cast<int>(cinfo.output_width);
//    int height = static_cast<int>(cinfo.output_height);
//    int comps = static_cast<int>(cinfo.output_components);
//
//    LOGI("JPEG: %dx%d, components=%d", width, height, comps);
//
//    // 解码到 RGBA 格式
//    size_t src_row_stride = static_cast<size_t>(width * comps);
//    std::vector<uint8_t> row(src_row_stride);
//    size_t rgba_stride = static_cast<size_t>(width) * 4;
//    std::vector<uint8_t> rgba(static_cast<size_t>(height) * rgba_stride);
//
//    while (cinfo.output_scanline < cinfo.output_height) {
//        JSAMPROW rowptr = row.data();
//        jpeg_read_scanlines(&cinfo, &rowptr, 1);
//        size_t y = static_cast<size_t>(cinfo.output_scanline - 1);
//        uint8_t *dst = rgba.data() + y * rgba_stride;
//
//        if (comps == 3) {
//            for (int x = 0; x < width; ++x) {
//                dst[4 * x + 0] = row[3 * x + 0]; // R
//                dst[4 * x + 1] = row[3 * x + 1]; // G
//                dst[4 * x + 2] = row[3 * x + 2]; // B
//                dst[4 * x + 3] = 255;            // A
//            }
//        } else if (comps == 1) {
//            for (int x = 0; x < width; ++x) {
//                uint8_t g = row[x];
//                dst[4 * x + 0] = g;
//                dst[4 * x + 1] = g;
//                dst[4 * x + 2] = g;
//                dst[4 * x + 3] = 255;
//            }
//        } else {
//            jpeg_finish_decompress(&cinfo);
//            jpeg_destroy_decompress(&cinfo);
//            LOGE("Unsupported JPEG format: %d components", comps);
//            return false;
//        }
//    }
//
//    jpeg_finish_decompress(&cinfo);
//    jpeg_destroy_decompress(&cinfo);
//
//    heif_image *image = CreateHeifImageFromRGBA(rgba.data(), width, height,
//                                                static_cast<int>(rgba_stride));
//    if (!image) {
//        return false;
//    }
//
//    heif_encoder *encoder = nullptr;
//    heif_error err =
//            heif_context_get_encoder_for_format(ctx, heif_compression_HEVC, &encoder);
//    if (err.code != heif_error_Ok || !encoder) {
//        heif_image_release(image);
//        LOGE("Failed to get HEVC encoder: %s", err.message);
//        return false;
//    }
//
//    heif_encoder_set_lossy_quality(encoder, 90);
//
//    // 禁用 alpha 通道保存，避免生成 alpha 辅助轨道导致图像显示为黑色
//    heif_encoding_options *enc_options = heif_encoding_options_alloc();
//    if (enc_options) {
//        enc_options->save_alpha_channel = 0;
//    }
//
//    heif_image_handle *handle = nullptr;
//    err = heif_context_encode_image(ctx, image, encoder, enc_options, &handle);
//
//    if (enc_options) {
//        heif_encoding_options_free(enc_options);
//    }
//
//    if (err.code == heif_error_Ok && handle) {
//        heif_context_set_primary_image(ctx, handle);
//    }
//    if (handle)
//        heif_image_handle_release(handle);
//    heif_encoder_release(encoder);
//    heif_image_release(image);
//
//    if (err.code != heif_error_Ok) {
//        LOGE("Failed to encode primary image: %s", err.message);
//        return false;
//    }
//
//    LOGI("Primary image encoded successfully (alpha disabled)");
//    return true;
//}
//
///**
// * 生成静态 HEIC
// */
//extern "C" JNIEXPORT jboolean JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeGenStillHeicSeq(
//        JNIEnv *env, jclass clazz, jbyteArray jpegBytes, jstring outputPath) {
//    const char *outPath = env->GetStringUTFChars(outputPath, nullptr);
//    if (!outPath) {
//        return JNI_FALSE;
//    }
//
//    jsize imgLen = env->GetArrayLength(jpegBytes);
//    if (imgLen <= 0) {
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return JNI_FALSE;
//    }
//
//    std::vector<uint8_t> jpegData(imgLen);
//    jbyte *imgData = env->GetByteArrayElements(jpegBytes, nullptr);
//    if (!imgData) {
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return JNI_FALSE;
//    }
//    memcpy(jpegData.data(), imgData, imgLen);
//    env->ReleaseByteArrayElements(jpegBytes, imgData, JNI_ABORT);
//
//    heif_context *ctx = heif_context_alloc();
//    if (!ctx) {
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return JNI_FALSE;
//    }
//
//    bool success = EncodePrimaryImageFromJpeg(jpegData, ctx);
//    if (success) {
//        heif_error werr = heif_context_write_to_file(ctx, outPath);
//        success = (werr.code == heif_error_Ok);
//        if (!success) {
//            LOGE("Failed to write HEIC: %s", werr.message);
//        }
//    }
//
//    heif_context_free(ctx);
//    env->ReleaseStringUTFChars(outputPath, outPath);
//
//    return success ? JNI_TRUE : JNI_FALSE;
//}
//
///**
// * 获取 libheif 版本
// */
//extern "C" JNIEXPORT jstring JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeGetLibVersion(JNIEnv *env,
//                                                              jclass clazz) {
//    const char *version = heif_get_version();
//    return env->NewStringUTF(version);
//}
//
///**
// * 自定义 heif_writer 用于写入内存缓冲区
// */
//struct MemoryWriter {
//    std::vector<uint8_t> data;
//};
//
//static heif_error memory_writer_write(heif_context *ctx, const void *data, size_t size, void *userdata) {
//    MemoryWriter *writer = static_cast<MemoryWriter *>(userdata);
//    const uint8_t *bytes = static_cast<const uint8_t *>(data);
//    size_t oldSize = writer->data.size();
//    writer->data.insert(writer->data.end(), bytes, bytes + size);
//    LOGD_MP("[MemoryWriter] write: %zu bytes (buffer: %zu -> %zu)", size, oldSize,
//            writer->data.size());
//    heif_error err = {heif_error_Ok, heif_suberror_Unspecified, nullptr};
//    return err;
//}
//
///**
// * 写入 mpvd box (Motion Photo Video Data)
// *
// * mpvd box 结构:
// * - 4 bytes: box size (big-endian)
// * - 4 bytes: box type 'mpvd'
// * - N bytes: MP4 video data
// */
//static bool WriteMpvdBox(FILE *file, const std::vector<uint8_t> &mp4Data) {
//    LOGD_MP("[WriteMpvdBox] START - mp4Data size=%zu", mp4Data.size());
//
//    // mpvd box: size(4) + type(4) + data(N)
//    uint32_t boxSize = static_cast<uint32_t>(8 + mp4Data.size());
//    LOGD_MP("[WriteMpvdBox] boxSize=%u (header=8 + data=%zu)", boxSize,
//            mp4Data.size());
//
//    // 写入 box size (big-endian)
//    uint8_t sizeBytes[4] = {static_cast<uint8_t>((boxSize >> 24) & 0xFF),
//                            static_cast<uint8_t>((boxSize >> 16) & 0xFF),
//                            static_cast<uint8_t>((boxSize >> 8) & 0xFF),
//                            static_cast<uint8_t>(boxSize & 0xFF)};
//    LOGD_MP("[WriteMpvdBox] size bytes (big-endian): [%02X %02X %02X %02X]",
//            sizeBytes[0], sizeBytes[1], sizeBytes[2], sizeBytes[3]);
//
//    if (fwrite(sizeBytes, 1, 4, file) != 4) {
//        LOGE_MP("[WriteMpvdBox] FAILED to write box size");
//        return false;
//    }
//    LOGD_MP("[WriteMpvdBox] wrote box size (4 bytes)");
//
//    // 写入 box type 'mpvd'
//    const char *boxType = "mpvd";
//    if (fwrite(boxType, 1, 4, file) != 4) {
//        LOGE_MP("[WriteMpvdBox] FAILED to write box type 'mpvd'");
//        return false;
//    }
//    LOGD_MP("[WriteMpvdBox] wrote box type 'mpvd' (4 bytes)");
//
//    // 打印 MP4 文件开头的 magic bytes (ftyp)
//    if (mp4Data.size() >= 12) {
//        LOGD_MP("[WriteMpvdBox] MP4 header bytes: [%02X %02X %02X %02X] [%c%c%c%c] "
//                "[%c%c%c%c]",
//                mp4Data[0], mp4Data[1], mp4Data[2], mp4Data[3], mp4Data[4],
//                mp4Data[5], mp4Data[6], mp4Data[7], mp4Data[8], mp4Data[9],
//                mp4Data[10], mp4Data[11]);
//    }
//
//    // 写入 MP4 视频数据
//    size_t written = fwrite(mp4Data.data(), 1, mp4Data.size(), file);
//    if (written != mp4Data.size()) {
//        LOGE_MP("[WriteMpvdBox] FAILED to write MP4 data (wrote %zu of %zu)",
//                written, mp4Data.size());
//        return false;
//    }
//    LOGD_MP("[WriteMpvdBox] wrote MP4 data (%zu bytes)", mp4Data.size());
//
//    LOGI_MP("[WriteMpvdBox] SUCCESS - total mpvd box = %u bytes", boxSize);
//    return true;
//}
//
///**
// * 从 JPEG 数据解码并编码为 HEIC Primary Image (用于 Motion Photo)
// * 返回 image handle 用于后续添加 XMP metadata
// */
//static heif_image_handle *EncodePrimaryImageForMotionPhoto(
//        const std::vector<uint8_t> &jpegBytes,
//        heif_context *ctx) {
//    LOGD_MP("[EncodePrimary] START - jpegBytes size=%zu, ctx=%p",
//            jpegBytes.size(), ctx);
//
//    if (jpegBytes.empty() || !ctx) {
//        LOGE_MP("[EncodePrimary] Invalid input (empty=%d, ctx=%p)",
//                jpegBytes.empty(), ctx);
//        return nullptr;
//    }
//
//    // 打印 JPEG 文件头 (应该是 FF D8 FF)
//    if (jpegBytes.size() >= 4) {
//        LOGD_MP("[EncodePrimary] JPEG header: [%02X %02X %02X %02X]", jpegBytes[0],
//                jpegBytes[1], jpegBytes[2], jpegBytes[3]);
//    }
//
//    jpeg_decompress_struct cinfo;
//    jpeg_error_mgr jerr;
//    cinfo.err = jpeg_std_error(&jerr);
//    jpeg_create_decompress(&cinfo);
//    LOGD_MP("[EncodePrimary] JPEG decompressor created");
//
//    jpeg_mem_src(&cinfo, const_cast<unsigned char *>(jpegBytes.data()),
//                 jpegBytes.size());
//
//    if (jpeg_read_header(&cinfo, TRUE) != JPEG_HEADER_OK) {
//        jpeg_destroy_decompress(&cinfo);
//        LOGE_MP("[EncodePrimary] Failed to read JPEG header");
//        return nullptr;
//    }
//    LOGD_MP("[EncodePrimary] JPEG header read OK");
//
//    cinfo.out_color_space = JCS_RGB;
//    if (!jpeg_start_decompress(&cinfo)) {
//        jpeg_destroy_decompress(&cinfo);
//        LOGE_MP("[EncodePrimary] Failed to start JPEG decompression");
//        return nullptr;
//    }
//
//    int width = static_cast<int>(cinfo.output_width);
//    int height = static_cast<int>(cinfo.output_height);
//    int comps = static_cast<int>(cinfo.output_components);
//
//    LOGI_MP("[EncodePrimary] JPEG decoded: %dx%d, components=%d", width, height,
//            comps);
//
//    // 解码到 RGBA 格式
//    size_t src_row_stride = static_cast<size_t>(width * comps);
//    std::vector<uint8_t> row(src_row_stride);
//    size_t rgba_stride = static_cast<size_t>(width) * 4;
//    std::vector<uint8_t> rgba(static_cast<size_t>(height) * rgba_stride);
//    LOGD_MP("[EncodePrimary] Allocated RGBA buffer: %zu bytes (stride=%zu)",
//            rgba.size(), rgba_stride);
//
//    int scanlines = 0;
//    while (cinfo.output_scanline < cinfo.output_height) {
//        JSAMPROW rowptr = row.data();
//        jpeg_read_scanlines(&cinfo, &rowptr, 1);
//        size_t y = static_cast<size_t>(cinfo.output_scanline - 1);
//        uint8_t *dst = rgba.data() + y * rgba_stride;
//
//        if (comps == 3) {
//            for (int x = 0; x < width; ++x) {
//                dst[4 * x + 0] = row[3 * x + 0]; // R
//                dst[4 * x + 1] = row[3 * x + 1]; // G
//                dst[4 * x + 2] = row[3 * x + 2]; // B
//                dst[4 * x + 3] = 255;            // A
//            }
//        } else if (comps == 1) {
//            for (int x = 0; x < width; ++x) {
//                uint8_t g = row[x];
//                dst[4 * x + 0] = g;
//                dst[4 * x + 1] = g;
//                dst[4 * x + 2] = g;
//                dst[4 * x + 3] = 255;
//            }
//        } else {
//            jpeg_finish_decompress(&cinfo);
//            jpeg_destroy_decompress(&cinfo);
//            LOGE_MP("[EncodePrimary] Unsupported JPEG format: %d components", comps);
//            return nullptr;
//        }
//        scanlines++;
//    }
//    LOGD_MP("[EncodePrimary] Decoded %d scanlines to RGBA", scanlines);
//
//    jpeg_finish_decompress(&cinfo);
//    jpeg_destroy_decompress(&cinfo);
//    LOGD_MP("[EncodePrimary] JPEG decompression finished");
//
//    LOGD_MP("[EncodePrimary] Creating heif_image from RGBA...");
//    heif_image *image = CreateHeifImageFromRGBA(rgba.data(), width, height,
//                                                static_cast<int>(rgba_stride));
//    if (!image) {
//        LOGE_MP("[EncodePrimary] Failed to create heif_image");
//        return nullptr;
//    }
//    LOGD_MP("[EncodePrimary] heif_image created: %p", image);
//
//    // 获取 HEVC 编码器
//    heif_encoder *encoder = nullptr;
//    heif_error err = heif_context_get_encoder_for_format(ctx, heif_compression_HEVC, &encoder);
//    if (err.code != heif_error_Ok || !encoder) {
//        heif_image_release(image);
//        LOGE_MP("[EncodePrimary] Failed to get HEVC encoder: %s (code=%d)", err.message, err.code);
//        return nullptr;
//    }
//
//    LOGD_MP("[EncodePrimary] HEVC encoder obtained: %p", encoder);
//
//    heif_encoder_set_lossy_quality(encoder, 90);
//    LOGD_MP("[EncodePrimary] Encoder quality set to 90");
//
//    // 禁用 alpha 通道保存
//    heif_encoding_options *enc_options = heif_encoding_options_alloc();
//    if (enc_options) {
//        enc_options->save_alpha_channel = 0;
//        LOGD_MP("[EncodePrimary] Alpha channel disabled");
//    }
//
//    heif_image_handle *handle = nullptr;
//    LOGD_MP("[EncodePrimary] Starting HEVC encoding...");
//    err = heif_context_encode_image(ctx, image, encoder, enc_options, &handle);
//
//    if (enc_options) {
//        heif_encoding_options_free(enc_options);
//    }
//
//    if (err.code == heif_error_Ok && handle) {
//        heif_context_set_primary_image(ctx, handle);
//        LOGD_MP("[EncodePrimary] Primary image set in context");
//    }
//
//    heif_encoder_release(encoder);
//    heif_image_release(image);
//    LOGD_MP("[EncodePrimary] Encoder and image released");
//
//    if (err.code != heif_error_Ok) {
//        LOGE_MP("[EncodePrimary] HEVC encoding failed: %s (code=%d, subcode=%d)",
//                err.message, err.code, err.subcode);
//        if (handle)
//            heif_image_handle_release(handle);
//        return nullptr;
//    }
//
//    LOGI_MP("[EncodePrimary] SUCCESS - handle=%p, image=%dx%d", handle, width,
//            height);
//    return handle;
//}
//
//// JPEG error handler to avoid exit
//struct my_error_mgr {
//    struct jpeg_error_mgr pub;
//    jmp_buf setjmp_buffer;
//};
//
//static void my_error_exit(j_common_ptr cinfo) {
//    my_error_mgr *myerr = (my_error_mgr *) cinfo->err;
//    (*cinfo->err->output_message)(cinfo);
//    longjmp(myerr->setjmp_buffer, 1);
//}
//
//static heif_image *DecodeJpegToHeifImage(const std::vector<uint8_t> &data) {
//    if (data.empty()) return nullptr;
//
//    struct jpeg_decompress_struct cinfo;
//    struct my_error_mgr jerr;
//
//    cinfo.err = jpeg_std_error(&jerr.pub);
//    jerr.pub.error_exit = my_error_exit;
//
//    if (setjmp(jerr.setjmp_buffer)) {
//        jpeg_destroy_decompress(&cinfo);
//        return nullptr;
//    }
//
//    jpeg_create_decompress(&cinfo);
//    jpeg_mem_src(&cinfo, data.data(), static_cast<unsigned long>(data.size()));
//
//    if (jpeg_read_header(&cinfo, TRUE) != JPEG_HEADER_OK) {
//        jpeg_destroy_decompress(&cinfo);
//        return nullptr;
//    }
//
//    cinfo.out_color_space = JCS_RGB;
//    jpeg_start_decompress(&cinfo);
//
//    int width = cinfo.output_width;
//    int height = cinfo.output_height;
//    int channels = cinfo.output_components;
//
//    if (channels != 3) {
//        LOGE_MP("DecodeJpegToHeifImage: Only RGB JPEG is supported (components=%d)", channels);
//        jpeg_finish_decompress(&cinfo);
//        jpeg_destroy_decompress(&cinfo);
//        return nullptr;
//    }
//
//    heif_image *image = nullptr;
//    heif_error err = heif_image_create(width, height, heif_colorspace_RGB, heif_chroma_interleaved_RGB, &image);
//    if (err.code != heif_error_Ok) {
//        LOGE_MP("DecodeJpegToHeifImage: Failed to create heif_image");
//        jpeg_finish_decompress(&cinfo);
//        jpeg_destroy_decompress(&cinfo);
//        return nullptr;
//    }
//
//    err = heif_image_add_plane(image, heif_channel_interleaved, width, height, 8);
//    if (err.code != heif_error_Ok) {
//        LOGE_MP("DecodeJpegToHeifImage: Failed to add plane");
//        heif_image_release(image);
//        jpeg_finish_decompress(&cinfo);
//        jpeg_destroy_decompress(&cinfo);
//        return nullptr;
//    }
//
//    int stride;
//    uint8_t *p = heif_image_get_plane(image, heif_channel_interleaved, &stride);
//
//    while (cinfo.output_scanline < cinfo.output_height) {
//        uint8_t *row_pointer = p + cinfo.output_scanline * stride;
//        jpeg_read_scanlines(&cinfo, &row_pointer, 1);
//    }
//
//    jpeg_finish_decompress(&cinfo);
//    jpeg_destroy_decompress(&cinfo);
//    return image;
//}
//
///**
// * 生成 Google Motion Photo 格式的 HEIC 动态照片
// *
// * 需求：
// * 使用 Google Motion Photo 格式生成 HEIC 动态照片
// *
// * 参考：
// * https://developer.android.com/media/platform/motion-photo-format?hl=zh-cn#isobmff-image-specific-behavior
// *
// * 结构：
// * - HEIC motion photo file (container)
// *   - ftyp box
// *   - meta box
// *      - XMP 里须包含 Google Motion Photo
// * 标识字段：例如(G)Camera:MotionPhoto、(G)Camera:MotionPhotoVersion、(G)Camera:MotionPhotoPresentationTimestampUs
// *      - XMP 的 Item 里必须Mime、Semantic、Length、Padding
// *      - ISOBMFF 图片的 XMP 还必须定义主媒体项目的 Padding 属性值为 8。
// *   - mdat box: image contents
// *   - mpvd box: (MotionPhotoVideoData)
// *      - 存放 mp4 视频原始字节流。
// *      - 位置： the "mpvd" box must come after all the HEIC image file's boxes.
// *
// *  技术要求：
// *  1、libheif 提供了 XMP_metadata的操作 api，可以使用他们来操作
// *  2、primaryImageBytes、mp4VideoBytes分别是原始字节数组数据
// *  3、优先使用 libheif 自带 api。
// *  4、hdr 数据
// *   - 如果 hdr 可用，则需要用 auxiliary image + auxC + iref 来实现 GainMap 绑定。libheif 已经实现了此能力。
// *  5、exif 数据
// *   - 如果 exif data 数据可用，则可以使用 heif_context_add_exif_metadata  接口实现绑定。
// */
//extern "C" JNIEXPORT jstring JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeGenHeicMotionPhoto(
//        JNIEnv *env, jclass clazz, jbyteArray primaryImageBytes,
//        jbyteArray hdrBytes,
//        jbyteArray exifBytes,
//        jbyteArray mp4VideoBytes, jstring outputPath) {
//    LOGI_MP("============================================================");
//    LOGI_MP("nativeGenGoogleMotionPhotoWithHeic: START");
//    LOGI_MP("libheif version: %s", heif_get_version());
//    LOGI_MP("============================================================");
//
//    const char *outPath = env->GetStringUTFChars(outputPath, nullptr);
//    if (!outPath) {
//        LOGE_MP("Failed to get output path from JNI");
//        return env->NewStringUTF("error: failed to get output path");
//    }
//    LOGD_MP("[JNI] Output path: %s", outPath);
//
//    // 提取 JPEG 数据
//    std::vector<uint8_t> jpegData;
//    if (primaryImageBytes) {
//        jsize len = env->GetArrayLength(primaryImageBytes);
//        LOGD_MP("[JNI] primaryImageBytes: array length = %d", len);
//        if (len > 0) {
//            jpegData.resize(len);
//            jbyte *p = env->GetByteArrayElements(primaryImageBytes, nullptr);
//            if (p) {
//                memcpy(jpegData.data(), p, len);
//                env->ReleaseByteArrayElements(primaryImageBytes, p, JNI_ABORT);
//                LOGD_MP("[JNI] primaryImageBytes: copied %d bytes to vector", len);
//            } else {
//                LOGW_MP("[JNI] primaryImageBytes: GetByteArrayElements returned NULL");
//            }
//        }
//    } else {
//        LOGD_MP("[JNI] primaryImageBytes: NULL");
//    }
//
////    // 提取 HDR 数据
////    std::vector <uint8_t> hdrData;
////    if (hdrBytes) {
////        jsize len = env->GetArrayLength(hdrBytes);
////        LOGD_MP("[JNI] hdrBytes: array length = %d", len);
////        if (len > 0) {
////            hdrData.resize(len);
////            jbyte *p = env->GetByteArrayElements(hdrBytes, nullptr);
////            if (p) {
////                hdrData.resize(len);
////                memcpy(hdrData.data(), p, len);
////                env->ReleaseByteArrayElements(hdrBytes, p, JNI_ABORT);
////                LOGD_MP("[JNI] hdrBytes: copied %d bytes to vector", len);
////            } else {
////                LOGW_MP("[JNI] hdrBytes: GetByteArrayElements returned NULL");
////            }
////        }
////    } else {
////        LOGD_MP("[JNI] hdrBytes: NULL");
////    }
//
//    // 提取 EXIF 数据
//    std::vector<uint8_t> exifData;
//    if (exifBytes) {
//        jsize len = env->GetArrayLength(exifBytes);
//        LOGD_MP("[JNI] exifBytes: array length = %d", len);
//        if (len > 0) {
//            exifData.resize(len);
//            jbyte *p = env->GetByteArrayElements(exifBytes, nullptr);
//            if (p) {
//                memcpy(exifData.data(), p, len);
//                env->ReleaseByteArrayElements(exifBytes, p, JNI_ABORT);
//                LOGD_MP("[JNI] exifBytes: copied %d bytes to vector", len);
//            } else {
//                LOGW_MP("[JNI] exifBytes: GetByteArrayElements returned NULL");
//            }
//        }
//    } else {
//        LOGD_MP("[JNI] exifBytes: NULL");
//    }
//
//    // 提取 MP4 视频数据
//    std::vector<uint8_t> mp4Data;
//    if (mp4VideoBytes) {
//        jsize len = env->GetArrayLength(mp4VideoBytes);
//        LOGD_MP("[JNI] mp4VideoBytes: array length = %d", len);
//        if (len > 0) {
//            mp4Data.resize(len);
//            jbyte *p = env->GetByteArrayElements(mp4VideoBytes, nullptr);
//            if (p) {
//                memcpy(mp4Data.data(), p, len);
//                env->ReleaseByteArrayElements(mp4VideoBytes, p, JNI_ABORT);
//                LOGD_MP("[JNI] mp4VideoBytes: copied %d bytes to vector", len);
//            } else {
//                LOGW_MP("[JNI] mp4VideoBytes: GetByteArrayElements returned NULL");
//            }
//        }
//    } else {
//        LOGD_MP("[JNI] mp4VideoBytes: NULL");
//    }
//
//    if (jpegData.empty()) {
//        LOGE_MP("Primary image data is empty!");
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: primary image data is empty");
//    }
//
//    if (mp4Data.empty()) {
//        LOGE_MP("MP4 video data is empty!");
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: MP4 video data is empty");
//    }
//
//    LOGI_MP("------------------------------------------------------------");
//    LOGI_MP("Input Summary:");
//    LOGI_MP("  JPEG: %zu bytes (%.2f KB)", jpegData.size(),
//            jpegData.size() / 1024.0);
//    LOGI_MP("  MP4:  %zu bytes (%.2f KB)", mp4Data.size(),
//            mp4Data.size() / 1024.0);
//    LOGI_MP("------------------------------------------------------------");
//
//    // 创建 HEIF 上下文
//    LOGD_MP("[Step 1/6] Creating HEIF context...");
//    heif_context *ctx = heif_context_alloc();
//    if (!ctx) {
//        LOGE_MP("[Step 1/6] FAILED to allocate HEIF context");
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: failed to allocate HEIF context");
//    }
//    LOGD_MP("[Step 1/6] HEIF context created: %p", ctx);
//
//    // 编码 Primary Image 并获取 handle
//    // 注意：不要在编码前设置 brands，否则会导致编码器获取失败
//    LOGI_MP("[Step 2/6] Encoding primary image...");
//    heif_image_handle *primaryHandle =
//            EncodePrimaryImageForMotionPhoto(jpegData, ctx);
//    if (!primaryHandle) {
//        LOGE_MP("[Step 2/6] FAILED to encode primary image");
//        heif_context_free(ctx);
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: failed to encode primary image");
//    }
//    LOGI_MP("[Step 2/5] Primary image encoded successfully, handle=%p",
//            primaryHandle);
//
//    // 生成 XMP metadata
//    LOGI_MP("[Step 3/5] Generating XMP metadata...");
//    std::string xmpData = GenerateMotionPhotoXMP(mp4Data.size());
//    LOGI_MP("[Step 3/5] XMP metadata generated: %zu bytes", xmpData.size());
//
//    // 添加 XMP metadata 到 primary image
//    LOGD_MP("[Step 3/5] Adding XMP to primary image handle...");
//    heif_error xmpErr = heif_context_add_XMP_metadata(
//            ctx, primaryHandle, xmpData.data(), static_cast<int>(xmpData.size()));
//    if (xmpErr.code != heif_error_Ok) {
//        LOGE_MP("[Step 3/5] FAILED to add XMP metadata: %s (code=%d, subcode=%d)",
//                xmpErr.message, xmpErr.code, xmpErr.subcode);
//        heif_image_handle_release(primaryHandle);
//        heif_context_free(ctx);
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: failed to add XMP metadata");
//    }
//    LOGD_MP("[Step 3/5] XMP metadata added to context");
//
//    // 添加 Exif metadata
//    if (!exifData.empty()) {
//        LOGD_MP("[Step 3/5] Adding Exif metadata (%zu bytes)...", exifData.size());
//        heif_error exifErr = heif_context_add_exif_metadata(
//                ctx, primaryHandle, exifData.data(), static_cast<int>(exifData.size()));
//        if (exifErr.code != heif_error_Ok) {
//            LOGW_MP("[Step 3/5] FAILED to add Exif metadata: %s (code=%d)",
//                    exifErr.message, exifErr.code);
//            // Exif 失败不应该阻断流程，仅记录警告
//        } else {
//            LOGD_MP("[Step 3/5] Exif metadata added successfully");
//        }
//    }
//
////    // 处理 HDR (GainMap)
////    if (!hdrData.empty()) {
////        LOGD_MP("[Step 3/5] HDR data available (%zu bytes), processing GainMap...", hdrData.size());
////
////        // 1. Decode hdrData to heif_image
////        heif_image* hdrImage = DecodeJpegToHeifImage(hdrData);
////        if (hdrImage) {
////            LOGD_MP("[Step 3/5] HDR JPEG decoded successfully (%dx%d)",
////                    heif_image_get_width(hdrImage, heif_channel_interleaved),
////                    heif_image_get_height(hdrImage, heif_channel_interleaved));
////
////            // Get encoder
////            heif_encoder* hdrEncoder = nullptr;
////            heif_error getEncErr = heif_context_get_encoder_for_format(ctx, heif_compression_HEVC, &hdrEncoder);
////            if (getEncErr.code != heif_error_Ok) {
////                 LOGE_MP("[Step 3/5] Failed to get HEVC encoder for HDR: %s", getEncErr.message);
////                 heif_image_release(hdrImage);
////            } else {
////                // 2. Encode heif_image to heif_image_handle
////                heif_image_handle* hdrHandle = nullptr;
////                heif_encoding_options* options = heif_encoding_options_alloc();
////                options->save_alpha_channel = 0;
////
////                // Set encoding quality
////                heif_encoder_set_lossy_quality(hdrEncoder, 85);
////
////                heif_error encErr = heif_context_encode_image(ctx, hdrImage, hdrEncoder, options, &hdrHandle);
////                heif_encoding_options_free(options);
////                heif_image_release(hdrImage);
////                heif_encoder_release(hdrEncoder);
////
////                if (encErr.code == heif_error_Ok && hdrHandle) {
////                     // 3. Link as auxiliary image (Workaround: use thumbnail)
////                     heif_error linkErr = heif_context_assign_thumbnail(ctx, primaryHandle, hdrHandle);
////                     if (linkErr.code != heif_error_Ok) {
////                          LOGW_MP("[Step 3/5] FAILED to assign HDR as thumbnail: %s", linkErr.message);
////                     } else {
////                          LOGD_MP("[Step 3/5] HDR data encoded and assigned as thumbnail (GainMap workaround)");
////                     }
////                     heif_image_handle_release(hdrHandle);
////                } else {
////                     LOGE_MP("[Step 3/5] FAILED to encode HDR image: %s", encErr.message);
////                }
////            }
////        } else {
////             LOGE_MP("[Step 3/5] Failed to decode HDR JPEG data");
////        }
////    }
//
//    heif_image_handle_release(primaryHandle);
//    LOGD_MP("[Step 3/5] Primary handle released");
//
//    // 使用自定义 writer 将 HEIC 数据写入内存
//    LOGI_MP("[Step 4/5] Writing HEIC to memory buffer...");
//    MemoryWriter memWriter;
//    heif_writer writer;
//    writer.writer_api_version = 1;
//    writer.write = memory_writer_write;
//
//    heif_error writeErr = heif_context_write(ctx, &writer, &memWriter);
//    heif_context_free(ctx);
//    LOGD_MP("[Step 4/5] HEIF context freed");
//
//    if (writeErr.code != heif_error_Ok) {
//        LOGE_MP("[Step 4/5] FAILED to write HEIC to memory: %s (code=%d)",
//                writeErr.message, writeErr.code);
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: failed to write HEIC to memory");
//    }
//    LOGI_MP("[Step 4/5] HEIC written to memory: %zu bytes",
//            memWriter.data.size());
//
//    // 打印 HEIC 文件开头
//    if (memWriter.data.size() >= 16) {
//        LOGD_MP(
//                "[Step 5/6] HEIC header: [%02X %02X %02X %02X] [%c%c%c%c] [%c%c%c%c]",
//                memWriter.data[0], memWriter.data[1], memWriter.data[2],
//                memWriter.data[3], memWriter.data[4], memWriter.data[5],
//                memWriter.data[6], memWriter.data[7], memWriter.data[8],
//                memWriter.data[9], memWriter.data[10], memWriter.data[11]);
//    }
//
//    // 写入文件：HEIC 数据 + mpvd box
//    LOGI_MP("[Step 5/5] Writing to file: %s", outPath);
//    FILE *outFile = fopen(outPath, "wb");
//    if (!outFile) {
//        LOGE_MP("[Step 5/5] FAILED to open output file: %s (errno=%d)", outPath,
//                errno);
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: failed to open output file");
//    }
//    LOGD_MP("[Step 5/5] Output file opened");
//
//    // 写入 HEIC 数据
//    size_t heicWritten =
//            fwrite(memWriter.data.data(), 1, memWriter.data.size(), outFile);
//    if (heicWritten != memWriter.data.size()) {
//        LOGE_MP("[Step 5/5] FAILED to write HEIC data (wrote %zu of %zu)",
//                heicWritten, memWriter.data.size());
//        fclose(outFile);
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: failed to write HEIC data to file");
//    }
//    LOGD_MP("[Step 5/5] HEIC data written: %zu bytes at offset 0", heicWritten);
//
//    // 写入 mpvd box (MP4 视频数据)
//    LOGD_MP("[Step 5/5] Writing mpvd box at offset %zu...", heicWritten);
//    if (!WriteMpvdBox(outFile, mp4Data)) {
//        LOGE_MP("[Step 5/5] FAILED to write mpvd box");
//        fclose(outFile);
//        env->ReleaseStringUTFChars(outputPath, outPath);
//        return env->NewStringUTF("error: failed to write mpvd box");
//    }
//
//    fclose(outFile);
//    LOGD_MP("[Step 5/5] Output file closed");
//
//    size_t totalSize = memWriter.data.size() + 8 + mp4Data.size();
//
//    LOGI_MP("============================================================");
//    LOGI_MP("SUCCESS!");
//    LOGI_MP("  Output file: %s", outPath);
//    LOGI_MP("  HEIC data:   %zu bytes (offset 0)", memWriter.data.size());
//    LOGI_MP("  mpvd box:    %zu bytes (offset %zu)", 8 + mp4Data.size(),
//            memWriter.data.size());
//    LOGI_MP("    - header:  8 bytes");
//    LOGI_MP("    - MP4:     %zu bytes", mp4Data.size());
//    LOGI_MP("  Total size:  %zu bytes (%.2f MB)", totalSize,
//            totalSize / (1024.0 * 1024.0));
//    LOGI_MP("============================================================");
//
//    env->ReleaseStringUTFChars(outputPath, outPath);
//
//    // 返回成功信息
//    char result[512];
//    snprintf(result, sizeof(result),
//             "success: HEIC=%zu bytes, MP4=%zu bytes, total=%zu bytes (%.2f MB)",
//             memWriter.data.size(), mp4Data.size(), totalSize,
//             totalSize / (1024.0 * 1024.0));
//    return env->NewStringUTF(result);
//}
//
///**
// * Motion Photo XMP 元数据解析结果
// */
//struct MotionPhotoXmpInfo {
//    bool isMotionPhoto = false; // 是否为 Motion Photo
//    int version = 0;            // MotionPhotoVersion
//    long videoLength = 0;       // 视频数据长度 (Item:Length for MotionPhoto)
//    long videoPadding = 0;      // 视频前的填充字节 (Item:Padding)
//    long presentationTimestampUs = -1; // 主图所在的视频哪一帧的位置
//    long primaryPadding = 0;    // Primary 图片的填充字节
//    std::string videoMime;      // 视频 MIME 类型
//    std::string xmpContent;     // 原始 XMP 内容（用于调试）
//};
//
//static long SafeStol(const std::string &str, long defaultValue = 0) {
//    try {
//        return std::stol(str);
//    } catch (...) {
//        return defaultValue;
//    }
//}
//
//static int SafeStoi(const std::string &str, int defaultValue = 0) {
//    try {
//        return std::stoi(str);
//    } catch (...) {
//        return defaultValue;
//    }
//}
//
///**
// * 从字符串中提取指定标签的值
// * 支持两种格式：
// * 1. 属性格式: GCamera:MotionPhoto="1"
// * 2. 标签格式: <Item:Length>12345</Item:Length>
// */
//static std::string ExtractXmpValue(const std::string &xmp, const std::string &tagName) {
//    // 尝试属性格式: tagName="value"
//    std::string attrPattern = tagName + "=\"";
//    size_t pos = xmp.find(attrPattern);
//    if (pos != std::string::npos) {
//        pos += attrPattern.length();
//        size_t endPos = xmp.find('"', pos);
//        if (endPos != std::string::npos) {
//            return xmp.substr(pos, endPos - pos);
//        }
//    }
//
//    // 尝试标签格式: <tagName>value</tagName>
//    std::string startTag = "<" + tagName + ">";
//    std::string endTag = "</" + tagName + ">";
//    pos = xmp.find(startTag);
//    if (pos != std::string::npos) {
//        pos += startTag.length();
//        size_t endPos = xmp.find(endTag, pos);
//        if (endPos != std::string::npos) {
//            return xmp.substr(pos, endPos - pos);
//        }
//    }
//
//    return "";
//}
//
//static MotionPhotoXmpInfo parseMotionPhotoXmpContent(const std::string &xmpContent) {
//    MotionPhotoXmpInfo info;
//    info.xmpContent = xmpContent;
//
//    // 1. 优先检查 v1 版本 (MicroVideo)
//    std::string microVideo = ExtractXmpValue(info.xmpContent, "GCamera:MicroVideo");
//    if (microVideo == "1") {
//        info.isMotionPhoto = true;
//        info.version = 1;
//        LOGD_MP("[ParseXMP] GCamera:MicroVideo = 1 (v1 Motion Photo)");
//
//        // 解析 GCamera:MicroVideoVersion
//        std::string version = ExtractXmpValue(info.xmpContent, "GCamera:MicroVideoVersion");
//        if (!version.empty()) {
//            info.version = SafeStoi(version);
//            LOGD_MP("[ParseXMP] GCamera:MicroVideoVersion = %d", info.version);
//        }
//
//        // 解析 GCamera:MicroVideoPresentationTimestampUs
//        std::string tsStr = ExtractXmpValue(info.xmpContent, "GCamera:MicroVideoPresentationTimestampUs");
//        if (!tsStr.empty()) {
//            info.presentationTimestampUs = SafeStol(tsStr);
//            LOGD_MP("[ParseXMP] GCamera:MicroVideoPresentationTimestampUs = %ld", info.presentationTimestampUs);
//        }
//
//        std::string offsetStr = ExtractXmpValue(info.xmpContent, "GCamera:MicroVideoOffset");
//        if (!offsetStr.empty()) {
//            info.videoLength = SafeStol(offsetStr);
//            LOGD_MP("[ParseXMP] GCamera:MicroVideoOffset = %ld", info.videoLength);
//        }
//        // v1 版本直接使用 Offset 作为视频长度，无需解析 Directory Item
//        return info;
//    }
//
//    // 2. 解析 GCamera:MotionPhoto (v2)
//    std::string motionPhoto = ExtractXmpValue(info.xmpContent, "GCamera:MotionPhoto");
//    if (motionPhoto == "1") {
//        info.isMotionPhoto = true;
//        LOGD_MP("[ParseXMP] GCamera:MotionPhoto = 1 (confirmed Motion Photo)");
//    }
//
//    // 解析 GCamera:MotionPhotoVersion
//    std::string version = ExtractXmpValue(info.xmpContent, "GCamera:MotionPhotoVersion");
//    if (!version.empty()) {
//        info.version = SafeStoi(version);
//        LOGD_MP("[ParseXMP] GCamera:MotionPhotoVersion = %d", info.version);
//    }
//
//    // 解析 GCamera:MotionPhotoPresentationTimestampUs
//    std::string tsStr = ExtractXmpValue(info.xmpContent, "GCamera:MotionPhotoPresentationTimestampUs");
//    if (!tsStr.empty()) {
//        info.presentationTimestampUs = SafeStol(tsStr);
//        LOGD_MP("[ParseXMP] GCamera:MotionPhotoPresentationTimestampUs = %ld", info.presentationTimestampUs);
//    }
//
//    // 查找 MotionPhoto Item 的 Length
//    size_t motionPhotoItemPos = info.xmpContent.find("MotionPhoto</Item:Semantic>");
//    if (motionPhotoItemPos == std::string::npos) {
//        motionPhotoItemPos = info.xmpContent.find("Item:Semantic=\"MotionPhoto\"");
//    }
//
//    if (motionPhotoItemPos != std::string::npos) {
//        size_t searchStart = (motionPhotoItemPos > 200) ? motionPhotoItemPos - 200 : 0;
//        size_t searchEnd = std::min(motionPhotoItemPos + 500, info.xmpContent.size());
//        std::string itemContext = info.xmpContent.substr(searchStart, searchEnd - searchStart);
//
//        std::string lengthStr = ExtractXmpValue(itemContext, "Item:Length");
//        if (!lengthStr.empty()) {
//            info.videoLength = SafeStol(lengthStr);
//            LOGD_MP("[ParseXMP] MotionPhoto Item:Length = %ld", info.videoLength);
//        }
//
//        std::string paddingStr = ExtractXmpValue(itemContext, "Item:Padding");
//        if (!paddingStr.empty()) {
//            info.videoPadding = SafeStol(paddingStr);
//            LOGD_MP("[ParseXMP] MotionPhoto Item:Padding = %ld", info.videoPadding);
//        }
//
//        info.videoMime = ExtractXmpValue(itemContext, "Item:Mime");
//        if (!info.videoMime.empty()) {
//            LOGD_MP("[ParseXMP] MotionPhoto Item:Mime = %s", info.videoMime.c_str());
//        }
//    }
//
//    // 查找 Primary Item 的 Padding
//    size_t primaryItemPos = info.xmpContent.find("Primary</Item:Semantic>");
//    if (primaryItemPos == std::string::npos) {
//        primaryItemPos = info.xmpContent.find("Item:Semantic=\"Primary\"");
//    }
//    if (primaryItemPos != std::string::npos) {
//        size_t searchStart = primaryItemPos;
//        size_t searchEnd = std::min(primaryItemPos + 300, info.xmpContent.size());
//        std::string primaryContext = info.xmpContent.substr(searchStart, searchEnd - searchStart);
//
//        std::string primaryPaddingStr = ExtractXmpValue(primaryContext, "Item:Padding");
//        if (!primaryPaddingStr.empty()) {
//            info.primaryPadding = SafeStol(primaryPaddingStr);
//            LOGD_MP("[ParseXMP] Primary Item:Padding = %ld", info.primaryPadding);
//        }
//    }
//
//    return info;
//}
//
///**
// * 使用 libheif 从 HEIC 文件中读取 XMP 元数据，解析 Motion Photo 信息
//
//  * 主要的命名空间：
// * GCAMERA = "http://ns.google.com/photos/1.0/camera/"
// * CONTAINER_NS = "http://ns.google.com/photos/1.0/container/"
// * RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
// * ITEM_NS = "http://ns.google.com/photos/1.0/container/item/"
// *
// * Google XMP 关键字段：
// * v1版本:
// * - GCamera:MicroVideo="1"
// * - GCamera:MicroVideoOffset="1"
// * - GCamera:MicroVideoVersion="1"
// * - GCamera:MicroVideoPresentationTimestampUs="1"
// *
// * v2版本:
// * - GCamera:MotionPhoto="1" - 标识这是一个 Motion Photo
// * - GCamera:MotionPhotoVersion="1" - 版本号
// * - GCamera:MotionPhotoPresentationTimestampUs="1"
// * - Item:Semantic="MotionPhoto" 的 Item:Length - 视频数据的长度
// * - Item:Padding - 可能存在的填充字节
// *
// * 参考：
// * https://developer.android.com/media/platform/motion-photo-format
// * @param filePath HEIC 文件路径
// * @return Motion Photo XMP 信息
// */
//static MotionPhotoXmpInfo ParseHeicMotionPhotoXmpWithLibheif(const char *filePath) {
//    MotionPhotoXmpInfo info;
//
//    LOGD_MP("[ParseHeicXMP] Parsing XMP from HEIC using libheif: %s", filePath);
//
//    // 创建 HEIF context 并读取文件
//    heif_context *ctx = heif_context_alloc();
//    if (!ctx) {
//        LOGE_MP("[ParseHeicXMP] Failed to allocate HEIF context");
//        return info;
//    }
//
//    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
//    if (err.code != heif_error_Ok) {
//        LOGE_MP("[ParseHeicXMP] Failed to read HEIC file: %s", err.message);
//        heif_context_free(ctx);
//        return info;
//    }
//
//    // 获取 primary image handle
//    heif_image_handle *handle = nullptr;
//    err = heif_context_get_primary_image_handle(ctx, &handle);
//    if (err.code != heif_error_Ok || !handle) {
//        LOGE_MP("[ParseHeicXMP] Failed to get primary image handle: %s", err.message);
//        heif_context_free(ctx);
//        return info;
//    }
//
//    // 获取所有 metadata blocks
//    int numMetadata = heif_image_handle_get_number_of_metadata_blocks(handle, nullptr);
//    LOGD_MP("[ParseHeicXMP] Found %d metadata blocks", numMetadata);
//
//    if (numMetadata > 0) {
//        std::vector<heif_item_id> metadataIds(numMetadata);
//        heif_image_handle_get_list_of_metadata_block_IDs(
//                handle, nullptr, metadataIds.data(), numMetadata);
//
//        for (int i = 0; i < numMetadata; i++) {
//            heif_item_id id = metadataIds[i];
//            const char *type = heif_image_handle_get_metadata_type(handle, id);
//            const char *contentType =
//                    heif_image_handle_get_metadata_content_type(handle, id);
//
//            LOGD_MP("[ParseHeicXMP] Metadata[%d]: id=%u, type='%s', contentType='%s'",
//                    i, id, type ? type : "(null)",
//                    contentType ? contentType : "(null)");
//
//            // XMP 的 content_type 是 "application/rdf+xml"
//            bool isXmp =
//                    (contentType && strcmp(contentType, "application/rdf+xml") == 0) ||
//                    (type && strcmp(type, "mime") == 0 && contentType &&
//                     strstr(contentType, "xmp") != nullptr);
//
//            if (isXmp) {
//                size_t metadataSize = heif_image_handle_get_metadata_size(handle, id);
//                LOGD_MP("[ParseHeicXMP] Found XMP metadata, size=%zu bytes",
//                        metadataSize);
//
//                if (metadataSize > 0) {
//                    std::vector<uint8_t> xmpData(metadataSize);
//                    err = heif_image_handle_get_metadata(handle, id, xmpData.data());
//                    if (err.code == heif_error_Ok) {
//                        info.xmpContent = std::string(
//                                reinterpret_cast<const char *>(xmpData.data()), metadataSize);
//                        LOGD_MP("[ParseHeicXMP] XMP content loaded, size=%zu",
//                                info.xmpContent.size());
//
//                        // 解析 XMP 内容
//                        info = parseMotionPhotoXmpContent(info.xmpContent);
//
//                        if (info.isMotionPhoto) {
//                            LOGD_MP("[ParseHeicXMP] Motion Photo detected (v%d)", info.version);
//                        }
//
//                        break; // 找到 XMP 后停止
//                    } else {
//                        LOGE_MP("[ParseHeicXMP] Failed to read XMP data: %s", err.message);
//                    }
//                }
//            }
//        }
//    }
//
//    heif_image_handle_release(handle);
//    heif_context_free(ctx);
//
//    if (!info.isMotionPhoto) {
//        LOGD_MP("[ParseHeicXMP] No Motion Photo XMP metadata found");
//    }
//
//    return info;
//}
//
///**
// * 需求：
// * 从 JPEG 文件数据中解析 XMP 元数据，提取 Motion Photo 信息
// *
// * XMP 在 JPEG 中的位置：
// * - 存储在 APP1 段 (0xFF 0xE1)
// *
// * 主要的命名空间：
// * GCAMERA = "http://ns.google.com/photos/1.0/camera/"
// * CONTAINER_NS = "http://ns.google.com/photos/1.0/container/"
// * RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
// * ITEM_NS = "http://ns.google.com/photos/1.0/container/item/"
// *
// * Google XMP 关键字段：
// * v1版本:
// * - GCamera:MicroVideo="1"
// * - GCamera:MicroVideoOffset="1"
// * - GCamera:MicroVideoVersion="1"
// * - GCamera:MicroVideoPresentationTimestampUs="1"
// *
// * v2版本:
// * - GCamera:MotionPhoto="1" - 标识这是一个 Motion Photo
// * - GCamera:MotionPhotoVersion="1" - 版本号
// * - GCamera:MotionPhotoPresentationTimestampUs="1"
// * - Item:Semantic="MotionPhoto" 的 Item:Length - 视频数据的长度
// * - Item:Padding - 可能存在的填充字节
// *
// * 参考：
// * https://developer.android.com/media/platform/motion-photo-format
// */
//static MotionPhotoXmpInfo ParseJpegMotionPhotoXmp(const std::vector<uint8_t> &fileData) {
//    MotionPhotoXmpInfo info;
//
//    // XMP 标识符
//    const char *XMP_MARKER = "http://ns.adobe.com/xap/1.0/";
//    const size_t XMP_MARKER_LEN = strlen(XMP_MARKER);
//
//    // 搜索 APP1 段 (0xFF 0xE1)
//    size_t pos = 2; // 跳过 JPEG SOI (FF D8)
//    while (pos < fileData.size() - 4) {
//        if (fileData[pos] != 0xFF) {
//            pos++;
//            continue;
//        }
//
//        uint8_t marker = fileData[pos + 1];
//
//        // 检查是否是 APP1 段 (0xE1)
//        if (marker == 0xE1) {
//            // 读取段长度 (big-endian, 包含长度字段本身的 2 字节)
//            uint16_t segmentLen = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
//                                  static_cast<uint16_t>(fileData[pos + 3]);
//
//            size_t segmentDataStart = pos + 4;
//            size_t segmentDataLen = segmentLen - 2;
//
//            // 检查是否是 XMP 段
//            if (segmentDataLen > XMP_MARKER_LEN + 1 &&
//                memcmp(fileData.data() + segmentDataStart, XMP_MARKER,
//                       XMP_MARKER_LEN) == 0) {
//
//                // XMP 内容在标识符后面（跳过结尾的 \0）
//                size_t xmpStart = segmentDataStart + XMP_MARKER_LEN + 1;
//                size_t xmpLen = segmentDataLen - XMP_MARKER_LEN - 1;
//
//                if (xmpStart + xmpLen <= fileData.size()) {
//                    info.xmpContent = std::string(
//                            reinterpret_cast<const char *>(fileData.data() + xmpStart),
//                            xmpLen);
//
//                    LOGD_MP("[ParseXMP] Found XMP segment at offset %zu, length %zu", pos,
//                            xmpLen);
//
//                    // 解析 XMP 内容
//                    info = parseMotionPhotoXmpContent(info.xmpContent);
//                    return info;
//                }
//            }
//        }
//
//        // 如果是 SOS (Start of Scan) 0xDA，后面是图像数据，停止搜索
//        if (marker == 0xDA) {
//            LOGD_MP("[ParseXMP] Reached SOS marker, stopping APP segment search");
//            break;
//        }
//
//        // 跳过当前段
//        if (marker >= 0xE0 && marker <= 0xEF) { // APPn 段
//            if (pos + 3 < fileData.size()) {
//                uint16_t len = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
//                               static_cast<uint16_t>(fileData[pos + 3]);
//                pos += 2 + len;
//            } else {
//                break;
//            }
//        } else if (marker == 0xDB || marker == 0xC0 || marker == 0xC2 ||
//                   marker == 0xC4 || marker == 0xDD || marker == 0xFE) {
//            // 其他有长度的段: DQT, SOF0, SOF2, DHT, DRI, COM
//            if (pos + 3 < fileData.size()) {
//                uint16_t len = (static_cast<uint16_t>(fileData[pos + 2]) << 8) |
//                               static_cast<uint16_t>(fileData[pos + 3]);
//                pos += 2 + len;
//            } else {
//                break;
//            }
//        } else {
//            pos++;
//        }
//    }
//
//    LOGD_MP("[ParseXMP] No XMP Motion Photo metadata found");
//    return info;
//}
//
///**
// * 需求:
// * 提取 JPEG 图片的 EXIF 数据。
// * */
//static std::vector<uint8_t> ExtractJpegExif(const char *path) {
//    std::vector<uint8_t> exifData;
//    FILE *file = fopen(path, "rb");
//    if (!file) {
//        LOGE_MP("[ExtractExif] Failed to open file: %s", path);
//        return exifData;
//    }
//
//    uint8_t header[2];
//    if (fread(header, 1, 2, file) != 2 || header[0] != 0xFF || header[1] != 0xD8) {
//        LOGE_MP("[ExtractExif] Not a JPEG file");
//        fclose(file);
//        return exifData;
//    }
//
//    while (true) {
//        int byte = fgetc(file);
//        if (byte == EOF) break;
//        if (byte != 0xFF) continue;
//
//        int type = fgetc(file);
//        if (type == EOF) break;
//        while (type == 0xFF) {
//            type = fgetc(file);
//        }
//
//        if (type == 0x00) continue; // FF 00 is not a marker (stuffed byte)
//        if (type == 0xDA || type == 0xD9) break; // SOS or EOI
//
//        // Segments without length
//        if (type == 0x01 || (type >= 0xD0 && type <= 0xD7)) continue;
//
//        // Read length
//        uint8_t lenBytes[2];
//        if (fread(lenBytes, 1, 2, file) != 2) break;
//        uint16_t length = (lenBytes[0] << 8) | lenBytes[1];
//        if (length < 2) break;
//
//        if (type == 0xE1) { // APP1
//            size_t payloadLen = length - 2;
//            std::vector<uint8_t> buffer(payloadLen);
//            if (fread(buffer.data(), 1, payloadLen, file) == payloadLen) {
//                // Check for "Exif\0\0"
//                if (payloadLen >= 6 &&
//                    buffer[0] == 'E' && buffer[1] == 'x' && buffer[2] == 'i' &&
//                    buffer[3] == 'f' && buffer[4] == 0x00 && buffer[5] == 0x00) {
//
//                    exifData.assign(buffer.begin() + 6, buffer.end());
//                    LOGD_MP("[ExtractExif] Found Exif data: %zu bytes", exifData.size());
//                    fclose(file);
//                    return exifData;
//                }
//            }
//        } else {
//            fseek(file, length - 2, SEEK_CUR);
//        }
//    }
//
//    fclose(file);
//    return exifData;
//}
//
//
///**
// * 需求：提取 Google HEIC Motion Photo 动态照片中的视频数据
// *
// *
// * Motion Photo 文件结构：
// * - HEIC 图像数据 (ftyp + meta + mdat)
// * - mpvd box (Motion Photo Video Data)
// *   - 4 bytes: box size (big-endian)
// *   - 4 bytes: box type 'mpvd'
// *   - N bytes: MP4 video data
// *
// * 支持两种定位方式：
// *
// * 方式一：mpvd box 搜索方式（优先）
// * - 从文件末尾往前搜索 "mpvd" 标识
// * - 读取 box size 确定视频数据范围
// * - 提取 MP4 数据并返回字节数组
// *
// * 方式二：XMP 方式（回退，使用 libheif API）
// * - 使用 libheif 读取 HEIC 文件中的 XMP 元数据
// * - 查找 GCamera:MotionPhoto="1" 确认是 Motion Photo
// * - 使用 Item:Length 从文件末尾计算视频位置
// * - 需要考虑 Item:Padding（mpvd box 头部大小）
// *
// * @param inputFilePath HEIC Motion Photo 文件路径
// * @return MP4 视频数据的字节数组，失败返回 null
// */
//extern "C" JNIEXPORT jbyteArray JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideoByMpvdCC(
//        JNIEnv *env, jclass clazz, jstring inputFilePath);
//
//extern "C" JNIEXPORT jbyteArray JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideoByXMP(
//        JNIEnv *env, jclass clazz, jstring inputFilePath);
//
//extern "C" JNIEXPORT jbyteArray JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideo(
//        JNIEnv *env, jclass clazz, jstring inputFilePath) {
//    LOGI_MP("============================================================");
//    LOGI_MP("nativeExtractGoogleHeicMotionPhotoVideo: START");
//    LOGI_MP("============================================================");
//    jbyteArray byMpvd = Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideoByMpvdCC(
//            env, clazz, inputFilePath);
//    bool isOk = false;
//    if (byMpvd) {
//        jsize len = env->GetArrayLength(byMpvd);
//        if (len >= 12) {
//            jbyte *p = env->GetByteArrayElements(byMpvd, nullptr);
//            if (p) {
//                isOk = (p[4] == 'f' && p[5] == 't' && p[6] == 'y' && p[7] == 'p');
//                env->ReleaseByteArrayElements(byMpvd, p, JNI_ABORT);
//            }
//        }
//        if (isOk) {
//            return byMpvd;
//        }
//        env->DeleteLocalRef(byMpvd);
//    }
//
//    return Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideoByXMP(
//            env, clazz, inputFilePath);
//}
//
//static bool ReadFileFully(const char *filePath, std::vector<uint8_t> &outData, long &outSize) {
//    outData.clear();
//    outSize = 0;
//    if (!filePath) return false;
//    FILE *file = fopen(filePath, "rb");
//    if (!file) return false;
//    fseek(file, 0, SEEK_END);
//    long fileSize = ftell(file);
//    fseek(file, 0, SEEK_SET);
//    if (fileSize < 0) {
//        fclose(file);
//        return false;
//    }
//    outData.resize(static_cast<size_t>(fileSize));
//    size_t bytesRead = fread(outData.data(), 1, static_cast<size_t>(fileSize), file);
//    fclose(file);
//    if (bytesRead != static_cast<size_t>(fileSize)) {
//        outData.clear();
//        return false;
//    }
//    outSize = fileSize;
//    return true;
//}
//
//extern "C" JNIEXPORT jbyteArray JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideoByMpvdCC(
//        JNIEnv *env, jclass clazz, jstring inputFilePath) {
//    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
//    if (!filePath) {
//        LOGE_MP("[ExtractHEIC][mpvd] Failed to get input file path");
//        return nullptr;
//    }
//    LOGD_MP("[ExtractHEIC][mpvd] Input file: %s", filePath);
//
//    std::vector<uint8_t> fileData;
//    long fileSize = 0;
//    if (!ReadFileFully(filePath, fileData, fileSize)) {
//        LOGE_MP("[ExtractHEIC][mpvd] Failed to read file");
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//    LOGD_MP("[ExtractHEIC][mpvd] File size: %ld bytes (%.2f MB)", fileSize,
//            fileSize / (1024.0 * 1024.0));
//
//    if (fileSize < 16) {
//        LOGE_MP("[ExtractHEIC][mpvd] File too small");
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    const uint8_t mpvdSignature[4] = {0x6D, 0x70, 0x76, 0x64};
//    long mpvdPos = -1;
//    for (long i = fileSize - 4; i >= 4; --i) {
//        if (fileData[i] == mpvdSignature[0] &&
//            fileData[i + 1] == mpvdSignature[1] &&
//            fileData[i + 2] == mpvdSignature[2] &&
//            fileData[i + 3] == mpvdSignature[3]) {
//            mpvdPos = i;
//            LOGD_MP("[ExtractHEIC][mpvd] Found 'mpvd' signature at offset %ld", mpvdPos);
//            break;
//        }
//    }
//
//    if (mpvdPos < 4) {
//        LOGW_MP("[ExtractHEIC][mpvd] 'mpvd' box not found");
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    long boxSizePos = mpvdPos - 4;
//    uint32_t boxSize = (static_cast<uint32_t>(fileData[boxSizePos]) << 24) |
//                       (static_cast<uint32_t>(fileData[boxSizePos + 1]) << 16) |
//                       (static_cast<uint32_t>(fileData[boxSizePos + 2]) << 8) |
//                       static_cast<uint32_t>(fileData[boxSizePos + 3]);
//    LOGD_MP("[ExtractHEIC][mpvd] box size = %u bytes", boxSize);
//
//    if (boxSize < 8) {
//        LOGE_MP("[ExtractHEIC][mpvd] Invalid box size: %u", boxSize);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    uint32_t mp4Size = boxSize - 8;
//    long mp4StartPos = mpvdPos + 4;
//    if (mp4StartPos < 0 || mp4Size == 0) {
//        LOGE_MP("[ExtractHEIC][mpvd] Invalid mp4 range");
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//    if (mp4StartPos + mp4Size > static_cast<unsigned long>(fileSize)) {
//        LOGE_MP("[ExtractHEIC][mpvd] MP4 data exceeds file bounds (offset=%ld, size=%u, fileSize=%ld)",
//                mp4StartPos, mp4Size, fileSize);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Size));
//    if (!result) {
//        LOGE_MP("[ExtractHEIC][mpvd] Failed to allocate Java byte array for %u bytes", mp4Size);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//    env->SetByteArrayRegion(result, 0, static_cast<jsize>(mp4Size),
//                            reinterpret_cast<const jbyte *>(fileData.data() + mp4StartPos));
//
//    env->ReleaseStringUTFChars(inputFilePath, filePath);
//    return result;
//}
//
//extern "C" JNIEXPORT jbyteArray JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideoByXMP(
//        JNIEnv *env, jclass clazz, jstring inputFilePath) {
//    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
//    if (!filePath) {
//        LOGE_MP("[ExtractHEIC][XMP] Failed to get input file path");
//        return nullptr;
//    }
//    LOGD_MP("[ExtractHEIC][XMP] Input file: %s", filePath);
//
//    std::vector<uint8_t> fileData;
//    long fileSize = 0;
//    if (!ReadFileFully(filePath, fileData, fileSize)) {
//        LOGE_MP("[ExtractHEIC][XMP] Failed to read file");
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//    LOGD_MP("[ExtractHEIC][XMP] File size: %ld bytes (%.2f MB)", fileSize,
//            fileSize / (1024.0 * 1024.0));
//
//    if (fileSize < 16) {
//        LOGE_MP("[ExtractHEIC][XMP] File too small");
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    MotionPhotoXmpInfo xmpInfo = ParseHeicMotionPhotoXmpWithLibheif(filePath);
//    if (!xmpInfo.isMotionPhoto || xmpInfo.videoLength <= 0) {
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    long padding = xmpInfo.videoPadding > 0 ? xmpInfo.videoPadding : 8;
//    long mp4StartPos = fileSize - xmpInfo.videoLength;
//    uint32_t mp4Size = static_cast<uint32_t>(xmpInfo.videoLength);
//
//    if (mp4StartPos <= 0 || mp4StartPos >= fileSize) {
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    if (mp4StartPos + 8 <= fileSize) {
//        if (!(fileData[mp4StartPos + 4] == 'f' &&
//              fileData[mp4StartPos + 5] == 't' &&
//              fileData[mp4StartPos + 6] == 'y' &&
//              fileData[mp4StartPos + 7] == 'p')) {
//            long adjustedPos = fileSize - xmpInfo.videoLength - padding;
//            if (adjustedPos > 0 && adjustedPos + 8 <= fileSize &&
//                fileData[adjustedPos + 4] == 'f' &&
//                fileData[adjustedPos + 5] == 't' &&
//                fileData[adjustedPos + 6] == 'y' &&
//                fileData[adjustedPos + 7] == 'p') {
//                mp4StartPos = adjustedPos;
//            } else {
//                long adjustedPos2 = fileSize - (xmpInfo.videoLength - padding);
//                if (adjustedPos2 > 0 && adjustedPos2 + 8 <= fileSize &&
//                    fileData[adjustedPos2 + 4] == 'f' &&
//                    fileData[adjustedPos2 + 5] == 't' &&
//                    fileData[adjustedPos2 + 6] == 'y' &&
//                    fileData[adjustedPos2 + 7] == 'p') {
//                    mp4StartPos = adjustedPos2;
//                    mp4Size = static_cast<uint32_t>(xmpInfo.videoLength - padding);
//                } else {
//                    env->ReleaseStringUTFChars(inputFilePath, filePath);
//                    return nullptr;
//                }
//            }
//        }
//    }
//
//    if (mp4StartPos + mp4Size > static_cast<unsigned long>(fileSize) || mp4Size == 0) {
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Size));
//    if (!result) {
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//    env->SetByteArrayRegion(result, 0, static_cast<jsize>(mp4Size),
//                            reinterpret_cast<const jbyte *>(fileData.data() + mp4StartPos));
//
//    env->ReleaseStringUTFChars(inputFilePath, filePath);
//    return result;
//}
//
//
///**
// * 提取 Google JPEG 格式 Motion Photo 动态照片中的视频数据
// *
// * 需求：提取 Google JPEG 结构的动态照片的视频数据，以供 app 播放动态效果。
// *
// * JPEG Motion Photo 文件结构：
// * - JPEG 图片数据 (以 FF D8 开始，以 FF D9 结束)
// * - [可选] Padding 填充字节
// * - MP4 视频数据 (以 ftyp box 开始)
// *
// * 支持两种定位方式：
// *
// * 方式一：Google XMP 方式（优先）
// * - 从 JPEG APP1 段读取 XMP 元数据
// * - 查找 GCamera:MotionPhoto="1" 确认是 Motion Photo
// * - 使用 Item:Length 从文件末尾计算视频位置
// * - 视频起始位置 = 文件大小 - Item:Length
// *
// * 方式二：华为 ftyp 搜索方式（回退）
// * - 从文件中搜索 "ftyp" 标识（MP4 文件的开头）
// * - ftyp 前 4 字节是 box size，定位 MP4 起始位置
// * - 从 MP4 起始位置到文件末尾就是完整的 MP4 数据
// *
// * @param inputFilePath JPEG Motion Photo 文件路径
// * @return MP4 视频数据的字节数组，失败返回 null
// */
//extern "C" JNIEXPORT jbyteArray JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractJpegMotionPhotoVideo(
//        JNIEnv *env, jclass clazz, jstring inputFilePath) {
//    LOGI_MP("============================================================");
//    LOGI_MP("nativeExtractGoogleJpegMotionPhotoVideo: START");
//    LOGI_MP("============================================================");
//
//    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
//    if (!filePath) {
//        LOGE_MP("[ExtractJPEG] Failed to get input file path");
//        return nullptr;
//    }
//    LOGD_MP("[ExtractJPEG] Input file: %s", filePath);
//
//    // 打开文件
//    FILE *file = fopen(filePath, "rb");
//    if (!file) {
//        LOGE_MP("[ExtractJPEG] Failed to open file: %s (errno=%d)", filePath,
//                errno);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    // 获取文件大小
//    fseek(file, 0, SEEK_END);
//    long fileSize = ftell(file);
//    fseek(file, 0, SEEK_SET);
//    LOGD_MP("[ExtractJPEG] File size: %ld bytes (%.2f MB)", fileSize,
//            fileSize / (1024.0 * 1024.0));
//
//    if (fileSize < 16) {
//        LOGE_MP("[ExtractJPEG] File too small to be a valid Motion Photo");
//        fclose(file);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    // 读取整个文件到内存
//    std::vector<uint8_t> fileData(fileSize);
//    size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
//    fclose(file);
//
//    if (bytesRead != static_cast<size_t>(fileSize)) {
//        LOGE_MP("[ExtractJPEG] Failed to read file (read %zu of %ld)", bytesRead,
//                fileSize);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//    LOGD_MP("[ExtractJPEG] File loaded into memory: %zu bytes", bytesRead);
//
//    // 验证 JPEG 文件头 (FF D8 FF)
//    if (fileData[0] != 0xFF || fileData[1] != 0xD8 || fileData[2] != 0xFF) {
//        LOGE_MP("[ExtractJPEG] Not a valid JPEG file (header: %02X %02X %02X)",
//                fileData[0], fileData[1], fileData[2]);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//    LOGD_MP("[ExtractJPEG] Valid JPEG header detected (FF D8 FF)");
//
//    long mp4StartPos = -1;
//    uint32_t mp4Size = 0;
//    const char *locateMethod = "unknown";
//
//    // ==================== 方式一：尝试 XMP 方式 ====================
//    LOGI_MP("[ExtractJPEG] Trying XMP method (Google style)...");
//    MotionPhotoXmpInfo xmpInfo = ParseJpegMotionPhotoXmp(fileData);
//
//    if (xmpInfo.isMotionPhoto && xmpInfo.videoLength > 0) {
//        LOGI_MP(
//                "[ExtractJPEG] XMP method: Motion Photo confirmed, video length = %ld",
//                xmpInfo.videoLength);
//
//        // 计算视频起始位置
//        mp4StartPos = fileSize - xmpInfo.videoLength;
//
//        if (mp4StartPos > 0 && mp4StartPos < fileSize) {
//            mp4Size = static_cast<uint32_t>(xmpInfo.videoLength);
//            locateMethod = "XMP (Google)";
//
//            // 验证视频数据是否以 ftyp 开头
//            if (mp4StartPos + 8 <= fileSize) {
//                // 检查偏移+4位置是否为 "ftyp"
//                if (fileData[mp4StartPos + 4] == 'f' &&
//                    fileData[mp4StartPos + 5] == 't' &&
//                    fileData[mp4StartPos + 6] == 'y' &&
//                    fileData[mp4StartPos + 7] == 'p') {
//                    LOGD_MP(
//                            "[ExtractJPEG] XMP method: Verified ftyp at calculated position");
//                } else {
//                    LOGW_MP("[ExtractJPEG] XMP method: No ftyp at calculated position, "
//                            "data may be invalid");
//                    LOGW_MP("[ExtractJPEG]   Expected 'ftyp', got: %02X %02X %02X %02X",
//                            fileData[mp4StartPos + 4], fileData[mp4StartPos + 5],
//                            fileData[mp4StartPos + 6], fileData[mp4StartPos + 7]);
//                }
//            }
//
//            LOGD_MP("[ExtractJPEG] XMP method: MP4 starts at offset %ld",
//                    mp4StartPos);
//        } else {
//            LOGW_MP("[ExtractJPEG] XMP method: Invalid calculated position %ld",
//                    mp4StartPos);
//            mp4StartPos = -1;
//        }
//    } else {
//        if (!xmpInfo.isMotionPhoto) {
//            LOGD_MP("[ExtractJPEG] XMP method: Not a Google Motion Photo "
//                    "(GCamera:MotionPhoto != 1)");
//        } else {
//            LOGD_MP("[ExtractJPEG] XMP method: No video length in XMP (Item:Length "
//                    "not found)");
//        }
//    }
//
//    // ==================== 方式二：回退到 ftyp 搜索方式 ====================
//    if (mp4StartPos < 0) {
//        LOGI_MP(
//                "[ExtractJPEG] Falling back to ftyp search method (Huawei style)...");
//
//        const uint8_t ftypSignature[4] = {0x66, 0x74, 0x79, 0x70}; // "ftyp"
//        long ftypPos = -1;
//
//        // 从文件中间往后搜索（因为 JPEG 数据在前面）
//        long searchStart = (fileSize > 1024) ? 1024 : 0;
//
//        for (long i = searchStart; i < fileSize - 4; ++i) {
//            if (fileData[i] == ftypSignature[0] &&
//                fileData[i + 1] == ftypSignature[1] &&
//                fileData[i + 2] == ftypSignature[2] &&
//                fileData[i + 3] == ftypSignature[3]) {
//                ftypPos = i;
//                LOGD_MP(
//                        "[ExtractJPEG] ftyp method: Found 'ftyp' signature at offset %ld",
//                        ftypPos);
//                break;
//            }
//        }
//
//        if (ftypPos >= 4) {
//            // ftyp 前 4 字节是 ftyp box 的 size
//            mp4StartPos = ftypPos - 4;
//            mp4Size = static_cast<uint32_t>(fileSize - mp4StartPos);
//            locateMethod = "ftyp search (Huawei)";
//
//            // 读取 ftyp box size 来验证
//            uint32_t ftypBoxSize =
//                    (static_cast<uint32_t>(fileData[mp4StartPos]) << 24) |
//                    (static_cast<uint32_t>(fileData[mp4StartPos + 1]) << 16) |
//                    (static_cast<uint32_t>(fileData[mp4StartPos + 2]) << 8) |
//                    static_cast<uint32_t>(fileData[mp4StartPos + 3]);
//
//            LOGD_MP("[ExtractJPEG] ftyp method: ftyp box size = %u bytes",
//                    ftypBoxSize);
//
//            // 验证 ftyp box size 是否合理
//            if (ftypBoxSize < 8 || ftypBoxSize > 256) {
//                LOGW_MP("[ExtractJPEG] ftyp method: Unusual ftyp box size: %u "
//                        "(expected 8-256)",
//                        ftypBoxSize);
//            }
//
//            LOGD_MP("[ExtractJPEG] ftyp method: MP4 starts at offset %ld",
//                    mp4StartPos);
//        } else {
//            LOGE_MP(
//                    "[ExtractJPEG] ftyp method: 'ftyp' not found or invalid position");
//        }
//    }
//
//    // ==================== 提取结果检查 ====================
//    if (mp4StartPos < 0 || mp4Size == 0) {
//        LOGE_MP("[ExtractJPEG] Failed to locate MP4 video data");
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    LOGD_MP("[ExtractJPEG] MP4 data: offset %ld, size %u bytes, method: %s",
//            mp4StartPos, mp4Size, locateMethod);
//
//    // 打印 MP4 文件头信息
//    if (mp4Size >= 12 && mp4StartPos + 12 <= fileSize) {
//        uint32_t firstBoxSize =
//                (static_cast<uint32_t>(fileData[mp4StartPos]) << 24) |
//                (static_cast<uint32_t>(fileData[mp4StartPos + 1]) << 16) |
//                (static_cast<uint32_t>(fileData[mp4StartPos + 2]) << 8) |
//                static_cast<uint32_t>(fileData[mp4StartPos + 3]);
//
//        LOGD_MP("[ExtractJPEG] MP4 first box: size=%u, type='%c%c%c%c', "
//                "brand='%c%c%c%c'",
//                firstBoxSize, fileData[mp4StartPos + 4], fileData[mp4StartPos + 5],
//                fileData[mp4StartPos + 6], fileData[mp4StartPos + 7],
//                fileData[mp4StartPos + 8], fileData[mp4StartPos + 9],
//                fileData[mp4StartPos + 10], fileData[mp4StartPos + 11]);
//    }
//
//    // 计算 JPEG 图片大小
//    long jpegSize = mp4StartPos;
//    LOGD_MP("[ExtractJPEG] JPEG image size: %ld bytes (%.2f KB)", jpegSize,
//            jpegSize / 1024.0);
//
//    // 创建 Java byte array
//    jbyteArray result = env->NewByteArray(static_cast<jsize>(mp4Size));
//    if (!result) {
//        LOGE_MP("[ExtractJPEG] Failed to allocate Java byte array for %u bytes",
//                mp4Size);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return nullptr;
//    }
//
//    // 复制 MP4 数据到 Java byte array
//    env->SetByteArrayRegion(
//            result, 0, static_cast<jsize>(mp4Size),
//            reinterpret_cast<const jbyte *>(fileData.data() + mp4StartPos));
//
//    LOGI_MP("============================================================");
//    LOGI_MP("SUCCESS!");
//    LOGI_MP("  Input file: %s", filePath);
//    LOGI_MP("  File size: %ld bytes", fileSize);
//    LOGI_MP("  Locate method: %s", locateMethod);
//    if (xmpInfo.isMotionPhoto) {
//        LOGI_MP("  XMP info: MotionPhoto=1, version=%d, videoLength=%ld",
//                xmpInfo.version, xmpInfo.videoLength);
//    }
//    LOGI_MP("  JPEG size: %ld bytes (%.2f KB)", jpegSize, jpegSize / 1024.0);
//    LOGI_MP("  MP4 offset: %ld", mp4StartPos);
//    LOGI_MP("  MP4 extracted: %u bytes (%.2f MB)", mp4Size,
//            mp4Size / (1024.0 * 1024.0));
//    LOGI_MP("============================================================");
//
//    env->ReleaseStringUTFChars(inputFilePath, filePath);
//    return result;
//}
//
///**
// * 需求：根据输入的图片地址，检查图片是否为哪种 Motion Photo。
// *
// * 逻辑：
// * 1. 读取文件原始字节确定图片真正的格式是 JPEG 或 HEIC。
// *
// * 2. 若格式为 JPEG
// * 2.1 读取 XMP，若存在 MotionPhoto="1" 或者 MicroVideo="1" 字段，则确定为 JPEG_MOTION_PHOTO
// * 2.2 如果上述不成立，搜索 ftyp 标识（MP4 文件头），如果存在特定的“mp4”的视频字符，则确定为 JPEG_MOTION_PHOTO
// * 2.3 否则返回 MOTION_PHOTO_TYPE_NONE
// *
// * 3、若格式为 HEIC
// * 3.1 使用 libheif 读取 XMP,若存在 MotionPhoto="1" 或者 MicroVideo="1" 字段，则确定为 HEIC_MOTION_PHOTO
// * 3.2 如果上述不成立，则检索 “mpvd” 字符，如果存在，则确定为 HEIC_MOTION_PHOTO
// * 3.3 否则返回 MOTION_PHOTO_TYPE_NONE
// *
// * 4、如果文件非 JPEG 和 HEIC，则直接返回 MOTION_PHOTO_TYPE_NONE
// *
// * 返回：Motion Photo Type枚举：
// * JPEG_MOTION_PHOTO(0)：JPEG 格式的动态照片
// * HEIC_MOTION_PHOTO(1)：HEIC 格式的动态照片
// * MOTION_PHOTO_TYPE_NONE(2)：非动态照片
// * */
//
//// Motion Photo Type 枚举值
//#define MOTION_PHOTO_TYPE_JPEG 0
//#define MOTION_PHOTO_TYPE_HEIC 1
//#define MOTION_PHOTO_TYPE_NONE 2
//
//extern "C" JNIEXPORT jint JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeCheckMotionPhotoType(
//        JNIEnv *env, jclass clazz, jstring inputFilePath) {
//    const char *filePath = env->GetStringUTFChars(inputFilePath, nullptr);
//    if (!filePath) {
//        LOGE_MP("[CheckType] Failed to get input file path");
//        return MOTION_PHOTO_TYPE_NONE;
//    }
//
//    // 打开文件
//    FILE *file = fopen(filePath, "rb");
//    if (!file) {
//        LOGE_MP("[CheckType] Failed to open file: %s", filePath);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return MOTION_PHOTO_TYPE_NONE;
//    }
//
//    // 获取文件大小
//    fseek(file, 0, SEEK_END);
//    long fileSize = ftell(file);
//    fseek(file, 0, SEEK_SET);
//
//    if (fileSize < 12) {
//        LOGD_MP("[CheckType] File too small: %ld bytes", fileSize);
//        fclose(file);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return MOTION_PHOTO_TYPE_NONE;
//    }
//
//    // 读取文件头（足够识别格式）
//    uint8_t header[12];
//    if (fread(header, 1, 12, file) != 12) {
//        LOGE_MP("[CheckType] Failed to read file header");
//        fclose(file);
//        env->ReleaseStringUTFChars(inputFilePath, filePath);
//        return MOTION_PHOTO_TYPE_NONE;
//    }
//
//    // 判断文件格式
//    bool isJpeg = (header[0] == 0xFF && header[1] == 0xD8 && header[2] == 0xFF);
//    bool isHeic = (header[4] == 'f' && header[5] == 't' && header[6] == 'y' &&
//                   header[7] == 'p');
//
//    LOGD_MP("[CheckType] File: %s, size: %ld, isJpeg: %d, isHeic: %d", filePath,
//            fileSize, isJpeg, isHeic);
//
//    if (isJpeg) {
//        // ==================== JPEG 格式处理 ====================
//        LOGD_MP("[CheckType] Detected JPEG format");
//
//        // 读取整个文件
//        fseek(file, 0, SEEK_SET);
//        std::vector<uint8_t> fileData(fileSize);
//        size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
//        fclose(file);
//        file = nullptr;
//
//        if (bytesRead != static_cast<size_t>(fileSize)) {
//            LOGE_MP("[CheckType] Failed to read JPEG file");
//            env->ReleaseStringUTFChars(inputFilePath, filePath);
//            return MOTION_PHOTO_TYPE_NONE;
//        }
//
//        // 方式1: 检查 XMP 元数据
//        MotionPhotoXmpInfo xmpInfo = ParseJpegMotionPhotoXmp(fileData);
//        LOGD_MP("[CheckType] JPEG XMP check: isMotionPhoto=%d",
//                xmpInfo.isMotionPhoto);
//
//        // 如果 XMP 确认是动态照片，直接返回
//        if (xmpInfo.isMotionPhoto) {
//            LOGI_MP("[CheckType] Result: JPEG_MOTION_PHOTO (XMP=1)");
//            env->ReleaseStringUTFChars(inputFilePath, filePath);
//            return MOTION_PHOTO_TYPE_JPEG;
//        }
//
//        // 方式2: XMP 未确认，搜索 ftyp 标识（MP4 文件头）
//        const uint8_t ftypSignature[4] = {0x66, 0x74, 0x79, 0x70}; // "ftyp"
//        // 从 JPEG 数据之后开始搜索（跳过前 1KB 的 JPEG 头部区域）
//        long searchStart = (fileSize > 1024) ? 1024 : 0;
//        for (long i = searchStart; i < fileSize - 4; ++i) {
//            if (fileData[i] == ftypSignature[0] &&
//                fileData[i + 1] == ftypSignature[1] &&
//                fileData[i + 2] == ftypSignature[2] &&
//                fileData[i + 3] == ftypSignature[3]) {
//                LOGD_MP("[CheckType] JPEG ftyp found at offset %ld", i);
//                LOGI_MP("[CheckType] Result: JPEG_MOTION_PHOTO (ftyp=1)");
//                env->ReleaseStringUTFChars(inputFilePath, filePath);
//                return MOTION_PHOTO_TYPE_JPEG;
//            }
//        }
//
//        LOGD_MP("[CheckType] JPEG is not a Motion Photo");
//
//    } else if (isHeic) {
//        // ==================== HEIC 格式处理 ====================
//        LOGD_MP("[CheckType] Detected HEIC format");
//        fclose(file);
//        file = nullptr;
//
//        // 方式1: 使用 libheif 检查 XMP 元数据
//        MotionPhotoXmpInfo xmpInfo = ParseHeicMotionPhotoXmpWithLibheif(filePath);
//        LOGD_MP("[CheckType] HEIC XMP check: isMotionPhoto=%d",
//                xmpInfo.isMotionPhoto);
//
//        // 如果 XMP 确认是动态照片，直接返回
//        if (xmpInfo.isMotionPhoto) {
//            LOGI_MP("[CheckType] Result: HEIC_MOTION_PHOTO (XMP=1)");
//            env->ReleaseStringUTFChars(inputFilePath, filePath);
//            return MOTION_PHOTO_TYPE_HEIC;
//        }
//
//        // 方式2: XMP 未确认，搜索 mpvd box 中的 ftyp 标识
//        FILE *heicFile = fopen(filePath, "rb");
//        if (heicFile) {
//            std::vector<uint8_t> fileData(fileSize);
//            fread(fileData.data(), 1, fileSize, heicFile);
//            fclose(heicFile);
//
//            // 搜索 mpvd box
//            const uint8_t mpvdSignature[4] = {0x6D, 0x70, 0x76, 0x64}; // "mpvd"
//            for (long i = fileSize - 4; i >= 4; --i) {
//                if (fileData[i] == mpvdSignature[0] &&
//                    fileData[i + 1] == mpvdSignature[1] &&
//                    fileData[i + 2] == mpvdSignature[2] &&
//                    fileData[i + 3] == mpvdSignature[3]) {
//                    // 找到 mpvd box，检查其中是否有 ftyp
//                    long mpvdDataStart = i + 4;
//                    if (mpvdDataStart + 8 <= fileSize) {
//                        // 检查 mpvd 数据区域是否以 ftyp 开头
//                        if (fileData[mpvdDataStart + 4] == 'f' &&
//                            fileData[mpvdDataStart + 5] == 't' &&
//                            fileData[mpvdDataStart + 6] == 'y' &&
//                            fileData[mpvdDataStart + 7] == 'p') {
//                            LOGD_MP("[CheckType] HEIC mpvd+ftyp found at offset %ld", i);
//                            LOGI_MP("[CheckType] Result: HEIC_MOTION_PHOTO (ftyp=1)");
//                            env->ReleaseStringUTFChars(inputFilePath, filePath);
//                            return MOTION_PHOTO_TYPE_HEIC;
//                        }
//                    }
//                    break;
//                }
//            }
//        }
//
//        LOGD_MP("[CheckType] HEIC is not a Motion Photo");
//
//    } else {
//        // 未知格式
//        LOGD_MP("[CheckType] Unknown format (header: %02X %02X %02X %02X %02X %02X "
//                "%02X %02X)",
//                header[0], header[1], header[2], header[3], header[4], header[5],
//                header[6], header[7]);
//        fclose(file);
//        file = nullptr;
//    }
//
//    // 清理并返回非动态照片
//    if (file) {
//        fclose(file);
//    }
//    env->ReleaseStringUTFChars(inputFilePath, filePath);
//
//    LOGI_MP("[CheckType] Final result: NONE (not a Motion Photo)");
//    return MOTION_PHOTO_TYPE_NONE;
//}
//
//static std::vector<uint8_t> JByteArrayToVector(JNIEnv *env, jbyteArray arr) {
//    std::vector<uint8_t> out;
//    if (!env || !arr) return out;
//    jsize len = env->GetArrayLength(arr);
//    if (len <= 0) return out;
//    out.resize(static_cast<size_t>(len));
//    jbyte *p = env->GetByteArrayElements(arr, nullptr);
//    if (!p) {
//        out.clear();
//        return out;
//    }
//    memcpy(out.data(), p, static_cast<size_t>(len));
//    env->ReleaseByteArrayElements(arr, p, JNI_ABORT);
//    return out;
//}
//
//static std::vector<uint8_t> ExtractHeicExifTiff(const char *filePath) {
//    std::vector<uint8_t> out;
//    if (!filePath) return out;
//
//    heif_context *ctx = heif_context_alloc();
//    if (!ctx) return out;
//
//    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
//    if (err.code != heif_error_Ok) {
//        heif_context_free(ctx);
//        return out;
//    }
//
//    heif_image_handle *handle = nullptr;
//    err = heif_context_get_primary_image_handle(ctx, &handle);
//    if (err.code != heif_error_Ok || !handle) {
//        heif_context_free(ctx);
//        return out;
//    }
//
//    int numMetadata = heif_image_handle_get_number_of_metadata_blocks(handle, "Exif");
//    if (numMetadata <= 0) {
//        numMetadata = heif_image_handle_get_number_of_metadata_blocks(handle, nullptr);
//    }
//    if (numMetadata > 0) {
//        std::vector<heif_item_id> metadataIds(numMetadata);
//        heif_image_handle_get_list_of_metadata_block_IDs(handle, nullptr, metadataIds.data(),
//                                                         numMetadata);
//        for (int i = 0; i < numMetadata; i++) {
//            heif_item_id id = metadataIds[i];
//            const char *type = heif_image_handle_get_metadata_type(handle, id);
//            if (!type || strcmp(type, "Exif") != 0) continue;
//
//            size_t metadataSize = heif_image_handle_get_metadata_size(handle, id);
//            if (metadataSize == 0) continue;
//
//            std::vector<uint8_t> buf(metadataSize);
//            err = heif_image_handle_get_metadata(handle, id, buf.data());
//            if (err.code != heif_error_Ok) continue;
//
//            if (buf.size() >= 2 &&
//                ((buf[0] == 'I' && buf[1] == 'I') || (buf[0] == 'M' && buf[1] == 'M'))) {
//                out = std::move(buf);
//                break;
//            }
//
//            if (buf.size() >= 6 &&
//                ((buf[4] == 'I' && buf[5] == 'I') || (buf[4] == 'M' && buf[5] == 'M'))) {
//                out.assign(buf.begin() + 4, buf.end());
//                break;
//            }
//
//            if (buf.size() >= 8) {
//                uint32_t offset = (static_cast<uint32_t>(buf[0]) << 24) |
//                                  (static_cast<uint32_t>(buf[1]) << 16) |
//                                  (static_cast<uint32_t>(buf[2]) << 8) |
//                                  static_cast<uint32_t>(buf[3]);
//                if (offset + 2 <= buf.size() &&
//                    ((buf[offset] == 'I' && buf[offset + 1] == 'I') ||
//                     (buf[offset] == 'M' && buf[offset + 1] == 'M'))) {
//                    out.assign(buf.begin() + static_cast<long>(offset), buf.end());
//                    break;
//                }
//            }
//        }
//    }
//
//    heif_image_handle_release(handle);
//    heif_context_free(ctx);
//    return out;
//}
//
//static std::vector<uint8_t> ExtractPrimaryJpegFromHeic(const char *filePath, int quality) {
//    std::vector<uint8_t> out;
//    if (!filePath) return out;
//
//    heif_context *ctx = heif_context_alloc();
//    if (!ctx) return out;
//
//    heif_error err = heif_context_read_from_file(ctx, filePath, nullptr);
//    if (err.code != heif_error_Ok) {
//        heif_context_free(ctx);
//        return out;
//    }
//
//    heif_image_handle *handle = nullptr;
//    err = heif_context_get_primary_image_handle(ctx, &handle);
//    if (err.code != heif_error_Ok || !handle) {
//        heif_context_free(ctx);
//        return out;
//    }
//
//    heif_image *image = nullptr;
//    err = heif_decode_image(handle, &image, heif_colorspace_RGB, heif_chroma_interleaved_RGB,
//                            nullptr);
//    if (err.code != heif_error_Ok || !image) {
//        heif_image_handle_release(handle);
//        heif_context_free(ctx);
//        return out;
//    }
//
//    int stride = 0;
//    const uint8_t *plane =
//            heif_image_get_plane_readonly(image, heif_channel_interleaved, &stride);
//    int width = heif_image_get_width(image, heif_channel_interleaved);
//    int height = heif_image_get_height(image, heif_channel_interleaved);
//
//    if (!plane || width <= 0 || height <= 0 || stride <= 0) {
//        heif_image_release(image);
//        heif_image_handle_release(handle);
//        heif_context_free(ctx);
//        return out;
//    }
//
//    jpeg_compress_struct cinfo;
//    jpeg_error_mgr jerr;
//    cinfo.err = jpeg_std_error(&jerr);
//    jpeg_create_compress(&cinfo);
//
//    unsigned char *jpegBuf = nullptr;
//    unsigned long jpegSize = 0;
//    jpeg_mem_dest(&cinfo, &jpegBuf, &jpegSize);
//
//    cinfo.image_width = width;
//    cinfo.image_height = height;
//    cinfo.input_components = 3;
//    cinfo.in_color_space = JCS_RGB;
//
//    jpeg_set_defaults(&cinfo);
//    jpeg_set_quality(&cinfo, quality, TRUE);
//    jpeg_start_compress(&cinfo, TRUE);
//
//    std::vector<uint8_t> row(static_cast<size_t>(width) * 3);
//    JSAMPROW rowPtr[1];
//    while (cinfo.next_scanline < cinfo.image_height) {
//        const uint8_t *src = plane + static_cast<size_t>(cinfo.next_scanline) * stride;
//        memcpy(row.data(), src, row.size());
//        rowPtr[0] = row.data();
//        jpeg_write_scanlines(&cinfo, rowPtr, 1);
//    }
//
//    jpeg_finish_compress(&cinfo);
//    jpeg_destroy_compress(&cinfo);
//
//    if (jpegBuf && jpegSize > 0) {
//        out.assign(jpegBuf, jpegBuf + jpegSize);
//        free(jpegBuf);
//    } else if (jpegBuf) {
//        free(jpegBuf);
//    }
//
//    heif_image_release(image);
//    heif_image_handle_release(handle);
//    heif_context_free(ctx);
//    return out;
//}
//
//
//static size_t FindJpegAppInsertPos(const std::vector<uint8_t> &jpeg) {
//    if (jpeg.size() < 4) return 0;
//    if (jpeg[0] != 0xFF || jpeg[1] != 0xD8) return 0;
//
//    size_t pos = 2;
//    while (pos + 4 <= jpeg.size()) {
//        if (jpeg[pos] != 0xFF) break;
//        uint8_t marker = jpeg[pos + 1];
//        if (marker == 0xDA || marker == 0xD9) break;
//        if (marker >= 0xE0 && marker <= 0xEF) {
//            uint16_t len = (static_cast<uint16_t>(jpeg[pos + 2]) << 8) |
//                           static_cast<uint16_t>(jpeg[pos + 3]);
//            if (len < 2) break;
//            size_t next = pos + 2 + static_cast<size_t>(len);
//            if (next > jpeg.size()) break;
//            pos = next;
//            continue;
//        }
//        break;
//    }
//    return pos;
//}
//
//static bool AppendApp1Segment(std::vector<uint8_t> &out, const uint8_t *payload,
//                              size_t payloadLen) {
//    if (!payload || payloadLen == 0) return true;
//    size_t segLen = payloadLen + 2;
//    if (segLen > 0xFFFF) return false;
//    out.push_back(0xFF);
//    out.push_back(0xE1);
//    out.push_back(static_cast<uint8_t>((segLen >> 8) & 0xFF));
//    out.push_back(static_cast<uint8_t>(segLen & 0xFF));
//    out.insert(out.end(), payload, payload + payloadLen);
//    return true;
//}
//
//static std::vector<uint8_t> BuildJpegMotionPhotoBytes(
//        const std::vector<uint8_t> &primaryJpeg,
//        const std::string &xmpXml,
//        const std::vector<uint8_t> &exifTiff,
//        const std::vector<uint8_t> &mp4) {
//    std::vector<uint8_t> out;
//    if (primaryJpeg.size() < 4 || primaryJpeg[0] != 0xFF || primaryJpeg[1] != 0xD8) {
//        return out;
//    }
//    if (mp4.empty()) {
//        return out;
//    }
//
//    size_t insertPos = FindJpegAppInsertPos(primaryJpeg);
//    if (insertPos == 0 || insertPos > primaryJpeg.size()) return out;
//
//    size_t reserveSize =
//            primaryJpeg.size() + mp4.size() + exifTiff.size() + xmpXml.size() + 256;
//    out.reserve(reserveSize);
//
//    out.insert(out.end(), primaryJpeg.begin(), primaryJpeg.begin() + static_cast<long>(insertPos));
//
//    if (!exifTiff.empty()) {
//        const uint8_t exifHeader[] = {'E', 'x', 'i', 'f', 0x00, 0x00};
//        std::vector<uint8_t> exifPayload;
//        exifPayload.reserve(sizeof(exifHeader) + exifTiff.size());
//        exifPayload.insert(exifPayload.end(), exifHeader, exifHeader + sizeof(exifHeader));
//        exifPayload.insert(exifPayload.end(), exifTiff.begin(), exifTiff.end());
//        if (!AppendApp1Segment(out, exifPayload.data(), exifPayload.size())) {
//            out.clear();
//            return out;
//        }
//    }
//
//    if (!xmpXml.empty()) {
//        const char *xmpMarker = "http://ns.adobe.com/xap/1.0/";
//        size_t markerLen = strlen(xmpMarker);
//        std::vector<uint8_t> xmpPayload;
//        xmpPayload.reserve(markerLen + 1 + xmpXml.size());
//        xmpPayload.insert(xmpPayload.end(),
//                          reinterpret_cast<const uint8_t *>(xmpMarker),
//                          reinterpret_cast<const uint8_t *>(xmpMarker) + markerLen);
//        xmpPayload.push_back(0x00);
//        xmpPayload.insert(xmpPayload.end(),
//                          reinterpret_cast<const uint8_t *>(xmpXml.data()),
//                          reinterpret_cast<const uint8_t *>(xmpXml.data() + xmpXml.size()));
//        if (!AppendApp1Segment(out, xmpPayload.data(), xmpPayload.size())) {
//            out.clear();
//            return out;
//        }
//    }
//
//    out.insert(out.end(), primaryJpeg.begin() + static_cast<long>(insertPos), primaryJpeg.end());
//    out.insert(out.end(), mp4.begin(), mp4.end());
//    return out;
//}
//
//static bool WriteBytesToFile(const char *path, const std::vector<uint8_t> &bytes) {
//    if (!path) return false;
//    FILE *f = fopen(path, "wb");
//    if (!f) return false;
//    size_t written = fwrite(bytes.data(), 1, bytes.size(), f);
//    fflush(f);
//    fclose(f);
//    return written == bytes.size();
//}
//
//
///**
// * 生成 Google Motion Photo 格式的 XMP metadata
// *
// * 根据 Android Motion Photo 规范:
// * - GCamera:MotionPhoto = 1 表示这是动态照片
// * - GCamera:MotionPhotoVersion = 1 表示版本
// * - GCamera:MotionPhotoPresentationTimestampUs = 0 表示呈现时间戳
// * - Container:Directory 包含媒体项目信息
// * - 主图像项目需要 Padding = 0
// * - 视频项目需要 Mime、Semantic、Length、Padding = 8
// */
//static std::string GenerateMotionPhotoXMP(size_t mp4VideoLength) {
//    LOGD_MP("[GenerateXMP] START - mp4VideoLength=%zu", mp4VideoLength);
//
//    std::string xmp = R"(<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="Adobe XMP Core 5.1.0-jc003">
//  <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
//    <rdf:Description rdf:about=""
//        xmlns:GCamera="http://ns.google.com/photos/1.0/camera/"
//        xmlns:Container="http://ns.google.com/photos/1.0/container/"
//        xmlns:Item="http://ns.google.com/photos/1.0/container/item/"
//        GCamera:MicroVideo="1"
//        GCamera:MicroVideoVersion="1"
//        GCamera:MicroVideoOffset=")" + std::to_string(mp4VideoLength) + R"("
//        GCamera:MicroVideoPresentationTimestampUs="0"
//        GCamera:MotionPhoto="1"
//        GCamera:MotionPhotoVersion="1"
//        GCamera:MotionPhotoPresentationTimestampUs="0">
//      <Container:Directory>
//        <rdf:Seq>
//          <rdf:li rdf:parseType="Resource">
//            <Item:Mime>image/heic</Item:Mime>
//            <Item:Semantic>Primary</Item:Semantic>
//            <Item:Length>0</Item:Length>
//            <Item:Padding>0</Item:Padding>
//          </rdf:li>
//          <rdf:li rdf:parseType="Resource">
//            <Item:Mime>video/mp4</Item:Mime>
//            <Item:Semantic>MotionPhoto</Item:Semantic>
//            <Item:Length>)" + std::to_string(mp4VideoLength) + R"(</Item:Length>
//            <Item:Padding>8</Item:Padding>
//          </rdf:li>
//        </rdf:Seq>
//      </Container:Directory>
//    </rdf:Description>
//  </rdf:RDF>
//</x:xmpmeta>)";
//
//    LOGD_MP("[GenerateXMP] XMP size=%zu bytes", xmp.size());
//    LOGD_MP("[GenerateXMP] XMP fields: MotionPhoto=1, Version=1, TimestampUs=0");
//    LOGD_MP("[GenerateXMP] Primary: Mime=image/heic, Semantic=Primary, Padding=8");
//    LOGD_MP("[GenerateXMP] Video: Mime=video/mp4, Semantic=MotionPhoto, Length=%zu", mp4VideoLength);
//    return xmp;
//}
//
//
//static std::string GenerateJpegMotionPhotoXMP(size_t mp4VideoLength) {
//    std::string xmp = R"(<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="Adobe XMP Core 5.1.0-jc003">
//  <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
//    <rdf:Description rdf:about=""
//        xmlns:GCamera="http://ns.google.com/photos/1.0/camera/"
//        xmlns:Container="http://ns.google.com/photos/1.0/container/"
//        xmlns:Item="http://ns.google.com/photos/1.0/container/item/"
//        xmlns:OpCamera="http://ns.oplus.com/photos/1.0/camera/"
//        GCamera:MicroVideo="1"
//        GCamera:MicroVideoVersion="1"
//        GCamera:MicroVideoOffset=")" + std::to_string(mp4VideoLength) + R"("
//        GCamera:MicroVideoPresentationTimestampUs="0"
//        GCamera:MotionPhoto="1"
//        GCamera:MotionPhotoVersion="1"
//        GCamera:MotionPhotoPresentationTimestampUs="0"
//        OpCamera:MotionPhotoPresentationTimestampUs="0"
//        OpCamera:MotionPhotoOwner="oplus"
//        OpCamera:OLivePhotoVersion="2"
//        OpCamera:VideoLength=")" + std::to_string(mp4VideoLength) + R"("
//        >
//      <Container:Directory>
//        <rdf:Seq>
//          <rdf:li rdf:parseType="Resource">
//            <Container:Item
//                Item:Mime="image/jpeg"
//                Item:Semantic="Primary"
//                Item:Length="0"
//                Item:Padding="0" />
//          </rdf:li>
//          <rdf:li rdf:parseType="Resource">
//            <Container:Item
//                Item:Mime="video/mp4"
//                Item:Semantic="MotionPhoto"
//                Item:Length=")" + std::to_string(mp4VideoLength) + R"("
//                Item:Padding="0" />
//          </rdf:li>
//        </rdf:Seq>
//      </Container:Directory>
//    </rdf:Description>
//  </rdf:RDF>
//</x:xmpmeta>)";
//    return xmp;
//}
//
//
//
///**
// * 需求:
// * 将 JPEG Motion Photo 动态照片转换为 HEIC Motion Photo 动态照片。
// *
// * 逻辑：
// * 1、提取 JPEG 文件里的 JPEG 图像字节和 VIDEO 视频字节数据
// *   1.1、提取 JPEG Motion Photo里的图像数据
// *   1.2、通过 nativeExtractGoogleJpegMotionPhotoVideo() 方法提取视频数据
// * 2、通过 nativeGenGoogleMotionPhotoWithHeic() 方法生成 HEIC 文件
// * 3、将生成后的文件写入到 outPath 文件地址里。并返回 outPath。
// *
// * @param hdrAndVideoSize long array
// * - 长度为3：[Image offset, HDR offset, Video offset]
// * */
//extern "C" JNIEXPORT jstring JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeConvertJpegMotionPhotoToHeic(
//        JNIEnv *env, jclass clazz,
//        jstring inputJpegFilePath,
//        jlongArray hdrAndVideoSize,
//        jstring outPath) {
//    LOGI_MP("============================================================");
//    LOGI_MP("nativeConvertJpegMotionPhotoToHeic: START");
//    LOGI_MP("============================================================");
//
//    const char *filePath = env->GetStringUTFChars(inputJpegFilePath, nullptr);
//    if (!filePath) {
//        LOGE_MP("[ExtractJPEG] Failed to get input file path");
//        return nullptr;
//    }
//    LOGD_MP("[ExtractJPEG] Input file: %s", filePath);
//
//    // 打开文件
//    FILE *file = fopen(filePath, "rb");
//    if (!file) {
//        LOGE_MP("[ExtractJPEG] Failed to open file: %s (errno=%d)", filePath,
//                errno);
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return nullptr;
//    }
//
//    // 获取文件大小
//    fseek(file, 0, SEEK_END);
//    long fileSize = ftell(file);
//    fseek(file, 0, SEEK_SET);
//    LOGD_MP("[Convert] File size: %ld bytes (%.2f MB)", fileSize,
//            fileSize / (1024.0 * 1024.0));
//
//    if (fileSize < 16) {
//        LOGE_MP("[Convert] File too small to be a valid Motion Photo");
//        fclose(file);
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: input file too small");
//    }
//
//    // 读取整个文件到内存
//    std::vector<uint8_t> fileData(fileSize);
//    size_t bytesRead = fread(fileData.data(), 1, fileSize, file);
//    fclose(file);
//    if (bytesRead != static_cast<size_t>(fileSize)) {
//        LOGE_MP("[Convert] Failed to read file (read %zu of %ld)", bytesRead,
//                fileSize);
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: failed to read input file");
//    }
//
//    // 验证 JPEG 文件头 (FF D8 FF)
//    if (fileData[0] != 0xFF || fileData[1] != 0xD8 || fileData[2] != 0xFF) {
//        LOGE_MP("[Convert] Not a valid JPEG file (header: %02X %02X %02X)",
//                fileData[0], fileData[1], fileData[2]);
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: not a valid JPEG file");
//    }
//    LOGD_MP("[Convert] Valid JPEG header detected (FF D8 FF)");
//
//    // 解析 hdrAndVideoSize 参数获取各部分偏移量
//    // 长度固定为 3：[Image Offset, HDR Offset, Video Offset]
//    long jpegStart = 0;
//    long jpegLen = 0;
//    long hdrStart = 0;
//    long hdrLen = 0;
//    long videoStart = 0;
//    long videoLen = 0;
//
//    if (hdrAndVideoSize == nullptr) {
//        LOGE_MP("[Convert] hdrAndVideoSize parameter is null");
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: hdrAndVideoSize is null");
//    }
//
//    jsize arrayLen = env->GetArrayLength(hdrAndVideoSize);
//    if (arrayLen != 3) {
//        LOGE_MP("[Convert] Invalid hdrAndVideoSize length: %d (expected 3)", arrayLen);
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: invalid hdrAndVideoSize length");
//    }
//
//    jlong *offsets = env->GetLongArrayElements(hdrAndVideoSize, nullptr);
//    if (offsets == nullptr) {
//        LOGE_MP("[Convert] Failed to get hdrAndVideoSize array elements");
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: failed to get offsets");
//    }
//
//    jpegStart = static_cast<long>(offsets[0]);
//    hdrStart = static_cast<long>(offsets[1]);
//    videoStart = static_cast<long>(offsets[2]);
//
//    // 计算长度
//    // Image: [jpegStart, hdrStart)
//    // HDR:   [hdrStart, videoStart)
//    // Video: [videoStart, fileSize)
//    jpegLen = hdrStart - jpegStart;
//    hdrLen = videoStart - hdrStart;
//    videoLen = fileSize - videoStart;
//
//    LOGD_MP("[Convert] Offsets: Jpeg[%ld, len=%ld], HDR[%ld, len=%ld], Video[%ld, len=%ld]",
//            jpegStart, jpegLen, hdrStart, hdrLen, videoStart, videoLen);
//
//    env->ReleaseLongArrayElements(hdrAndVideoSize, offsets, JNI_ABORT);
//
//    // 验证偏移量和长度有效性
//    if (jpegStart < 0 || jpegLen <= 0 || hdrLen < 0 || videoLen <= 0 ||
//        videoStart > fileSize || hdrStart > videoStart || jpegStart > hdrStart) {
//        LOGE_MP("[Convert] Invalid offsets or lengths detected");
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: invalid data offsets");
//    }
//
//    // 构造 Java byte arrays
//    jbyteArray jpegArray = env->NewByteArray(static_cast<jsize>(jpegLen));
//    if (!jpegArray) {
//        LOGE_MP("[Convert] Failed to allocate JPEG byte array");
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: failed to allocate JPEG array");
//    }
//    env->SetByteArrayRegion(jpegArray, 0, static_cast<jsize>(jpegLen),
//                            reinterpret_cast<const jbyte *>(fileData.data() + jpegStart));
//
//    jbyteArray hdrArray = nullptr;
//    if (hdrLen > 0) {
//        hdrArray = env->NewByteArray(static_cast<jsize>(hdrLen));
//        if (!hdrArray) {
//            LOGE_MP("[Convert] Failed to allocate HDR byte array");
//            env->DeleteLocalRef(jpegArray);
//            env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//            return env->NewStringUTF("error: failed to allocate HDR array");
//        }
//        env->SetByteArrayRegion(hdrArray, 0, static_cast<jsize>(hdrLen),
//                                reinterpret_cast<const jbyte *>(fileData.data() + hdrStart));
//    }
//
//    jbyteArray mp4Array = env->NewByteArray(static_cast<jsize>(videoLen));
//    if (!mp4Array) {
//        LOGE_MP("[Convert] Failed to allocate MP4 byte array");
//        env->DeleteLocalRef(jpegArray);
//        if (hdrArray) env->DeleteLocalRef(hdrArray);
//        env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//        return env->NewStringUTF("error: failed to allocate MP4 array");
//    }
//    env->SetByteArrayRegion(
//            mp4Array, 0, static_cast<jsize>(videoLen),
//            reinterpret_cast<const jbyte *>(fileData.data() + videoStart));
//
//    // 提取 Exif 数据
//    jbyteArray exifs = nullptr;
//    std::vector<uint8_t> exifDataVec = ExtractJpegExif(filePath);
//    if (!exifDataVec.empty()) {
//        exifs = env->NewByteArray(static_cast<jsize>(exifDataVec.size()));
//        if (exifs) {
//            env->SetByteArrayRegion(exifs, 0, static_cast<jsize>(exifDataVec.size()),
//                                    reinterpret_cast<const jbyte *>(exifDataVec.data()));
//            LOGD_MP("[Convert] Extracted Exif data: %zu bytes", exifDataVec.size());
//        }
//    } else {
//        LOGD_MP("[Convert] No Exif data found in JPEG");
//    }
//
//    // 调用生成 HEIC Motion Photo 的方法
//    LOGI_MP("[Convert] Generating HEIC Motion Photo with "
//            "nativeGenGoogleMotionPhotoWithHeic...");
//    jstring genResult =
//            Java_com_seafile_seadroid2_jni_HeicNative_nativeGenHeicMotionPhoto(
//                    env, clazz, jpegArray, hdrArray, exifs, mp4Array, outPath);
//
//    if (exifs) env->DeleteLocalRef(exifs);
//
//    // 释放大数组的局部引用
//    env->DeleteLocalRef(jpegArray);
//    if (hdrArray) env->DeleteLocalRef(hdrArray);
//    env->DeleteLocalRef(mp4Array);
//
//    // 释放输入路径
//    env->ReleaseStringUTFChars(inputJpegFilePath, filePath);
//
//    if (!genResult) {
//        LOGE_MP("[Convert] HEIC generation returned NULL");
//        return env->NewStringUTF("error: HEIC generation failed");
//    }
//
//    const char *genStr = env->GetStringUTFChars(genResult, nullptr);
//    if (!genStr) {
//        LOGW_MP("[Convert] Failed to get generation result string");
//        return env->NewStringUTF("error: unknown generation result");
//    }
//    LOGI_MP("[Convert] Generation result: %s", genStr);
//
//    bool success = (strncmp(genStr, "success", 7) == 0);
//    env->ReleaseStringUTFChars(genResult, genStr);
//    if (success) {
//        LOGI_MP("[Convert] SUCCESS - returning output path");
//        return outPath;
//    } else {
//        LOGE_MP("[Convert] FAILED - returning error string");
//        return genResult;
//    }
//}
//
//
//
///**
// * 需求:
// * 将 HEIC Motion Photo 动态照片转换为 JPEG Motion Photo 动态照片。
// *
// * 逻辑：
// * 1、提取 HEIC 文件里数据
// *   1.1、提取 JPEG Motion Photo里的 Primary 主图图像数据
// *   1.2、通过 nativeExtractGoogleHeicMotionPhotoVideo() 方法提取视频数据
// *   1.3、提取 HEIC 的 XMP 数据
// *   1.4、提取 HEIC 的 EXIF 数据
// * 2、增加一个本地生成 JPEG Motion Photo 动态照片文件的方法，将第 1 步的数据写入到 JPEG 文件里。
// * 3、生成 JPEG 文件的位置使用 outputFilePath 参数地址。并最终返回 outputFilePath。
// *
// * */
//extern "C" JNIEXPORT jstring JNICALL
//Java_com_seafile_seadroid2_jni_HeicNative_nativeConvertHeicMotionPhotoToJpeg(
//        JNIEnv *env, jclass clazz, jstring inputFilePath, jstring outputFilePath) {
//    LOGI_MP("============================================================");
//    LOGI_MP("nativeConvertHeicMotionPhotoToJpeg: START");
//    LOGI_MP("============================================================");
//
//    const char *inPath = env->GetStringUTFChars(inputFilePath, nullptr);
//    if (!inPath) {
//        LOGE_MP("[ConvertHeic2Jpeg] Failed to get input path");
//        return nullptr;
//    }
//
//    const char *outPath = env->GetStringUTFChars(outputFilePath, nullptr);
//    if (!outPath) {
//        LOGE_MP("[ConvertHeic2Jpeg] Failed to get output path");
//        env->ReleaseStringUTFChars(inputFilePath, inPath);
//        return nullptr;
//    }
//
//    // video
//    jbyteArray mp4Arr = Java_com_seafile_seadroid2_jni_HeicNative_nativeExtractHeicMotionPhotoVideo(
//            env, clazz, inputFilePath);
//    std::vector<uint8_t> mp4Data = JByteArrayToVector(env, mp4Arr);
//    if (mp4Arr) env->DeleteLocalRef(mp4Arr);
//
//    if (mp4Data.empty()) {
//        LOGE_MP("[ConvertHeic2Jpeg] MP4 video data is empty");
//        env->ReleaseStringUTFChars(outputFilePath, outPath);
//        env->ReleaseStringUTFChars(inputFilePath, inPath);
//        return env->NewStringUTF("error: MP4 video data is empty");
//    }
//
//    MotionPhotoXmpInfo xmpInfo = ParseHeicMotionPhotoXmpWithLibheif(inPath);
//    if (!xmpInfo.xmpContent.empty()) {
//        LOGD_MP("[ConvertHeic2Jpeg] Extracted XMP: %zu bytes", xmpInfo.xmpContent.size());
//    } else {
//        LOGD_MP("[ConvertHeic2Jpeg] No XMP extracted from HEIC");
//    }
//
//    std::vector<uint8_t> exifTiff = ExtractHeicExifTiff(inPath);
//    if (!exifTiff.empty()) {
//        LOGD_MP("[ConvertHeic2Jpeg] Extracted Exif(TIFF): %zu bytes", exifTiff.size());
//    } else {
//        LOGD_MP("[ConvertHeic2Jpeg] No Exif extracted from HEIC");
//    }
//
//    std::vector<uint8_t> primaryJpeg = ExtractPrimaryJpegFromHeic(inPath, 95);
//    if (primaryJpeg.empty()) {
//        LOGE_MP("[ConvertHeic2Jpeg] Failed to extract primary JPEG from HEIC");
//        env->ReleaseStringUTFChars(outputFilePath, outPath);
//        env->ReleaseStringUTFChars(inputFilePath, inPath);
//        return env->NewStringUTF("error: failed to extract primary image");
//    }
//
//    std::string xmpXml = GenerateJpegMotionPhotoXMP(mp4Data.size());
//    std::vector<uint8_t> outBytes =
//            BuildJpegMotionPhotoBytes(primaryJpeg, xmpXml, exifTiff, mp4Data);
//    if (outBytes.empty()) {
//        LOGE_MP("[ConvertHeic2Jpeg] Failed to build JPEG Motion Photo bytes");
//        env->ReleaseStringUTFChars(outputFilePath, outPath);
//        env->ReleaseStringUTFChars(inputFilePath, inPath);
//        return env->NewStringUTF("error: failed to build jpeg motion photo");
//    }
//
//    if (!WriteBytesToFile(outPath, outBytes)) {
//        LOGE_MP("[ConvertHeic2Jpeg] Failed to write output file: %s", outPath);
//        env->ReleaseStringUTFChars(outputFilePath, outPath);
//        env->ReleaseStringUTFChars(inputFilePath, inPath);
//        return env->NewStringUTF("error: failed to write output file");
//    }
//
//    LOGI_MP("[ConvertHeic2Jpeg] Output written: %s (%.2f MB)", outPath,
//            outBytes.size() / (1024.0 * 1024.0));
//
//    env->ReleaseStringUTFChars(outputFilePath, outPath);
//    env->ReleaseStringUTFChars(inputFilePath, inPath);
//    return outputFilePath;
//}
