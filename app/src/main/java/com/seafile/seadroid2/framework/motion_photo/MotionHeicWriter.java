package com.seafile.seadroid2.framework.motion_photo;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.media.ExifInterface;
import android.util.Base64;
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.core.util.Pair;
import androidx.heifwriter.HeifWriter;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.XMPMetaFactory;
import com.adobe.internal.xmp.options.PropertyOptions;
import com.adobe.internal.xmp.options.SerializeOptions;
import com.blankj.utilcode.util.FileUtils;
import com.drew.imaging.ImageMetadataReader;
import com.drew.metadata.Metadata;
import com.drew.metadata.xmp.XmpDirectory;
import com.drew.tools.FileUtil;
import com.seafile.seadroid2.framework.datastore.DataManager;

import org.mp4parser.BasicContainer;
import org.mp4parser.Box;
import org.mp4parser.IsoFile;
import org.mp4parser.PropertyBoxParserImpl;
import org.mp4parser.boxes.UserBox;
import org.mp4parser.boxes.iso14496.part12.ItemLocationBox;
import org.mp4parser.boxes.iso14496.part12.MetaBox;
import org.mp4parser.boxes.iso14496.part12.XmlBox;

import java.io.*;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;


/**
 * MotionHeicWriterComplete
 * <p>
 * End-to-end:
 * - create HEIC primary image with HeifWriter
 * - build XMP RDF/XML string containing Container:Directory + GCamera MotionPhoto tags
 * - inject XMP into meta/xml via XmlBox.setXml(String)
 * - append mpvd box (support largesize)
 * <p>
 * Limitations / notes:
 * - EXIF injection is left to HeifWriter.setExif (if available) or separate isoparser item insertion if you need strict item-based EXIF.
 * - This class focuses on putting XMP in meta/xml (as you requested) and appending mpvd.
 */
public final class MotionHeicWriter {
    private static final String TAG = "MotionHeicWriterComplete";

    private MotionHeicWriter() {
    }

    private static int computePayloadSize(int xmpLen, int margin) {
        int raw = xmpLen + Math.max(0, margin);
        // align to 16 bytes for tidiness
        int align = 16;
        int rem = raw % align;
        if (rem == 0) return raw;
        return raw + (align - rem);
    }

    @Nullable
    public static File writeMotionHeic(String path, String fileName) throws Exception {
        byte[] bytes = MotionPhotoParser.readBytesFromContentUri(path);
        MotionPhotoParser.MotionPhotoType motionPhotoType = MotionPhotoParser.checkMotionPhotoType(bytes);
        if (!motionPhotoType.isMotionPhoto()) {
            return null;
        }

        if (motionPhotoType != MotionPhotoParser.MotionPhotoType.JPEG_MOTION_PHOTO) {
            return null;
        }

        GoogleMotionPhotoWithJPEGExtractor.ExtractResult extractResult = GoogleMotionPhotoWithJPEGExtractor.extractData(bytes);
        if (extractResult == null) {
            return null;
        }

        byte[] jpegBytes = extractResult.primaryBytes;
        byte[] mp4Bytes = extractResult.videoBytes;

        if (jpegBytes == null || jpegBytes.length == 0) throw new IllegalArgumentException("jpegBytes empty");
        if (mp4Bytes == null || mp4Bytes.length == 0) throw new IllegalArgumentException("mp4Bytes empty");


        // 1) build merged XMP string first (so we can measure size)
        // mpvd header size is 8 bytes (size + 'mpvd')
        int mpvdHeaderSize = 8;
        String xmpString = XmpUtil.buildMotionXmpString(jpegBytes, mp4Bytes.length, mpvdHeaderSize);

        byte[] xmpBytes = xmpString.getBytes(StandardCharsets.UTF_8);
        int xmpLen = xmpBytes.length;
        int margin = 0;

        // 2) compute payload size: choose alignment and margin
        int payload = computePayloadSize(xmpLen, margin);

        byte[] exifBytes = ExifUtil.extractExifFromJpeg(jpegBytes); // may be null

        //  create temporary base HEIC (primary image) using HeifWriter
        File baseHeic = null;
        try {
            baseHeic = createPrimaryHeic(jpegBytes);
        } catch (Exception e) {
            baseHeic.delete();
            throw e;
        }


        // 4) insert xml placeholder with computed payload
        File xmlPlace = new File(baseHeic.getAbsolutePath() + ".xmlplaceholder.heic");
        BinaryHeicXmlPlaceholderInjector.insertXmlPlaceholder(baseHeic, xmlPlace, payload);

        // 5) replace xml payload
        File xmlInjected = new File(baseHeic.getAbsolutePath() + ".xmlinjected.heic");
        BinaryHeicXmpRewriter.replaceXmlBoxIfFits(xmlPlace, xmpString, xmlInjected);

        File outHeic = DataManager.createTempFile(".heic");
        // 6) append mpvd and write final
        try (FileOutputStream fos = new FileOutputStream(outHeic)) {
            fos.write(FileUtil.readBytes(xmlInjected));
            fos.write(MpvdBox.buildMpvdBox(mp4Bytes));
            fos.flush();
        }

        // cleanup
        baseHeic.delete();
        xmlPlace.delete();
        xmlInjected.delete();

        return outHeic;

//        File tmpJpegHeic = DataManager.createTempFile(".jpg");
//        //将 bytes数据写入到tmpJpegHeic文件里
//        try (FileOutputStream fos = new FileOutputStream(tmpJpegHeic)) {
//            fos.write(jpegBytes);
//        }
//
//        fileName = fileName + ".heic";
//
//        File g = writeMotionHeic(tmpJpegHeic, tmpHeic, imageBytes, mp4Bytes);
//        FileUtils.rename(g, fileName);
//        return g;
    }


    // ----------------- create HEIC primary using HeifWriter (bitmap) -----------------
    private static File createPrimaryHeic(byte[] imageBytes) throws Exception {
        Bitmap bmp = BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.length);
        if (bmp == null) throw new IOException("decode image failed");
        int w = bmp.getWidth();
        int h = bmp.getHeight();

        File out = DataManager.createTempFile(".heic");

        HeifWriter.Builder builder = new HeifWriter.Builder(out.getAbsolutePath(), w, h, HeifWriter.INPUT_MODE_BITMAP)
                .setQuality(100)
                .setMaxImages(1);
        // If your environment's HeifWriter.Builder supports setExif(byte[]) and you want to set EXIF, do it here.
        try (HeifWriter writer = builder.build()) {
            try {
                writer.start();
                writer.addBitmap(bmp);
            } finally {
                writer.stop(0);

                // 回收Bitmap以释放内存
                if (!bmp.isRecycled()) {
                    bmp.recycle();
                }
            }
        }

        return out;
    }
}


