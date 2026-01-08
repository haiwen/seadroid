package com.seafile.seadroid2.framework.motionphoto;

import android.content.Context;
import android.content.res.AssetFileDescriptor;
import android.database.Cursor;
import android.net.Uri;
import android.text.TextUtils;
import android.provider.OpenableColumns;

import androidx.core.util.Pair;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.XMPMetaFactory;
import com.adobe.internal.xmp.options.IteratorOptions;
import com.adobe.internal.xmp.options.SerializeOptions;
import com.adobe.internal.xmp.properties.XMPPropertyInfo;
import com.blankj.utilcode.util.FileIOUtils;
import com.drew.imaging.ImageMetadataReader;
import com.drew.imaging.ImageProcessingException;
import com.drew.metadata.Metadata;
import com.drew.metadata.xmp.XmpDirectory;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.jni.HeicNative;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

@Todo
public final class MotionPhotoDetector {
    public static final String TAG = "MotionPhotoDetector";

    private MotionPhotoDetector() {
    }

    private static final byte[] XMP_HEADER_XAP = "http://ns.adobe.com/xap/1.0/\0".getBytes(StandardCharsets.US_ASCII);
    private static final byte[] XMP_HEADER_XMP = "http://ns.adobe.com/xmp/1.0/\0".getBytes(StandardCharsets.US_ASCII);

    public static final String RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String GCAMERA_NS = "http://ns.google.com/photos/1.0/camera/";
    public static final String CONTAINER_NS = "http://ns.google.com/photos/1.0/container/";
    public static final String ITEM_NS = "http://ns.google.com/photos/1.0/container/item/";

    public static MotionPhotoDescriptor extractJpegXmp(Context context, Uri p) {
        MotionPhotoDescriptor descriptor = new MotionPhotoDescriptor();
        try (InputStream is = context.getContentResolver().openInputStream(p)) {
            if (is == null) {
                SafeLogs.e(TAG, "parseJpegXmpWithUri()", "openInputStream is null");
                return descriptor;
            }

            Metadata metadata = ImageMetadataReader.readMetadata(is);
            return extractXmp(metadata, true);
        } catch (ImageProcessingException | IOException e) {
            SafeLogs.e(TAG, "parseJpegXmpWithUri()", e.getMessage());
            return descriptor;
        }
    }

    public static MotionPhotoDescriptor extractJpegXmp(InputStream p) {
        try {
            Metadata metadata = ImageMetadataReader.readMetadata(p);
            return extractXmp(metadata, true);
        } catch (ImageProcessingException | IOException e) {
            return new MotionPhotoDescriptor();
        }
    }

    public static MotionPhotoDescriptor extractJpegXmp(File p) {
        try {
            Metadata metadata = ImageMetadataReader.readMetadata(p);
            return extractXmp(metadata, true);
        } catch (ImageProcessingException | IOException e) {
            return new MotionPhotoDescriptor();
        }
    }

    public static MotionPhotoDescriptor extractJpegXmp(byte[] jpeg) {
        try {
            Metadata metadata = ImageMetadataReader.readMetadata(new ByteArrayInputStream(jpeg));
            return extractXmp(metadata, true);
        } catch (ImageProcessingException | IOException e) {
            return new MotionPhotoDescriptor();
        }
    }

    public static MotionPhotoDescriptor extractHeicXmp(File p) {
        try {
            String xml = HeicNative.nativeExtractHeicMotionPhotoXMP(p.getAbsolutePath());
            MotionPhotoDescriptor descriptor = parse(xml, false);
            return descriptor;
        } catch (Exception e) {
            return new MotionPhotoDescriptor();
        }
    }

//    public static MotionPhotoDescriptor extractHeicXmp(InputStream p) {
//        try {
//
//            Metadata metadata = ImageMetadataReader.readMetadata(p);
//            return extractXmp(metadata, false);
//        } catch (ImageProcessingException | IOException e) {
//            return new MotionPhotoDescriptor();
//        }
//    }

    public static MotionPhotoDescriptor extractXmp(Metadata metadata, boolean isJpeg) {
        MotionPhotoDescriptor descriptor = new MotionPhotoDescriptor();
        if (metadata == null) {
            return descriptor;
        }
        XmpDirectory xmpDirectory = metadata.getFirstDirectoryOfType(XmpDirectory.class);
        if (xmpDirectory == null) {
            return descriptor;
        }
        XMPMeta xmpMeta = xmpDirectory.getXMPMeta();
        if (xmpMeta == null) {
            return descriptor;
        }

        SerializeOptions options = new SerializeOptions();
        options.setOmitPacketWrapper(false);   // true=只要 <rdf:RDF>，false=保留<?xpacket?> 包裹
        options.setUseCompactFormat(true);     // 紧凑格式
        options.setIndent("  ");
        try {
            String xml = XMPMetaFactory.serializeToString(xmpMeta, options);
            descriptor = parse(xml, isJpeg);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }


        return descriptor;
    }


    /**
     * 解析 Motion Photo XMP
     *
     * @param xmpString XMP 字符串
     * @return MotionPhotoDescriptor 对象
     * @throws Exception
     */
    public static MotionPhotoDescriptor parse(String xmpString, boolean isJpeg) throws Exception {
        MotionPhotoDescriptor descriptor = new MotionPhotoDescriptor();

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document doc = builder.parse(new ByteArrayInputStream(xmpString.getBytes(StandardCharsets.UTF_8)));

        Element rdfDescription = (Element) doc.getElementsByTagNameNS(RDF_NS, "Description").item(0);

        if (rdfDescription == null) {
            return descriptor;
        }

        // Google V1 Motion Photo
        String microVideoStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MicroVideo");
        if (TextUtils.equals("1", microVideoStr)) {
            descriptor.source = MotionPhotoDescriptor.Source.MICRO_VIDEO;
            descriptor.mpType = isJpeg ? MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_JPEG
                    : MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_HEIC;

            String presentationTsStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MicroVideoPresentationTimestampUs");
            String microVideoVersion = rdfDescription.getAttributeNS(GCAMERA_NS, "MicroVideoVersion");
            String microVideoOffset = rdfDescription.getAttributeNS(GCAMERA_NS, "MicroVideoOffset");

            if (!presentationTsStr.isEmpty()) {
                descriptor.motionPhotoPresentationTimestampUs = parseLongSafe(presentationTsStr);
            }
            if (!microVideoVersion.isEmpty()) {
                descriptor.motionPhotoVersion = Integer.parseInt(microVideoVersion);
            }

            if (!microVideoOffset.isEmpty()) {
                descriptor.items = new ArrayList<>();
                MotionPhotoDescriptor.MotionPhotoItem mpi = new MotionPhotoDescriptor.MotionPhotoItem();
                mpi.length = parseLongSafe(microVideoOffset);
                descriptor.items.add(mpi);
            }

            return descriptor;
        }

        // Google V2 Motion Photo
        String motionPhotoStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MotionPhoto");
        if (!TextUtils.equals("1", motionPhotoStr)) {
            descriptor.source = MotionPhotoDescriptor.Source.CONTAINER;
            return descriptor;
        }

        descriptor.source = MotionPhotoDescriptor.Source.MOTION_PHOTO;
        descriptor.mpType = isJpeg ? MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_JPEG
                : MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_HEIC;


        String motionPhotoVerStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MotionPhotoVersion");
        if (!TextUtils.isEmpty(motionPhotoVerStr)) {
            descriptor.motionPhotoVersion = Integer.parseInt(motionPhotoVerStr);
        }

        String presentationTsStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MotionPhotoPresentationTimestampUs");
        if (!TextUtils.isEmpty(presentationTsStr)) {
            descriptor.motionPhotoPresentationTimestampUs = parseLongSafe(presentationTsStr);
        }

        descriptor.items = new ArrayList<>();

        // 解析 Container:Directory 下的 Item
        NodeList directoryNodes = rdfDescription.getElementsByTagNameNS(CONTAINER_NS, "Directory");
        if (directoryNodes.getLength() > 0) {
            Element directory = (Element) directoryNodes.item(0);
            NodeList liNodes = directory.getElementsByTagNameNS(RDF_NS, "li");
            for (int i = 0; i < liNodes.getLength(); i++) {
                Element li = (Element) liNodes.item(i);
                NodeList itemNodes = li.getElementsByTagNameNS(CONTAINER_NS, "Item");
                if (itemNodes.getLength() > 0) {
                    Element item = (Element) itemNodes.item(0);
                    MotionPhotoDescriptor.MotionPhotoItem mpi = new MotionPhotoDescriptor.MotionPhotoItem();

                    mpi.mime = item.getAttributeNS(ITEM_NS, "Mime");
                    mpi.semantic = item.getAttributeNS(ITEM_NS, "Semantic");

                    String length = item.getAttributeNS(ITEM_NS, "Length");
                    String padding = item.getAttributeNS(ITEM_NS, "Padding");
                    mpi.length = parseLongSafe(length);
                    mpi.padding = parseLongSafe(padding);

                    descriptor.items.add(mpi);
                }
            }
        }

        return descriptor;
    }

    public static long calcPrimaryJpegLength(File f) throws IOException {
        return calcPrimaryJpegLength(new FileInputStream(f));
    }

    /**
     * 从 InputStream 中顺序扫描，计算 Primary JPEG 的真实字节长度
     */
    public static long calcPrimaryJpegLength(InputStream is) throws IOException {
        if (is == null) {
            return -1;
        }

        long offset = 0;

        // --- SOI ---
        int b1 = is.read();
        int b2 = is.read();
        if (b1 != 0xFF || b2 != 0xD8) {
            return -1;
        }
        offset += 2;

        while (true) {
            int b = is.read();
            if (b == -1) {
                break;
            }
            offset++;

            if (b != 0xFF) {
                continue;
            }

            // 跳过 padding FF
            int marker;
            do {
                marker = is.read();
                if (marker == -1) return -1;
                offset++;
            } while (marker == 0xFF);

            // standalone marker（无 length）
            if (isStandaloneMarker(marker)) {
                if (marker == 0xD9) { // EOI
                    return offset;
                }
                continue;
            }

            // --- SOS ---
            if (marker == 0xDA) {
                // 读取 SOS header 长度
                int lh = is.read();
                int ll = is.read();
                if (lh == -1 || ll == -1) return -1;
                offset += 2;

                int len = (lh << 8) | ll;
                if (len < 2) return -1;

                // 跳过 SOS header 剩余部分
                long skipped = skipFully(is, len - 2);
                if (skipped != len - 2) return -1;
                offset += skipped;

                // 扫描熵编码数据直到 EOI
                while (true) {
                    int data = is.read();
                    if (data == -1) return -1;
                    offset++;

                    if (data == 0xFF) {
                        int next = is.read();
                        if (next == -1) return -1;
                        offset++;

                        if (next == 0x00) {
                            // byte stuffing
                            continue;
                        }
                        if (next == 0xD9) {
                            // EOI
                            return offset;
                        }
                        // 其他 marker：继续扫描
                    }
                }
            }

            // --- 普通 marker，读取 length ---
            int lh = is.read();
            int ll = is.read();
            if (lh == -1 || ll == -1) {
                return -1;
            }
            offset += 2;

            int len = (lh << 8) | ll;
            if (len < 2) {
                return -1;
            }

            long skipped = skipFully(is, len - 2);
            if (skipped != len - 2) {
                return -1;
            }
            offset += skipped;
        }

        return -1;
    }

    private static long skipFully(InputStream is, long bytes) throws IOException {
        long remaining = bytes;
        while (remaining > 0) {
            long skipped = is.skip(remaining);
            if (skipped <= 0) {
                // fallback: 读一个字节防止死循环
                if (is.read() == -1) {
                    break;
                }
                skipped = 1;
            }
            remaining -= skipped;
        }
        return bytes - remaining;
    }

    /**
     * 计算 Primary JPEG 的真实字节长度（直到第一个合法 EOI）
     *
     * @param jpegBytes JPEG 字节数据
     * @return JPEG 真实长度（EOI 之后的 offset），失败返回 -1
     */
    public static long calcPrimaryJpegLength(byte[] jpegBytes) {
        if (jpegBytes == null || jpegBytes.length < 4) {
            return -1;
        }

        // SOI
        if ((jpegBytes[0] & 0xFF) != 0xFF || (jpegBytes[1] & 0xFF) != 0xD8) {
            return -1;
        }

        int offset = 2;
        final int size = jpegBytes.length;

        while (offset < size) {
            // 寻找 marker 前导 FF
            if ((jpegBytes[offset] & 0xFF) != 0xFF) {
                offset++;
                continue;
            }

            // 跳过填充 FF
            while (offset < size && (jpegBytes[offset] & 0xFF) == 0xFF) {
                offset++;
            }
            if (offset >= size) {
                break;
            }

            int marker = jpegBytes[offset] & 0xFF;
            offset++;

            // 无 length 的 standalone marker
            if (isStandaloneMarker(marker)) {
                if (marker == 0xD9) { // EOI
                    return offset;
                }
                continue;
            }

            // SOS：进入熵编码段
            if (marker == 0xDA) {
                // 跳过 SOS header
                if (offset + 2 > size) return -1;
                int len = ((jpegBytes[offset] & 0xFF) << 8)
                        | (jpegBytes[offset + 1] & 0xFF);
                offset += len;

                // 扫描熵编码数据，直到遇到 EOI
                while (offset + 1 < size) {
                    int b = jpegBytes[offset] & 0xFF;
                    if (b == 0xFF) {
                        int next = jpegBytes[offset + 1] & 0xFF;
                        if (next == 0x00) {
                            // byte stuffing
                            offset += 2;
                            continue;
                        }
                        if (next == 0xD9) {
                            // EOI
                            return offset + 2;
                        }
                    }
                    offset++;
                }
                break;
            }

            // 普通带 length 的 marker
            if (offset + 2 > size) {
                return -1;
            }

            int len = ((jpegBytes[offset] & 0xFF) << 8)
                    | (jpegBytes[offset + 1] & 0xFF);
            if (len < 2) {
                return -1;
            }

            offset += len;
        }

        return -1;
    }

    /**
     * JPEG 中不带 length 的 marker
     */
    private static boolean isStandaloneMarker(int marker) {
        return (marker >= 0xD0 && marker <= 0xD7) // RST0~RST7
                || marker == 0xD8                // SOI
                || marker == 0xD9                // EOI
                || marker == 0x01;               // TEM
    }

    public static MotionPhotoDescriptor parseJpegXmpWithUri(Context context, Uri uri, boolean copyToLocal) {
        MotionPhotoDescriptor descriptor = new MotionPhotoDescriptor();

        // extract xmp
        try (InputStream is = context.getContentResolver().openInputStream(uri)) {
            if (is == null) {
                SafeLogs.e(TAG, "parseJpegXmpWithUri()", "openInputStream is null");
                return descriptor;
            }

            descriptor = extractJpegXmp(is);
        } catch (IOException e) {
            SafeLogs.e(TAG, "parseJpegXmpWithUri()", e.getMessage());
            return descriptor;
        }

        // check mp
        if (!descriptor.isMotionPhoto()) {
            SafeLogs.e(TAG, "parseJpegXmpWithUri()", "not motion photo");
            return descriptor;
        }

        long primaryLength = -1;
        try (InputStream is = context.getContentResolver().openInputStream(uri)) {
            primaryLength = calcPrimaryJpegLength(is);
        } catch (IOException e) {
            SafeLogs.e(TAG, "parseJpegXmpWithUri()", e.getMessage());
        }

        // copy to local
        File tempFile = null;
        if (copyToLocal) {
            try (InputStream is = context.getContentResolver().openInputStream(uri)) {
                if (is == null) {
                    SafeLogs.e(TAG, "parseJpegXmpWithUri()", "openInputStream[2] is null");
                    return descriptor;
                }

                tempFile = DataManager.createTempFile("tmp-jmp-", ".jpeg");
                FileUtils.copyInputStreamToFile(is, tempFile);

                descriptor.tempJpegPath = tempFile.getAbsolutePath();
            } catch (Exception e) {
                SafeLogs.e(TAG, "parseJpegXmpWithUri() - copyInputStreamToFile", e.getMessage());
                return descriptor;
            }
        }


        int primaryIndex = getSpecialSemanticPosition(descriptor.items, Constants.MotionPhoto.PRIMARY);
        // not found primary semantic, it is not a motion photo
        if (primaryIndex == -1) {
            SafeLogs.e(TAG, "parseJpegXmpWithUri()", "primaryIndex = -1");
            descriptor.mpType = MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_NONE;
            return descriptor;
        }

        //
        descriptor.totalSize = queryUriLength(context, uri);
        applyJpegItemOffsets(descriptor, primaryLength);


        return descriptor;
    }

    public static MotionPhotoDescriptor parseJpegXmpWithFile(String filePath) {
        MotionPhotoDescriptor descriptor = new MotionPhotoDescriptor();
        try {
            if (TextUtils.isEmpty(filePath)) {
                return descriptor;
            }

            File file = new File(filePath);
            if (!file.exists()) {
                return descriptor;
            }

            // extract xmp
            descriptor = extractJpegXmp(file);

            if (!descriptor.isMotionPhoto()) {
                SafeLogs.e(TAG, "parseJpegXmpWithUri()", "not a motion photo");
                return descriptor;
            }


            int primaryIndex = getSpecialSemanticPosition(descriptor.items, Constants.MotionPhoto.PRIMARY);

            // not found primary semantic, it is not a motion photo
            if (primaryIndex == -1) {
                SafeLogs.e(TAG, "parseJpegXmpWithUri()", "primaryIndex = -1");
                descriptor.mpType = MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_NONE;
                return descriptor;
            }

            // calc primary jpeg length
            long primaryLength = calcPrimaryJpegLength(file);

            //
            descriptor.totalSize = file.length();
            applyJpegItemOffsets(descriptor, primaryLength);

        } catch (Exception e) {
            descriptor.mpType = MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_NONE;
            SafeLogs.e(TAG, "parseJpegXmpWithFile()", e.getMessage());
        }
        return descriptor;

    }

    private static long queryUriLength(Context context, Uri uri) {
        try (Cursor c = context.getContentResolver().query(uri, new String[]{OpenableColumns.SIZE}, null, null, null)) {
            if (c != null && c.moveToFirst()) {
                int idx = c.getColumnIndex(OpenableColumns.SIZE);
                if (idx >= 0) {
                    long s = c.getLong(idx);
                    if (s > 0) return s;
                }
            }
        } catch (Exception ignored) {
        }
        try (AssetFileDescriptor afd = context.getContentResolver().openAssetFileDescriptor(uri, "r")) {
            if (afd != null) {
                long s = afd.getLength();
                if (s > 0) return s;
            }
        } catch (Exception ignored) {
        }
        return -1L;
    }

    private static void applyJpegItemOffsets(MotionPhotoDescriptor descriptor, long primaryLength) {
        int primaryIndex = getSpecialSemanticPosition(descriptor.items, Constants.MotionPhoto.PRIMARY);
        int gainMapIndex = getSpecialSemanticPosition(descriptor.items, Constants.MotionPhoto.GAIN_MAP);
        int videoIndex = getSpecialSemanticPosition(descriptor.items, Constants.MotionPhoto.MOTION_PHOTO);
        if (primaryIndex == -1) {
            return;
        }

        descriptor.items.get(primaryIndex).length = primaryLength;
        descriptor.items.get(primaryIndex).offset = 0L;
        long primaryPadding = 0L;
        if (descriptor.items.get(primaryIndex).padding != null) {
            primaryPadding = descriptor.items.get(primaryIndex).padding;
        }
        if (gainMapIndex != -1 && primaryLength != -1) {
            descriptor.items.get(gainMapIndex).offset = primaryLength + primaryPadding;
        }
        if (videoIndex != -1) {
            if (gainMapIndex != -1) {
                MotionPhotoDescriptor.MotionPhotoItem gainItem = descriptor.items.get(gainMapIndex);
                long gainPadding = 0L;
                if (gainItem.padding != null) {
                    gainPadding = gainItem.padding;
                }
                descriptor.items.get(videoIndex).offset = gainItem.offset + gainItem.length + gainPadding;
            } else {
                descriptor.items.get(videoIndex).offset = primaryLength + primaryPadding;
            }
        }
    }

    public static int getSpecialSemanticPosition(List<MotionPhotoDescriptor.MotionPhotoItem> items, String semantic) {
        for (int i = 0; i < items.size(); i++) {
            String s = items.get(i).semantic;
            if (TextUtils.equals(semantic, s)) {
                return i;
            }
        }

        return -1;
    }

    private static long parseLongSafe(String str) {
        try {
            if (str == null || str.isEmpty()) return 0;
            return Long.parseLong(str);
        } catch (NumberFormatException e) {
            return 0;
        }
    }
}
