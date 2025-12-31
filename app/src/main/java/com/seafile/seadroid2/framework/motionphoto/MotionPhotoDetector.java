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
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

@Todo
public final class MotionPhotoDetector {
    private MotionPhotoDetector() {
    }

    private static final byte[] XMP_HEADER_XAP = "http://ns.adobe.com/xap/1.0/\0".getBytes(StandardCharsets.US_ASCII);
    private static final byte[] XMP_HEADER_XMP = "http://ns.adobe.com/xmp/1.0/\0".getBytes(StandardCharsets.US_ASCII);

    public static final String RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String GCAMERA_NS = "http://ns.google.com/photos/1.0/camera/";
    public static final String CONTAINER_NS = "http://ns.google.com/photos/1.0/container/";
    public static final String ITEM_NS = "http://ns.google.com/photos/1.0/container/item/";

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

    /**
     * 根据输入流，补全“读取 JPEG 图像数据的实际长度”方法
     *
     */
    private static long calcuRealJpegLength(InputStream is) throws IOException {
        long totalBytesRead = 0L;
        int b1 = is.read();
        int b2 = is.read();
        if (b1 != 0xFF || b2 != 0xD8) {
            return -1;
        }
        totalBytesRead += 2;

        while (true) {
            int b = is.read();
            if (b == -1) break;
            totalBytesRead++;

            if (b != 0xFF) {
                continue;
            }

            int marker = is.read();
            if (marker == -1) break;
            totalBytesRead++;

            while (marker == 0xFF) {
                marker = is.read();
                if (marker == -1) break;
                totalBytesRead++;
            }
            if (marker == -1) break;

            if (marker == 0xDA) {
                while (true) {
                    int d = is.read();
                    if (d == -1) break;
                    totalBytesRead++;

                    if (d == 0xFF) {
                        int m = is.read();
                        if (m == -1) break;
                        totalBytesRead++;
                        if (m == 0xD9) {
                            return totalBytesRead;
                        }
                    }
                }
                break;
            }

            int lenHi = is.read();
            int lenLo = is.read();
            if (lenHi == -1 || lenLo == -1) break;
            totalBytesRead += 2;
            int length = (lenHi << 8) | lenLo;

            int dataLength = length - 2;
            if (dataLength <= 0) continue;

            long skipped = 0;
            while (skipped < dataLength) {
                long s = is.skip(dataLength - skipped);
                if (s > 0) {
                    skipped += s;
                } else {
                    int readOne = is.read();
                    if (readOne == -1) break;
                    skipped++;
                }
            }
            totalBytesRead += skipped;
        }

        return -1;
    }

    private static long calcuRealJpegLength(byte[] jpegBytes) {
        if (jpegBytes == null || jpegBytes.length < 2 || (jpegBytes[0] & 0xFF) != 0xFF || (jpegBytes[1] & 0xFF) != 0xD8) {
            return -1;
        }

        int offset = 2;
        while (offset < jpegBytes.length) {
            if ((jpegBytes[offset] & 0xFF) != 0xFF) {
                offset++;
                continue;
            }

            while (offset < jpegBytes.length && (jpegBytes[offset] & 0xFF) == 0xFF) {
                offset++;
            }
            if (offset >= jpegBytes.length) break;

            int marker = jpegBytes[offset] & 0xFF;
            offset++;

            if (marker == 0xDA) {
                while (offset + 1 < jpegBytes.length) {
                    if ((jpegBytes[offset] & 0xFF) == 0xFF && (jpegBytes[offset + 1] & 0xFF) == 0xD9) {
                        return offset + 2;
                    }
                    offset++;
                }
                break;
            }

            if (offset + 2 > jpegBytes.length) break;
            int length = ((jpegBytes[offset] & 0xFF) << 8) | (jpegBytes[offset + 1] & 0xFF);
            offset += 2;
            int dataLength = length - 2;
            if (dataLength < 0) break;
            offset += dataLength;
        }

        return -1;

    }

    private static boolean startsWith(byte[] data, int offset, byte[] prefix) {
        if (offset + prefix.length > data.length) return false;
        for (int i = 0; i < prefix.length; i++) {
            if (data[offset + i] != prefix[i]) return false;
        }
        return true;
    }

    private static String getXmpPropertyStringSafe(XMPMeta xmpMeta, String namespace, String propertyName) {
        try {
            return xmpMeta.getPropertyString(namespace, propertyName);
        } catch (XMPException e) {
            return null;
        }
    }


    public static MotionPhotoDescriptor parseJpegXmpWithUri(Context context, Uri uri, boolean copyToLocal) {
        MotionPhotoDescriptor defaultDesc = new MotionPhotoDescriptor();
        long size = queryUriLength(context, uri);
        final long MAX_IN_MEMORY = 20L * 1024L * 1024L;
        MotionPhotoDescriptor d = new MotionPhotoDescriptor();
        long realLength = -1;
        File tempFile = null;
        if (size > MAX_IN_MEMORY) {
            try (InputStream is = context.getContentResolver().openInputStream(uri)) {
                if (is == null) {
                    return defaultDesc;
                }
                tempFile = DataManager.createTempFile("tmp-jmp-", ".jpeg");
                FileUtils.copyInputStreamToFile(is, tempFile);
                d = extractJpegXmp(tempFile);
                realLength = calcuRealJpegLength(new FileInputStream(tempFile));

            } catch (Exception e) {
                SafeLogs.e(e);
                return defaultDesc;
            }
        } else {
            byte[] bytes;
            try (InputStream is = context.getContentResolver().openInputStream(uri)) {
                if (is == null) {
                    return defaultDesc;
                }
                bytes = IOUtils.toByteArray(is);
                d = extractJpegXmp(bytes);
                realLength = calcuRealJpegLength(bytes);
            } catch (Exception e) {
                SafeLogs.e(e);
                return defaultDesc;
            }
        }

        if (d.mpType == MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_NONE) {
            return defaultDesc;
        }

        if (d.source == MotionPhotoDescriptor.Source.CONTAINER) {
            return d;
        }

        if (copyToLocal) {
            try {
                if (tempFile == null) {
                    try (InputStream is = context.getContentResolver().openInputStream(uri)) {
                        if (is == null) {
                            return defaultDesc;
                        }
                        tempFile = DataManager.createTempFile("tmp-jmp-", ".jpeg");
                        FileUtils.copyInputStreamToFile(is, tempFile);
                    }
                }
                d.tempJpegPath = tempFile.getAbsolutePath();

                int primaryIndex = getSpecialSemanticPosition(d.items, "Primary");
                int gainMapIndex = getSpecialSemanticPosition(d.items, "GainMap");
                int videoIndex = getSpecialSemanticPosition(d.items, "MotionPhoto");

                if (primaryIndex == -1) {
                    return d;
                }

                d.items.get(primaryIndex).length = realLength;
                d.items.get(primaryIndex).offset = 0L;

                if (gainMapIndex != -1 && realLength != -1) {
                    long padding = d.items.get(gainMapIndex).padding;
                    d.items.get(gainMapIndex).offset = padding + realLength;
                }

                long fileLength = tempFile.length();
                if (videoIndex != -1) {
                    long length = d.items.get(videoIndex).length;
                    d.items.get(videoIndex).offset = fileLength - length;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return d;
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


    public static MotionPhotoDescriptor parseJpegXmpWithFile(String filePath) {
        try {
            MotionPhotoDescriptor desc = new MotionPhotoDescriptor();
            if (TextUtils.isEmpty(filePath)) {
                return desc;
            }

            File f = new File(filePath);
            if (!f.exists()) {
                return desc;
            }

            desc = extractJpegXmp(f);

            long realLength = calcuRealJpegLength(new FileInputStream(f));

            if (desc.mpType == MotionPhotoDescriptor.MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_NONE) {
                return desc;
            }

            if (desc.source == MotionPhotoDescriptor.Source.CONTAINER) {
                return desc;
            }


            // re-calculate
            int primaryIndex = getSpecialSemanticPosition(desc.items, "Primary");
            int gainMapIndex = getSpecialSemanticPosition(desc.items, "GainMap");
            int videoIndex = getSpecialSemanticPosition(desc.items, "MotionPhoto");

            if (primaryIndex == -1) {
                return desc;
            }

            //
            desc.items.get(primaryIndex).length = 0L;
            desc.items.get(primaryIndex).offset = 0L;

            // gain map: add a jpeg offset into Padding field
            if (gainMapIndex != -1) {
                long padding = desc.items.get(gainMapIndex).padding;
                desc.items.get(gainMapIndex).offset = padding + realLength;
            }

            long fileLength = f.length();
            if (videoIndex != -1) {
                long length = desc.items.get(videoIndex).length;
                desc.items.get(videoIndex).offset = fileLength - length;
            }

            return desc;
        } catch (Exception e) {
            throw new RuntimeException(e);
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
