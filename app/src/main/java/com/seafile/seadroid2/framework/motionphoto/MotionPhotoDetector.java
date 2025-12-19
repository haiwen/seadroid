package com.seafile.seadroid2.framework.motionphoto;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import com.seafile.seadroid2.framework.datastore.DataManager;

import org.apache.commons.io.FileUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

public final class MotionPhotoDetector {
    private MotionPhotoDetector() {
    }

    private static final byte[] XMP_HEADER_XAP = "http://ns.adobe.com/xap/1.0/\0".getBytes(StandardCharsets.US_ASCII);
    private static final byte[] XMP_HEADER_XMP = "http://ns.adobe.com/xmp/1.0/\0".getBytes(StandardCharsets.US_ASCII);

    public static final String RDF_NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String GCAMERA_NS = "http://ns.google.com/photos/1.0/camera/";
    public static final String CONTAINER_NS = "http://ns.google.com/photos/1.0/container/";
    public static final String ITEM_NS = "http://ns.google.com/photos/1.0/container/item/";

    /**
     * 从 InputStream 中提取 XMP 数据
     * <p>
     * 优化：不需要读取整个文件，只需顺序扫描头部标记。
     */
    public static String extractXmpFromStream(InputStream is) throws IOException {
        int b1 = is.read();
        int b2 = is.read();
        // 必须以 SOI (FF D8) 开始
        if (b1 != 0xFF || b2 != 0xD8) {
            return null;
        }

        while (true) {
            int b = is.read();
            if (b == -1) break;
            if (b != 0xFF) {
                break;
            }

            int marker = is.read();
            // 跳过填充的 0xFF
            while (marker == 0xFF) {
                marker = is.read();
            }
            if (marker == -1) break;

            // SOS (Start of Scan) 后面是图像数据，不再有 APP 段
            if (marker == 0xDA) {
                break;
            }

            // 读取长度 (2 bytes)
            int lenHi = is.read();
            int lenLo = is.read();
            if (lenHi == -1 || lenLo == -1) break;
            int length = (lenHi << 8) | lenLo;

            // 长度包含长度字节本身，所以数据长度需减 2
            int dataLength = length - 2;
            if (dataLength < 0) break;

            if (marker == 0xE1) { // APP1
                byte[] data = new byte[dataLength];
                int totalRead = 0;
                while (totalRead < dataLength) {
                    int count = is.read(data, totalRead, dataLength - totalRead);
                    if (count == -1) break;
                    totalRead += count;
                }

                if (totalRead < dataLength) break;

                if (startsWith(data, 0, XMP_HEADER_XMP)) {
                    int headerLen = XMP_HEADER_XMP.length;
                    return new String(data, headerLen, dataLength - headerLen, StandardCharsets.UTF_8);
                } else if (startsWith(data, 0, XMP_HEADER_XAP)) {
                    int headerLen = XMP_HEADER_XAP.length;
                    return new String(data, headerLen, dataLength - headerLen, StandardCharsets.UTF_8);
                }
            } else {
                // 跳过其他 marker 的数据
                long skipped = 0;
                while (skipped < dataLength) {
                    long s = is.skip(dataLength - skipped);
                    if (s > 0) {
                        skipped += s;
                    } else {
                        // skip 失败尝试读取
                        if (is.read() == -1) break;
                        skipped++;
                    }
                }
            }
        }
        return null;
    }

    public static String extractXmpFromJpeg(byte[] jpeg) {

        int offset = 0;

        // JPEG 必须以 SOI 开始
        if ((jpeg[0] & 0xFF) != 0xFF || (jpeg[1] & 0xFF) != 0xD8) {
            return null;
        }
        offset = 2;

        while (offset + 4 < jpeg.length) {
            if ((jpeg[offset] & 0xFF) != 0xFF) {
                break;
            }

            int marker = jpeg[offset + 1] & 0xFF;
            offset += 2;

            // SOS 后面是图像数据，不再有 APP 段
            if (marker == 0xDA /* SOS */) {
                break;
            }

            int length = ((jpeg[offset] & 0xFF) << 8)
                    | (jpeg[offset + 1] & 0xFF);
            offset += 2;

            if (length < 2 || offset + length - 2 > jpeg.length) {
                break;
            }

            if (marker == 0xE1 /* APP1 */) {
                int headerLen = -1;
                if (startsWith(jpeg, offset, XMP_HEADER_XMP)) {
                    headerLen = XMP_HEADER_XMP.length;
                } else if (startsWith(jpeg, offset, XMP_HEADER_XAP)) {
                    headerLen = XMP_HEADER_XAP.length;
                }
                if (headerLen > 0) {
                    int xmpStart = offset + headerLen;
                    int xmpLength = length - 2 - headerLen;
                    if (xmpLength > 0) {
                        return new String(
                                jpeg,
                                xmpStart,
                                xmpLength,
                                StandardCharsets.UTF_8
                        );
                    }
                }
            }

            offset += length - 2;
        }

        return null;
    }

    private static boolean startsWith(byte[] data, int offset, byte[] prefix) {
        if (offset + prefix.length > data.length) return false;
        for (int i = 0; i < prefix.length; i++) {
            if (data[offset + i] != prefix[i]) return false;
        }
        return true;
    }

    public static MotionPhotoDescriptor parseMotionPhotoXmpWithUri(Context context, Uri uri, boolean copyToLocal) {
        MotionPhotoDescriptor defaultDesc = new MotionPhotoDescriptor();

        // 1. 第一遍：流式解析 XMP，避免将整个文件读入内存
        String xmp = null;
        try (InputStream is = context.getContentResolver().openInputStream(uri)) {
            if (is == null) {
                return defaultDesc;
            }
            xmp = extractXmpFromStream(is);
        } catch (Exception e) {
            e.printStackTrace();
            return defaultDesc;
        }

        if (TextUtils.isEmpty(xmp)) {
            return defaultDesc;
        }

        // 2. 解析 Descriptor
        MotionPhotoDescriptor d;
        try {
            d = parse(xmp);
        } catch (Exception e) {
            e.printStackTrace();
            return defaultDesc;
        }

        if (copyToLocal) {
            // 3. 只有当确实是 Motion Photo 且解析成功时，才将文件保存到临时文件
            // 重新打开流以进行复制
            try (InputStream is = context.getContentResolver().openInputStream(uri)) {
                if (is != null) {
                    File tempFile = DataManager.createTempFile("jpeg-", ".jpeg");
                    FileUtils.copyInputStreamToFile(is, tempFile);
                    d.tempJpegPath = tempFile.getAbsolutePath();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }


        return d;
    }


    public static MotionPhotoDescriptor parseMotionPhotoXmpWithFilePath(String filePath) {
        try {
            MotionPhotoDescriptor defaultDesc = new MotionPhotoDescriptor();
            if (TextUtils.isEmpty(filePath)) {
                return defaultDesc;
            }

            File f = new File(filePath);
            if (!f.exists()) {
                return defaultDesc;
            }

            byte[] fileBytes = FileUtils.readFileToByteArray(f);
            String xmp = extractXmpFromJpeg(fileBytes);
            if (TextUtils.isEmpty(xmp)) {
                return defaultDesc;
            }

            MotionPhotoDescriptor d = parse(xmp);
            return d;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

    }

    /**
     * 解析 Motion Photo XMP
     *
     * @param xmpString XMP 字符串
     * @return MotionPhotoDescriptor 对象
     * @throws Exception
     */
    public static MotionPhotoDescriptor parse(String xmpString) throws Exception {
        MotionPhotoDescriptor descriptor = new MotionPhotoDescriptor();

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document doc = builder.parse(new ByteArrayInputStream(xmpString.getBytes(StandardCharsets.UTF_8)));

        Element rdfDescription = (Element) doc.getElementsByTagNameNS(RDF_NS, "Description").item(0);

        if (rdfDescription != null) {
            String microVideoStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MicroVideo");
            if (TextUtils.equals("1", microVideoStr)) {
                descriptor.source = MotionPhotoDescriptor.Source.MICRO_VIDEO;
                descriptor.isMotionPhoto = true;

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

            String motionPhotoStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MotionPhoto");
            descriptor.isMotionPhoto = TextUtils.equals("1", motionPhotoStr);
            if (!descriptor.isMotionPhoto) {
                descriptor.source = MotionPhotoDescriptor.Source.CONTAINER;
                return descriptor;
            }

            String motionPhotoVerStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MotionPhotoVersion");
            if (!motionPhotoVerStr.isEmpty()) {
                descriptor.motionPhotoVersion = Integer.parseInt(motionPhotoVerStr);
            }

            String presentationTsStr = rdfDescription.getAttributeNS(GCAMERA_NS, "MotionPhotoPresentationTimestampUs");
            if (!presentationTsStr.isEmpty()) {
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
        }

        return descriptor;
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
