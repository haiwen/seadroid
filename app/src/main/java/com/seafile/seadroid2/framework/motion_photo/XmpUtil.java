package com.seafile.seadroid2.framework.motion_photo;

import java.nio.charset.StandardCharsets;

public final class XmpUtil {
    private XmpUtil(){}

    /**
     * Build or merge XMP packet as RDF string.
     * If original JPEG contains XMP packet, this attempts a conservative merge:
     * insert Container:Directory before closing </rdf:Description>.
     *
     * @param jpegBytes original jpeg bytes (may contain XMP)
     * @param videoLength mp4 payload length in bytes
     * @param padding mpvd header size (commonly 8)
     * @return full XMP packet string including <?xpacket ... ?> wrapper
     */
    public static String buildMotionXmpString(byte[] jpegBytes, int videoLength, int padding) {
        String existing = extractXmpPacketAsString(jpegBytes);
        String container =
                "<Container:Directory>\n" +
                        "  <rdf:Seq>\n" +
                        "    <rdf:li rdf:parseType=\"Resource\">\n" +
                        "      <Container:Item Item:Mime=\"image/heic\" Item:Semantic=\"Primary\" Item:Length=\"0\" Item:Padding=\"" + padding + "\"/>\n" +
                        "    </rdf:li>\n" +
                        "    <rdf:li rdf:parseType=\"Resource\">\n" +
                        "      <Container:Item Item:Mime=\"video/mp4\" Item:Semantic=\"MotionPhoto\" Item:Length=\"" + videoLength + "\"/>\n" +
                        "    </rdf:li>\n" +
                        "  </rdf:Seq>\n" +
                        "</Container:Directory>\n";

        if (existing == null) {
            return "<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n" +
                    "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmptk=\"Generated\">\n" +
                    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n" +
                    "    <rdf:Description rdf:about=\"\"\n" +
                    "      xmlns:hdrgm=\"http://ns.adobe.com/hdr-gain-map/1.0/\"\n" +
                    "      xmlns:GCamera=\"http://ns.google.com/photos/1.0/camera/\"\n" +
                    "      xmlns:Container=\"http://ns.google.com/photos/1.0/container/\"\n" +
                    "      xmlns:Item=\"http://ns.google.com/photos/1.0/container/item/\"\n" +
                    "      hdrgm:Version=\"1.0\"\n" +
                    "      GCamera:MotionPhoto=\"1\"\n" +
                    "      GCamera:MotionPhotoVersion=\"1\"\n" +
                    "      GCamera:MotionPhotoPresentationTimestampUs=\"0\">\n" +
                    container +
                    "    </rdf:Description>\n" +
                    "  </rdf:RDF>\n" +
                    "</x:xmpmeta>\n" +
                    "<?xpacket end=\"w\"?>";
        } else {
            int pos = existing.lastIndexOf("</rdf:Description>");
            if (pos < 0) {
                // fallback to fresh
                return buildMotionXmpString(null, videoLength, padding);
            }
            StringBuilder sb = new StringBuilder();
            sb.append(existing, 0, pos);
            sb.append(container);
            sb.append(existing.substring(pos));
            return sb.toString();
        }
    }

    private static String extractXmpPacketAsString(byte[] jpegBytes) {
        if (jpegBytes == null) return null;
        byte[] begin = "<?xpacket".getBytes(StandardCharsets.UTF_8);
        int start = indexOf(jpegBytes, begin, 0);
        if (start < 0) return null;
        byte[] endMarker = "</x:xmpmeta>".getBytes(StandardCharsets.UTF_8);
        int end = indexOf(jpegBytes, endMarker, start);
        if (end < 0) return null;
        int endPos = end + endMarker.length;
        return new String(jpegBytes, start, endPos - start, StandardCharsets.UTF_8);
    }

    private static int indexOf(byte[] data, byte[] pattern, int from) {
        outer:
        for (int i = from; i <= data.length - pattern.length; i++) {
            for (int j = 0; j < pattern.length; j++) {
                if (data[i+j] != pattern[j]) continue outer;
            }
            return i;
        }
        return -1;
    }
}