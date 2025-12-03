package com.seafile.seadroid2.framework.motion_photo;

import com.adobe.internal.xmp.options.SerializeOptions;
import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.XMPMetaFactory;

import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Extract / modify / rewrite XMP inside a HEIC file using Adobe XMP Core.
 */
public final class XmpMotionPhotoHelper {

    private static final String GCAMERA_NS = "http://ns.google.com/photos/1.0/camera/";
    private static final String GCAMERA_PREFIX = "GCamera";

    private static final Pattern XMP_PATTERN = Pattern.compile(
            "(<x:xmpmeta[\\s\\S]*?</x:xmpmeta>)",
            Pattern.CASE_INSENSITIVE
    );

    /**
     * Extract XMP string from HEIC bytes
     */
    public static String extractXmp(byte[] heicBytes) throws XMPException {
        String content = new String(heicBytes, StandardCharsets.UTF_8);
        Matcher matcher = XMP_PATTERN.matcher(content);
        if (!matcher.find()) {
            throw new XMPException("XMP packet not found in HEIC file", -1);
        }
        return matcher.group(1);
    }

    /**
     * Modify XMP: set GCamera:MotionPhotoPadding to given value (Google requirement)
     */
    public static String updatePadding(String xmpPacket, int paddingBytes) throws XMPException {
        XMPMeta xmp = XMPMetaFactory.parseFromString(xmpPacket);

        // register namespace
        XMPMetaFactory.getSchemaRegistry().registerNamespace(GCAMERA_NS, GCAMERA_PREFIX + ":");

        // set property
        xmp.setPropertyInteger(GCAMERA_NS, "MotionPhotoPadding", paddingBytes);

        // also ensure MotionPhoto = 1
        xmp.setPropertyInteger(GCAMERA_NS, "MotionPhoto", 1);

        // optionally: MotionPhotoVersion (google uses 1)
        xmp.setPropertyInteger(GCAMERA_NS, "MotionPhotoVersion", 1);

        // serialize back to string
        SerializeOptions opts = new SerializeOptions();
        opts.setUseCompactFormat(true);
        opts.setOmitPacketWrapper(false);

        return XMPMetaFactory.serializeToString(xmp, opts);
    }

    /**
     * Replace original XMP inside HEIC bytes with updated XMP
     */
    public static byte[] replaceXmp(byte[] heicBytes, String newXmp) {
        String file = new String(heicBytes, StandardCharsets.UTF_8);
        return XMP_PATTERN.matcher(file)
                .replaceFirst(Matcher.quoteReplacement(newXmp))
                .getBytes(StandardCharsets.UTF_8);
    }
}
