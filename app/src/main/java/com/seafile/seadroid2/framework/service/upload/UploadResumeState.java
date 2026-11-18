package com.seafile.seadroid2.framework.service.upload;

/**
 * Result of the resumable upload capability probe and the start offset for the current upload.
 */
public final class UploadResumeState {

    private static final String ACCEPT_RANGES_BYTES = "bytes";

    /**
     * Whether the server supports resumable upload
     * (the file-uploaded-bytes response header contains "Accept-Ranges: bytes").
     */
    public final boolean supported;
    /**
     * Number of bytes already stored on the server.
     */
    public final long uploadedBytes;
    /**
     * Start offset actually used by the current upload.
     */
    public final long startOffset;
    /**
     * Whether the upload really resumes from a checkpoint (startOffset > 0).
     */
    public final boolean resumable;

    private UploadResumeState(boolean supported, long uploadedBytes, long startOffset, boolean resumable) {
        this.supported = supported;
        this.uploadedBytes = uploadedBytes;
        this.startOffset = startOffset;
        this.resumable = resumable;
    }

    public static UploadResumeState unsupported() {
        return new UploadResumeState(false, 0L, 0L, false);
    }

    /**
     * Computes the start offset for the current upload from the probe result.
     *
     * <p>The upload resumes from the checkpoint only when the server supports resumable upload,
     * the uploaded bytes are greater than 0 and less than the total file size. Otherwise
     * (not supported, first upload, or invalid offset) it starts from 0 and keeps the original flow.</p>
     */
    public static UploadResumeState decide(boolean httpSuccessful,
                                           String acceptRangesHeader,
                                           Long uploadedBytes,
                                           long fileSize) {
        if (!httpSuccessful || !isBytesRangeSupported(acceptRangesHeader)) {
            return unsupported();
        }

        long bytes = uploadedBytes == null ? 0L : uploadedBytes;
        if (bytes <= 0L) {
            return new UploadResumeState(true, Math.max(bytes, 0L), 0L, false);
        }

        // Fall back to a full upload when the server offset is unreliable (greater than or equal to the file size)
        if (fileSize <= 0L || bytes >= fileSize) {
            return new UploadResumeState(true, bytes, 0L, false);
        }

        return new UploadResumeState(true, bytes, bytes, true);
    }

    private static boolean isBytesRangeSupported(String acceptRangesHeader) {
        return acceptRangesHeader != null
                && ACCEPT_RANGES_BYTES.equalsIgnoreCase(acceptRangesHeader.trim());
    }
}
