package com.seafile.seadroid2.framework.service.upload;

/**
 * Result of the resumable upload probe: how the current upload should start.
 */
public final class UploadResumeState {

    private static final String ACCEPT_RANGES_BYTES = "bytes";

    public enum Mode {
        /**
         * Chunked upload from offset 0. The server reported no stored bytes, so there is
         * no stale chunk temp file that could be appended to.
         */
        CHUNKED_FROM_ZERO,
        /**
         * Chunked upload resuming from the offset reported by the server.
         */
        CHUNKED_RESUME,
        /**
         * Upload the whole file in a single request. The persistent chunk temp file is
         * never used, so an unusable checkpoint cannot be appended to.
         */
        FULL_UPLOAD
    }

    public final Mode mode;
    /**
     * Number of bytes reported by the server, kept for logging.
     */
    public final long uploadedBytes;
    /**
     * Start offset used by the current upload. Only non-zero for {@link Mode#CHUNKED_RESUME}.
     */
    public final long startOffset;

    private UploadResumeState(Mode mode, long uploadedBytes, long startOffset) {
        this.mode = mode;
        this.uploadedBytes = uploadedBytes;
        this.startOffset = startOffset;
    }

    public static UploadResumeState chunkedFromZero(long uploadedBytes) {
        return new UploadResumeState(Mode.CHUNKED_FROM_ZERO, uploadedBytes, 0L);
    }

    public static UploadResumeState resume(long uploadedBytes) {
        return new UploadResumeState(Mode.CHUNKED_RESUME, uploadedBytes, uploadedBytes);
    }

    public static UploadResumeState fullUpload(long uploadedBytes) {
        return new UploadResumeState(Mode.FULL_UPLOAD, uploadedBytes, 0L);
    }

    /**
     * Computes how the upload should start from the probe result.
     *
     * <p>The upload resumes when the server supports resumable upload, the reported
     * offset is within the file and is greater than 0. An offset that is not usable
     * (greater than or equal to the file size) does not fall back to chunking from 0:
     * the file server rejects a Content-Range that does not match the length of the
     * existing temp file, so the caller uploads the file as a single request instead.</p>
     */
    public static UploadResumeState decide(boolean httpSuccessful,
                                           String acceptRangesHeader,
                                           Long uploadedBytes,
                                           long fileSize) {
        if (!httpSuccessful || !isBytesRangeSupported(acceptRangesHeader)) {
            // The server did not report a checkpoint; chunking from 0 cannot append to
            // an existing temp file that we know nothing about.
            return chunkedFromZero(0L);
        }

        long bytes = uploadedBytes == null ? 0L : Math.max(uploadedBytes, 0L);
        if (bytes == 0L) {
            return chunkedFromZero(0L);
        }

        if (fileSize > 0L && bytes < fileSize) {
            return resume(bytes);
        }

        return fullUpload(bytes);
    }

    private static boolean isBytesRangeSupported(String acceptRangesHeader) {
        return acceptRangesHeader != null
                && ACCEPT_RANGES_BYTES.equalsIgnoreCase(acceptRangesHeader.trim());
    }
}
