package com.seafile.seadroid2.config;

import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.BuildConfig;

public class Constants {
    private Constants() {
        throw new IllegalStateException("Constants class"); //NON-NLS(1)
    }

    public static final String URL_PRIVACY = "https://www.seafile.com/privacy/";

    public static class UA {
        private UA() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        public static final String SEAFILE_UA = "Seafile/3.0";
        public static final String SEAFILE_ANDROID_UA = "Seafile Android/3.0";
        public static final String SEAFILE_ANDROID_DOWNLOAD_UA = "Seafile Android Downloader/3.0";
        public static final String SEAFILE_ANDROID_UPLOAD_UA = "Seafile Android Uploader/3.0";
    }


    public static class Account {
        private Account() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        /**
         * Type of the account (currently there is only one type)
         */
        public final static String ACCOUNT_TYPE = BuildConfig.ACCOUNT_TYPE;
    }


    public static class Protocol {
        private Protocol() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        public static final String HTTPS = "https://";
        public static final String HTTP = "http://";
    }

    public static class FileExtensions {
        private FileExtensions() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        public static final String SDOC = "sdoc";
        public static final String DOT_SDOC = ".sdoc";
        public static final String DOT_EXDRAW = ".exdraw";
        public static final String DOT_DRAW = ".draw";
        public static final String DOT_DOC = ".doc";
        public static final String DOT_DOCX = ".docx";
        public static final String DOT_ODT = ".odt";
        public static final String DOT_XLSX = ".xlsx";
        public static final String DOT_XLS = ".xls";
        public static final String DOT_ODS = ".ods";
        public static final String DOT_PPT = ".ppt";
        public static final String DOT_PPTX = ".pptx";
        public static final String DOT_PPSX = ".ppsx";
        public static final String DOT_ODP = ".odp";
    }

    public static class DP {
        private DP() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        public static final int DP_2 = SizeUtils.dp2px(2);
        public static final int DP_4 = SizeUtils.dp2px(4);
        public static final int DP_8 = SizeUtils.dp2px(8);
        public static final int DP_16 = SizeUtils.dp2px(16);
        public static final int DP_32 = SizeUtils.dp2px(32);
        public static final int DP_128 = SizeUtils.dp2px(128);
        public static final int DP_160 = SizeUtils.dp2px(160);
    }

    /**
     * <pre>
     * {@code
     * data status
     * 0: normal,
     * -1: deleted,
     * -2: hidden,
     * -3: locked,
     * -4: locked and hidden.
     * }
     * </pre>
     */
    public static class DataStatus {
        public static final int NORMAL = 0;
        public static final int DELETED = -1;
        public static final int HIDDEN = -2;
        public static final int LOCKED = -3;
        public static final int LOCKED_AND_HIDDEN = -4;
    }

    public static class MotionPhoto {
        public static final String MP_TYPE_JPEG = "jpeg";
        public static final String MP_TYPE_HEIC = "heic";
        public static final String PRIMARY = "Primary";
        public static final String GAIN_MAP = "GainMap";
        public static final String MOTION_PHOTO = "MotionPhoto";
    }
}
