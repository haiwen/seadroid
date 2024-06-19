package com.seafile.seadroid2.config;

import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.BuildConfig;

public class Constants {
    private Constants() {
        throw new IllegalStateException("Constants class"); //NON-NLS(1)
    }

    public static final String APP_PACKAGE = "com.seafile.seadroid2";


    public static final int PERIODIC_SCAN_INTERVALS = 1000 * 60 * 30; //30 mins
    public static final int PASSWORD_MINIMUM_LENGTH = 8;
    public static final String URL_PRIVACY = "https://www.seafile.com/privacy/";

    public static class ObjType {
        private ObjType() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        public static final String REPO = "repo";
        public static final String DIR = "dir";
        public static final String FILE = "file";
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

    public static class Format {
        private Format() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        public static final String SDOC = "sdoc";
        public static final String DOT_SDOC = ".sdoc";
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
}
