package com.seafile.seadroid2.config;

import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.BuildConfig;

public class Constants {
    private Constants() {
        throw new IllegalStateException("Constants class"); //NON-NLS(1)
    }

    public static final int PASSWORD_MINIMUM_LENGTH = 4;
    public static final String URL_PRIVACY = "https://www.seafile.com/privacy/";

    public static class App {
        private App() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        /**
         * When the app version is upgraded to v3.0.0(or v3x), some data must be migrated,
         * such as. CameraUploadDBHelper/FolderBackupDBHelper.
         * This field is used to check if it has been migrated.
         * <p>
         * 0 no
         * 1 yes
         * <p/>
         */
        public static final String DATA_IS_MIGRATION = "data_is_migrated_when_app_version_is_v3x";
    }


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

    public static class SP {
        private SP() {
            throw new IllegalStateException("Don't instantiate this class");
        }

        public static final String ACCOUNT_CURRENT = "latest_account";
        public static final String ACCOUNT_NAME = "com.seafile.seadroid..account_name";

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
}
