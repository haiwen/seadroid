package com.seafile.seadroid2.util;

import android.content.Context;

public class SharedSystemSetXml extends SharedPreferencesUtil {

    public SharedSystemSetXml() {
        super("MY_SYSTEM_SET");
    }

    /**
     * @param context
     * @param type
     * @return
     */
    public boolean putData(Context context, Type type) {
        return putData(context, type.getKey(), type.getDefult());
    }

    /**
     * @param fileName
     * @param context
     * @param type
     * @return
     */
    public Object getData(Context context, Type type) {
        return getData(context, type.getKey(), type.getDefult());
    }

    public enum Type {
        SEAFILE_UPLOAD_NUMBER("seafile_upload_number", 0),
        PIC_CHECK_START("pic_check_start", 0),
        ;

        Type(String key, Object defult) {
            this.key = key;
            this.defult = defult;
        }

        String key;

        Object defult;

        public String getKey() {
            return key;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public Object getDefult() {
            return defult;
        }

        public void setDefult(Object defult) {
            this.defult = defult;
        }
    }
}
