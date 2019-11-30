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
        WAITING_UPLOAD_NUMBER("waiting_upload_number", 0),
        TOTAL_UPLOAD_NUMBER("total_upload_number", 0),
        PIC_CHECK_START("pic_check_start", 0),
        UPLOAD_COMPLETED_TIME("upload_completed_time", ""),
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
