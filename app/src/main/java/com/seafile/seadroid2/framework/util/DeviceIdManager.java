package com.seafile.seadroid2.framework.util;

import android.os.Environment;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.SupportAccountManager;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Locale;

public class DeviceIdManager {
    private static volatile DeviceIdManager mSingleton = null;
    public static final String PREFIX = ".sd-";
    public File LOCAL_FOLDER;

    private DeviceIdManager() {
        if (Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState())) {
            LOCAL_FOLDER = SeadroidApplication.getAppContext().getExternalCacheDir();
        }

        if (LOCAL_FOLDER == null) {
            LOCAL_FOLDER = SeadroidApplication.getAppContext().getCacheDir();
        }
    }

    public static DeviceIdManager getInstance() {
        if (mSingleton == null) {
            synchronized (DeviceIdManager.class) {
                if (mSingleton == null) {
                    mSingleton = new DeviceIdManager();
                }
            }
        }
        return mSingleton;
    }

    public String getOrSet() {
        File stFile = checkLocalDeviceIdFileExistsState();
        if (stFile != null) {
            String name = stFile.getName().replace(PREFIX, "");
            return name.substring(0, 16);
        }

        String salt;
        if (SupportAccountManager.getInstance().getCurrentAccount() != null) {
            salt = SupportAccountManager.getInstance().getCurrentAccount().email;
        } else {
            salt = TimeUtils.getNowString();
        }
        String uid = EncryptUtils.encryptMD5ToString(("" + TimeUtils.getNowMills()), salt);
        if (LOCAL_FOLDER != null) {
            File file = new File(LOCAL_FOLDER.getAbsolutePath() + "/" + PREFIX + uid.toLowerCase(Locale.getDefault()));
            FileUtils.createOrExistsFile(file);
        }
        return uid.toLowerCase(Locale.getDefault()).substring(0, 16);

//        String uid = UUID.randomUUID().toString().replace("-", "");
//        File file = new File(LOCAL_FOLDER.getAbsolutePath() + "/" + PREFIX + uid);
//        FileUtils.createOrExistsFile(file);
//        return uid.substring(0, 16);
    }

    public File checkLocalDeviceIdFileExistsState() {
        if (LOCAL_FOLDER == null) {
            return null;
        }

        File[] temp = LOCAL_FOLDER.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.startsWith(PREFIX);
            }
        });

        if (temp == null) {
            return null;
        }

        if (temp.length == 1) {
            return temp[0];
        }

        for (File file : temp) {
            file.delete();
        }
        return null;
    }
}
