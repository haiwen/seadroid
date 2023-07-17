package com.seafile.seadroid2.util;

import android.bluetooth.le.AdvertiseData;
import android.bluetooth.le.AdvertiseSettings;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.SupportAccountManager;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Locale;
import java.util.UUID;

public class DeviceIdManager {
    private static volatile DeviceIdManager mSingleton = null;
    public static final String PREFIX = ".sd-";
    public final File LOCAL_FOLDER = SeadroidApplication.getAppContext().getExternalCacheDir();

    private DeviceIdManager() {

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
        File file = new File(LOCAL_FOLDER.getAbsolutePath() + "/" + PREFIX + uid.toLowerCase(Locale.getDefault()));
        FileUtils.createOrExistsFile(file);
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
