package com.seafile.seadroid2.preferences.livedata;

import android.content.SharedPreferences;
import android.os.Parcel;
import android.text.TextUtils;

import androidx.annotation.StringRes;

import com.blankj.utilcode.util.EncodeUtils;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.preferences.SettingsLiveData;

@Unstable
public class ParcelValueSettingLiveData<T> extends SettingsLiveData<T> {

    private final T defaultValue;

    public ParcelValueSettingLiveData(@StringRes int keyRes, T defaultValue) {
        this(null, keyRes, defaultValue);
    }

    public ParcelValueSettingLiveData(String nameSuffix, @StringRes int keyRes, T defaultValue) {
        this(nameSuffix, keyRes, null, defaultValue);
    }

    public ParcelValueSettingLiveData(String nameSuffix, @StringRes int keyRes, String keySuffix, T defaultValue) {
        super(nameSuffix, keyRes, keySuffix, 0);

        this.defaultValue = defaultValue;

        register();
    }

    @Override
    protected T getDefaultValue(int defaultValueRes) {
        return defaultValue;
    }

    @Override
    protected T getValue(SharedPreferences sharedPreferences, String key, T defaultValue) {
        String string = sharedPreferences.getString(key, null);
        if (TextUtils.isEmpty(string)) {
            return defaultValue;
        }

        Parcel parcel = Parcel.obtain();
        try {
            byte[] bs = EncodeUtils.base64Encode(string);
            parcel.unmarshall(bs, 0, bs.length);
            parcel.setDataPosition(0);

            T t = (T) parcel.readValue(getClass().getClassLoader());
            if (t == null) {
                return defaultValue;
            }

            return t;
        } finally {
            parcel.recycle();
        }

    }

    @Override
    protected void putValue(SharedPreferences sharedPreferences, String key, T value) {

        Parcel parcel = Parcel.obtain();
        try {
            parcel.writeValue(value);
            byte[] bs = parcel.marshall();
            String p = EncodeUtils.base64Encode2String(bs);
            sharedPreferences.edit().putString(key, p).apply();
        } finally {
            parcel.recycle();
        }
    }
}
