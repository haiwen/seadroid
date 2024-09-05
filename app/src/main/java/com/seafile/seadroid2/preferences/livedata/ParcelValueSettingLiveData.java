package com.seafile.seadroid2.preferences.livedata;

import android.content.SharedPreferences;
import android.os.Parcel;
import android.text.TextUtils;

import androidx.annotation.StringRes;

import com.google.android.gms.common.util.Base64Utils;
import com.seafile.seadroid2.preferences.SettingsLiveData;

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
            byte[] bs = Base64Utils.decode(string);
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
            String p = Base64Utils.encode(bs);
            sharedPreferences.edit().putString(key, p).apply();
        } finally {
            parcel.recycle();
        }
    }
}
