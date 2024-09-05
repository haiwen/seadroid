package com.seafile.seadroid2.preferences.livedata;

import android.content.SharedPreferences;
import android.content.res.Resources;

import androidx.annotation.StringRes;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.preferences.SettingsLiveData;

public class StringSettingLiveData extends SettingsLiveData<String> {
    public StringSettingLiveData(@StringRes int keyRes, @StringRes int defaultValueRes) {
        this(null, keyRes, defaultValueRes);
    }

    public StringSettingLiveData(String nameSuffix, @StringRes int keyRes, @StringRes int defaultValueRes) {
        super(nameSuffix, keyRes, defaultValueRes);

        register();
    }

    @Override
    protected String getDefaultValue(@StringRes int defaultValueRes) {
        if (defaultValueRes == Resources.ID_NULL) {
            return null;
        }
        return SeadroidApplication.getInstance().getString(defaultValueRes);
    }

    @Override
    protected String getValue(SharedPreferences sharedPreferences, String key, String defaultValue) {
        return sharedPreferences.getString(key, defaultValue);
    }

    @Override
    protected void putValue(SharedPreferences sharedPreferences, String key, String value) {
        sharedPreferences.edit().putString(key, value).apply();
    }
}
