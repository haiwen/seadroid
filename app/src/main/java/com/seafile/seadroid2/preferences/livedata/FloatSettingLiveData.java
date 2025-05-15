package com.seafile.seadroid2.preferences.livedata;

import android.content.SharedPreferences;

import androidx.annotation.DimenRes;
import androidx.annotation.StringRes;
import androidx.core.content.res.ResourcesCompat;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.preferences.SettingsLiveData;

public class FloatSettingLiveData extends SettingsLiveData<Float> {
    public FloatSettingLiveData(@StringRes int keyRes, @DimenRes int defaultValueRes) {
        this(null, keyRes, defaultValueRes);
    }

    public FloatSettingLiveData(String nameSuffix, @StringRes int keyRes, @DimenRes int defaultValueRes) {
        this(nameSuffix, keyRes, null, defaultValueRes);
    }

    public FloatSettingLiveData(String nameSuffix, @StringRes int keyRes, String keySuffix, @DimenRes int defaultValueRes) {
        super(nameSuffix, keyRes, keySuffix, defaultValueRes);

        register();
    }

    @Override
    protected Float getDefaultValue(@DimenRes int defaultValueRes) {
        return ResourcesCompat.getFloat(SeadroidApplication.getAppContext().getResources(), defaultValueRes);
    }

    @Override
    protected Float getValue(SharedPreferences sharedPreferences, String key, Float defaultValue) {
        return sharedPreferences.getFloat(key, defaultValue);
    }

    @Override
    protected void putValue(SharedPreferences sharedPreferences, String key, Float value) {
        sharedPreferences.edit().putFloat(key, value).apply();
    }
}
