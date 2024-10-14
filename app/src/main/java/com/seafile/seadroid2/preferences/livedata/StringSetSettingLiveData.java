package com.seafile.seadroid2.preferences.livedata;

import android.content.SharedPreferences;

import androidx.annotation.ArrayRes;
import androidx.annotation.StringRes;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.preferences.SettingsLiveData;

import java.util.Set;

@Unstable
public class StringSetSettingLiveData extends SettingsLiveData<Set<String>> {

    public StringSetSettingLiveData(@StringRes int keyRes, @ArrayRes int defaultValueRes) {
        this(null, keyRes, defaultValueRes);
    }

    public StringSetSettingLiveData(String nameSuffix, @StringRes int keyRes, @ArrayRes int defaultValueRes) {
        this(nameSuffix, keyRes, null, defaultValueRes);
    }

    public StringSetSettingLiveData(String nameSuffix, @StringRes int keyRes, String keySuffix, @ArrayRes int defaultValueRes) {
        super(nameSuffix, keyRes, keySuffix, defaultValueRes);

        register();
    }

    @Override
    protected Set<String> getDefaultValue(@StringRes int defaultValueRes) {
        String[] arr = SeadroidApplication.getInstance().getResources().getStringArray(defaultValueRes);
        return Set.of(arr);
    }

    @Override
    protected Set<String> getValue(SharedPreferences sharedPreferences, String key, Set<String> defaultValue) {
        return sharedPreferences.getStringSet(key, defaultValue);
    }

    @Override
    protected void putValue(SharedPreferences sharedPreferences, String key, Set<String> value) {
        sharedPreferences.edit().putStringSet(key, value).apply();
    }
}
