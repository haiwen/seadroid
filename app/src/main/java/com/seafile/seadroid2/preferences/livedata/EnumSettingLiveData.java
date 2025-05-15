package com.seafile.seadroid2.preferences.livedata;

import android.content.SharedPreferences;

import androidx.annotation.StringRes;
import androidx.core.content.res.ResourcesCompat;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.preferences.SettingsLiveData;

public class EnumSettingLiveData<E extends Enum<E>> extends SettingsLiveData<E> {

    private final E[] enumValues;

    public EnumSettingLiveData(Class<E> enumClass, @StringRes int keyRes, @StringRes int defaultValueRes) {
        this(enumClass, null, keyRes, defaultValueRes);
    }

    public EnumSettingLiveData(Class<E> enumClass, String nameSuffix, @StringRes int keyRes, @StringRes int defaultValueRes) {
        super(nameSuffix, keyRes, defaultValueRes);

        enumValues = enumClass.getEnumConstants();

        register();
    }

    @Override
    protected E getDefaultValue(@StringRes int defaultValueRes) {
        if (defaultValueRes != ResourcesCompat.ID_NULL) {
            String idStr = SeadroidApplication.getAppString(defaultValueRes);
            int id = Integer.parseInt(idStr);
            return enumValues[id];
        }
        return null;
    }

    @Override
    protected E getValue(SharedPreferences sharedPreferences, String key, E defaultValue) {
        String valueString = sharedPreferences.getString(key, null);
        if (valueString == null) {
            return defaultValue;
        }

        int valueOrdinal;
        try {
            valueOrdinal = Integer.parseInt(valueString);
        } catch (NumberFormatException e) {
            return defaultValue;
        }

        if (valueOrdinal >= 0 && valueOrdinal < enumValues.length) {
            return enumValues[valueOrdinal];
        } else {
            return defaultValue;
        }
    }

    @Override
    protected void putValue(SharedPreferences sharedPreferences, String key, E value) {
        sharedPreferences.edit().putString(key, String.valueOf(value.ordinal())).apply();
    }
}
