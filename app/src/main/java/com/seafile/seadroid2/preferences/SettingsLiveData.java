package com.seafile.seadroid2.preferences;

import android.content.SharedPreferences;

import androidx.annotation.AnyRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.Observer;

import com.kunminx.architecture.domain.message.MutableResult;
import com.kunminx.architecture.ui.callback.ProtectedUnPeekLiveData;
import com.seafile.seadroid2.SeadroidApplication;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

public abstract class SettingsLiveData<T> extends ProtectedUnPeekLiveData<T> implements SharedPreferences.OnSharedPreferenceChangeListener {
    private final String nameSuffix;
    private final String keySuffix;
    private final int keyRes;
    private final int defaultValueRes;

    public SettingsLiveData(@StringRes int keyRes, @AnyRes int defaultValueRes) {
        this(null, keyRes, null, defaultValueRes);
    }

    public SettingsLiveData(String nameSuffix, @StringRes int keyRes, @AnyRes int defaultValueRes) {
        this(nameSuffix, keyRes, null, defaultValueRes);
    }

    public SettingsLiveData(String nameSuffix, @StringRes int keyRes, String keySuffix, @AnyRes int defaultValueRes) {
        this.nameSuffix = nameSuffix;
        this.keySuffix = keySuffix;
        this.keyRes = keyRes;
        this.defaultValueRes = defaultValueRes;
    }

    private SharedPreferences sharedPreferences = null;
    private String key = null;
    private T defaultValue = null;

    protected void register() {
        sharedPreferences = SharedPreferencesHelper.getSharedPreferences(nameSuffix);
        key = getKey(keyRes, keySuffix);
        defaultValue = getDefaultValue(defaultValueRes);

//        loadValue();

        // Only a weak reference is stored so we don't need to worry about unregistering.
        sharedPreferences.registerOnSharedPreferenceChangeListener(this);
    }

    protected void unregister() {
        sharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
    }

    protected abstract T getDefaultValue(@AnyRes int defaultValueRes);

    private String getKey(@StringRes int keyRes, String keySuffix) {
        String key = SeadroidApplication.getInstance().getString(keyRes);
        return keySuffix != null ? key + "_" + keySuffix : key;
    }

    /**
     * remove key-value
     */
    public void remove() {
        sharedPreferences.edit().remove(key).apply();
    }

    public void putValue(T value) {
        putValue(sharedPreferences, key, value);
    }

    protected abstract void putValue(SharedPreferences sharedPreferences, String key, T value);

    @Override
    public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, @Nullable String key) {
        if (Objects.equals(key, this.key)) {
            loadValue();
        }
    }


    private void loadValue() {
        T t = getValue(sharedPreferences, key, (T) defaultValue);

        setValue(t);
    }

    /**
     * Used for cases where T is Void, to make calls cleaner.
     */
    public void call() {
        setValue(null);
    }

    public T queryValue() {
        return getValue(sharedPreferences, key, (T) defaultValue);
    }

    protected abstract T getValue(
            SharedPreferences sharedPreferences,
            String key,
            T defaultValue
    );

}
