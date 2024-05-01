package com.seafile.seadroid2.framework.datastore;

import android.content.Context;
import android.content.SharedPreferences;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.SeadroidApplication;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Multi-account preference manager
 */
public class DataStoreManager {
    private static final String DATA_STORE_NAME = "seafile_app_data_store";
    private final String DATA_STORE_NAME_PREFIX = "seafile_user_";
    private final String DATA_STORE_NAME_SUFFIX = "_data_store";

    private static final Map<String, DataStoreManager> instanceMap = new HashMap<>();

    //    private String filePath;
    //    private RxDataStore<Preferences> dataStore;
    private SharedPreferences sharedPreferences;

    public SharedPreferences getSharedPreferences() {
        return sharedPreferences;
    }

    private DataStoreManager(String acc) {
        Context context = SeadroidApplication.getAppContext();
        if (DATA_STORE_NAME.equals(acc)) {
            sharedPreferences = context.getSharedPreferences(DATA_STORE_NAME, Context.MODE_PRIVATE);
//            dataStore = new RxPreferenceDataStoreBuilder(context, DATA_STORE_NAME).build();
        } else {
            acc = replaceNonAlphanumeric(acc);
            if (acc.endsWith("_")) {
                acc = StringUtils.substringBeforeLast(acc, "_");
            }

            sharedPreferences = context.getSharedPreferences(DATA_STORE_NAME_PREFIX + acc + DATA_STORE_NAME_SUFFIX, Context.MODE_PRIVATE);
//            dataStore = new RxPreferenceDataStoreBuilder(context, DATA_STORE_NAME_PREFIX + acc).build();
        }

//        this.filePath = context.getFilesDir().getAbsolutePath() + "/datastore/";
    }

    private String replaceNonAlphanumeric(String input) {
        String regex = "[^a-zA-Z0-9]";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(input);
        return matcher.replaceAll("_");
    }

    public static void resetUserInstance() {
        instanceMap.clear();
    }

    public static DataStoreManager getCommonInstance() {
        return getInstanceByUser(DATA_STORE_NAME);
    }

    public static DataStoreManager getInstanceByUser(String accountSignature) {
        if (instanceMap.containsKey(accountSignature)) {
            return instanceMap.get(accountSignature);
        }

        DataStoreManager dataStoreManager = new DataStoreManager(accountSignature);
        instanceMap.put(accountSignature, dataStoreManager);
        return dataStoreManager;
    }


//    private void writingStringSteps(String nameKey, String valueKey) {
//        Preferences.Key<String> key = PreferencesKeys.stringKey(nameKey);
//        Single<Preferences> updateResult = dataStore.updateDataAsync(inputPreferences -> {
//            MutablePreferences mutablePreferences = inputPreferences.toMutablePreferences();
//            mutablePreferences.set(key, valueKey);
//            return Single.just(mutablePreferences);
//        });
//        updateResult.subscribe();
//    }

//    public Completable migrateSharePreference2DataStore(Context context) {
//        SharedPreferences sp = context.getSharedPreferences(SPs.SP_NAME, Context.MODE_PRIVATE);
//        Map<String, ?> sKeys = sp.getAll();
//        return Completable.fromAction(new Action() {
//            @Override
//            public void run() throws Exception {
//                dataStore.updateDataAsync(new Function<Preferences, Single<Preferences>>() {
//                    @Override
//                    public Single<Preferences> apply(Preferences preferences) throws Exception {
//                        MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//
//                        if (sKeys != null) {
//                            for (Map.Entry<String, ?> stringEntry : sKeys.entrySet()) {
//                                String nameKey = stringEntry.getKey();
//                                Object value = stringEntry.getValue();
//
//
//                                if (value instanceof String) {
//                                    Preferences.Key<String> pKey = PreferencesKeys.stringKey(nameKey);
//                                    mutablePreferences.set(pKey, value.toString());
//                                } else if (value instanceof Integer) {
//                                    Preferences.Key<Integer> pKey = PreferencesKeys.intKey(nameKey);
//                                    mutablePreferences.set(pKey, (Integer) value);
//                                } else if (value instanceof Long) {
//                                    Preferences.Key<Long> pKey = PreferencesKeys.longKey(nameKey);
//                                    mutablePreferences.set(pKey, (Long) value);
//                                } else if (value instanceof Boolean) {
//                                    Preferences.Key<Boolean> pKey = PreferencesKeys.booleanKey(nameKey);
//                                    mutablePreferences.set(pKey, (Boolean) value);
//                                } else if (value instanceof Float) {
//                                    Preferences.Key<Float> pKey = PreferencesKeys.floatKey(nameKey);
//                                    mutablePreferences.set(pKey, (Float) value);
//                                } else if (value instanceof Set) {
//                                    Preferences.Key<Set<String>> pKey = PreferencesKeys.stringSetKey(nameKey);
//                                    mutablePreferences.set(pKey, (Set<String>) value);
//                                }
//
//
//                            }
//                        }
//
//                        return Single.just(mutablePreferences);
//                    }
//                });
//            }
//        });
//    }

    /***
     * clear all data store file/
     */
//    public void removeAll() {
//        File file = new File(filePath);
//        File[] files = file.listFiles();
//
//        if (files == null) {
//            return;
//        }
//
//        for (File file1 : files) {
//            file1.delete();
//        }
//    }
    public void writeInteger(String strKey, Integer value) {
        writeValue(strKey, value);
    }

    public void writeBoolean(String strKey, Boolean value) {
        writeValue(strKey, value);
    }

    public void writeString(String strKey, String value) {
        if (null == value) {
            value = "";
        }
        writeValue(strKey, value);
    }

    public void writeDouble(String strKey, @NonNull Double value) {
        writeValue(strKey, value);
    }

    public void writeFloat(String strKey, @NonNull Float value) {
        writeValue(strKey, value);
    }

    public void writeLong(String strKey, @NonNull Long value) {
        writeValue(strKey, value);
    }

//    public void writeSetString(String strKey, Set<String> value) {
//        if (null == value) {
//            value = new HashSet<>();
//        }
//        writeValue(strKey, value);
//    }

    /**
     * write value
     */
    private void writeValue(String strKey, Object value) {
//        Single<Preferences> single = null;
        if (value instanceof Integer) {
            sharedPreferences.edit().putInt(strKey, (Integer) value).apply();

//            Preferences.Key<Integer> intKey = PreferencesKeys.intKey(strKey);
//            single = dataStore.updateDataAsync(preferences -> {
//                MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//                mutablePreferences.set(intKey, (Integer) value);
//                return Single.just(mutablePreferences);
//            });
        } else if (value instanceof String) {
            sharedPreferences.edit().putString(strKey, (String) value).apply();

//            Preferences.Key<String> stringKey = PreferencesKeys.stringKey(strKey);
//            single = dataStore.updateDataAsync(preferences -> {
//                        MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//                        mutablePreferences.set(stringKey, (String) value);
//                        return Single.just(mutablePreferences);
//                    }
//            );
        } else if (value instanceof Boolean) {
            sharedPreferences.edit().putBoolean(strKey, (Boolean) value).apply();

//            Preferences.Key<Boolean> booleanKey = PreferencesKeys.booleanKey(strKey);
//            single = dataStore.updateDataAsync(preferences -> {
//                MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//                mutablePreferences.set(booleanKey, (Boolean) value);
//                return Single.just(mutablePreferences);
//            });
        } else if (value instanceof Double) {
            sharedPreferences.edit().putFloat(strKey, (Float) value).apply();

//            Preferences.Key<Double> doubleKey = PreferencesKeys.doubleKey(strKey);
//            single = dataStore.updateDataAsync(preferences -> {
//                MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//                mutablePreferences.set(doubleKey, (Double) value);
//                return Single.just(mutablePreferences);
//            });
        } else if (value instanceof Float) {
            sharedPreferences.edit().putFloat(strKey, (Float) value).apply();

//            Preferences.Key<Float> floatKey = PreferencesKeys.floatKey(strKey);
//            single = dataStore.updateDataAsync(preferences -> {
//                MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//                mutablePreferences.set(floatKey, (Float) value);
//                return Single.just(mutablePreferences);
//            });
        } else if (value instanceof Long) {
            sharedPreferences.edit().putLong(strKey, (Long) value).apply();

//            Preferences.Key<Long> longKey = PreferencesKeys.longKey(strKey);
//            single = dataStore.updateDataAsync(preferences -> {
//                MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//                mutablePreferences.set(longKey, (Long) value);
//                return Single.just(mutablePreferences);
//            });
        }
//        else if (value instanceof Set<?>) {
//            Set<String> set = (Set<String>) value;
//            sharedPreferences.edit().putStringSet(strKey, set).apply();
//
////            Preferences.Key<Set<String>> setKey = PreferencesKeys.stringSetKey(strKey);
////            single = dataStore.updateDataAsync(preferences -> {
////                MutablePreferences mutablePreferences = preferences.toMutablePreferences();
////                mutablePreferences.set(setKey, (Set<String>) value);
////                return Single.just(mutablePreferences);
////            });
//        }
        else {
            throw new IllegalStateException("Unexpected value: " + value);
        }

//        single.subscribe();
    }

    /**
     * read String
     */
    public String readString(String key) {
        return readString(key, null);
    }

    public String readString(String key, String defaultValue) {
        if (defaultValue == null) {
            defaultValue = "";
        }
        return sharedPreferences.getString(key, defaultValue);

//        Preferences.Key<String> stringKey = PreferencesKeys.stringKey(key);
//        Flowable<String> flowable = dataStore.data().map(new Function<Preferences, String>() {
//            @Override
//            public String apply(Preferences preferences) throws Exception {
//                String value = preferences.get(stringKey);
//                return value != null ? value : "";
//            }
//        });
//        return flowable.first(defaultValue).blockingGet();
    }

    public int readInteger(String key) {
        return readInteger(key, 0);
//        Preferences.Key<Integer> intKey = PreferencesKeys.intKey(key);
//        Flowable<Integer> flowable = dataStore.data().map(new Function<Preferences, Integer>() {
//            @Override
//            public Integer apply(Preferences preferences) throws Exception {
//                Integer value = preferences.get(intKey);
//                return value != null ? value : 0;
//            }
//        });
//        return flowable.first(0).blockingGet();
    }

    public int readInteger(String key, Integer defaultValue) {
        return sharedPreferences.getInt(key, defaultValue);
    }

    public boolean readBoolean(String key) {
        return sharedPreferences.getBoolean(key, false);

//        Preferences.Key<Boolean> booleanKey = PreferencesKeys.booleanKey(key);
//        Flowable<Boolean> flowable = dataStore.data().map(new Function<Preferences, Boolean>() {
//            @Override
//            public Boolean apply(Preferences preferences) throws Exception {
//                Boolean value = preferences.get(booleanKey);
//                return value != null ? value : false;
//            }
//        });
//        return flowable.first(false).blockingGet();
    }

    public boolean readBoolean(String key, boolean defaultValue) {
        return sharedPreferences.getBoolean(key, defaultValue);
    }

    public boolean readAndSetBooleanWhenNotExists(String key, boolean defaultToSetWhenNotExists) {
        if (!sharedPreferences.contains(key)) {
            writeBoolean(key, defaultToSetWhenNotExists);
            return defaultToSetWhenNotExists;
        }
        return sharedPreferences.getBoolean(key, defaultToSetWhenNotExists);
    }

    public double readDouble(String key) {
        return sharedPreferences.getFloat(key, 0F);

//        Preferences.Key<Double> doubleKey = PreferencesKeys.doubleKey(key);
//        Flowable<Double> flowable = dataStore.data().map(new Function<Preferences, Double>() {
//            @Override
//            public Double apply(Preferences preferences) throws Exception {
//                Double value = preferences.get(doubleKey);
//                return value != null ? value : 0.0;
//            }
//        });
//        return flowable.first(0.0).blockingGet();
    }

    public float readFloat(String key) {
        return sharedPreferences.getFloat(key, 0F);

//        Preferences.Key<Float> floatKey = PreferencesKeys.floatKey(key);
//        Flowable<Float> flowable = dataStore.data().map(new Function<Preferences, Float>() {
//            @Override
//            public Float apply(Preferences preferences) throws Exception {
//                Float value = preferences.get(floatKey);
//                return value != null ? value : 0F;
//            }
//        });
//        return flowable.first(0F).blockingGet();
    }

    public long readLong(String key) {
        return sharedPreferences.getLong(key, 0L);

//        Preferences.Key<Long> longKey = PreferencesKeys.longKey(key);
//        Flowable<Long> flowable = dataStore.data().map(new Function<Preferences, Long>() {
//            @Override
//            public Long apply(Preferences preferences) throws Exception {
//                Long value = preferences.get(longKey);
//                return value != null ? value : 0L;
//            }
//        });
//        return flowable.first(0L).blockingGet();
    }


//    public Set<String> readSetString(String key) {
//        return sharedPreferences.getStringSet(key, new HashSet<>());
//
////        Preferences.Key<Set<String>> setKey = PreferencesKeys.stringSetKey(key);
////        Flowable<Set<String>> flowable = dataStore.data().map(new Function<Preferences, Set<String>>() {
////            @Override
////            public Set<String> apply(Preferences preferences) throws Exception {
////                Set<String> value = preferences.get(setKey);
////                return value != null ? new HashSet<>(value) : new HashSet<>();
////            }
////        });
////
////        return flowable.first(new HashSet<>()).blockingGet();
//    }


//    public void removeByStringKey(String strKey) {
//        Preferences.Key<String> k = PreferencesKeys.stringKey(strKey);
//        removeByKey(k);
//    }
//
//    public void removeByIntegerKey(String strKey) {
//        Preferences.Key<Integer> k = PreferencesKeys.intKey(strKey);
//        removeByKey(k);
//    }
//
//    public void removeByBooleanKey(String strKey) {
//        Preferences.Key<Boolean> k = PreferencesKeys.booleanKey(strKey);
//        removeByKey(k);
//    }
//
//    public void removeByDoubleKey(String strKey) {
//        Preferences.Key<Double> k = PreferencesKeys.doubleKey(strKey);
//        removeByKey(k);
//    }
//
//    public void removeByLongKey(String strKey) {
//        Preferences.Key<Long> k = PreferencesKeys.longKey(strKey);
//        removeByKey(k);
//    }
//
//    public void removeByFloatKey(String strKey) {
//        Preferences.Key<Float> k = PreferencesKeys.floatKey(strKey);
//        removeByKey(k);
//    }
//
//    public void removeBySetKey(String strKey) {
//        Preferences.Key<Set<String>> k = PreferencesKeys.stringSetKey(strKey);
//        removeByKey(k);
//    }

    public void removeByKey(String key) {
        if (sharedPreferences != null) {
            sharedPreferences.edit().remove(key).apply();
        }
    }

//    public <T> void removeByKey(Preferences.Key<T> key) {
//
//        dataStore.updateDataAsync(new Function<Preferences, Single<Preferences>>() {
//            @Override
//            public Single<Preferences> apply(Preferences preferences) throws Exception {
//                MutablePreferences mutablePreferences = preferences.toMutablePreferences();
//                mutablePreferences.remove(key);
//                return Single.just(mutablePreferences);
//            }
//        }).subscribe();
//    }

}
