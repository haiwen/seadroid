package com.seafile.seadroid2.util;

import android.content.Context;
import android.content.SharedPreferences;

import java.util.Map;

/**
 * XML-storage
 */
public abstract class SharedPreferencesUtil {

	/**
	 * system settings
	 */
	private String XML_FILE_NAME = "MY_SYSTEM_SET";

	public SharedPreferencesUtil(String name) {
		this.XML_FILE_NAME = name;
	}

	/**
	 * set storage
	 *
	 * @param context
	 * @param keys
	 * @param object
	 * @return
	 */
	public boolean putData(Context context, String keys, Object object) {
		return put(XML_FILE_NAME, context, keys, object);
	}

	/**
	 * get data
	 *
	 * @param context
	 * @param keys
	 * @param defValue
	 * @return
	 */
	public Object getData(Context context, String keys, Object defValue) {
		return get(XML_FILE_NAME, context, keys, defValue);
	}

	public Object getData(Context context, String keys) {
		return get(XML_FILE_NAME, context, keys);
	}

	/**
	 * @param context
	 * @return
	 */
	public boolean cleanData(Context context) {
		return clear(XML_FILE_NAME, context);
	}

	/**
	 * @param context
	 * @param keys
	 * @return
	 */
	public boolean removeDate(Context context, String keys) {
		return remove(XML_FILE_NAME, context, keys);
	}

	/**
	 * @param context
	 * @return
	 */
	public Map<String, ?> getAllData(Context context) {
		return getAll(XML_FILE_NAME, context);
	}


	/**
	 * @param fileName
	 * @param context
	 * @param keys
	 * @param object
	 * @return
	 */
	private boolean put(String fileName, Context context, String keys, Object object) {
		if (object == null) {
			return false;
		}
		final SharedPreferences sp = context.getSharedPreferences(fileName, Context.MODE_PRIVATE);
		final SharedPreferences.Editor editor = sp.edit();
		if (object instanceof String) {
			editor.putString(keys, String.valueOf(object));
		} else if (object instanceof Integer) {
			editor.putInt(keys, (Integer) object);
		} else if (object instanceof Float) {
			editor.putFloat(keys, (Float) object);
		} else if (object instanceof Boolean) {
			editor.putBoolean(keys, (Boolean) object);
		} else if (object instanceof Long) {
			editor.putLong(keys, (Long) object);
		} else {
			editor.putString(keys, String.valueOf(object));
		}
		return editor.commit();
	}

	/**
	 * @param fileName
	 * @param context
	 * @param keys
	 * @param defValue
	 * @return
	 */
	private Object get(String fileName, Context context, String keys, Object defValue) {
		final SharedPreferences sp = context.getSharedPreferences(fileName, Context.MODE_PRIVATE);
		if (defValue instanceof String) {
			return sp.getString(keys, String.valueOf(defValue));
		} else if (defValue instanceof Integer) {
			return sp.getInt(keys, (Integer) defValue);
		} else if (defValue instanceof Float) {
			return sp.getFloat(keys, (Float) defValue);
		} else if (defValue instanceof Boolean) {
			return sp.getBoolean(keys, (Boolean) defValue);
		} else if (defValue instanceof Long) {
			return sp.getLong(keys, (Long) defValue);
		}
		return null;
	}

	/**
	 * @param fileName
	 * @param context
	 * @param keys
	 * @return Object
	 */
	private Object get(String fileName, Context context, String keys) {
		final SharedPreferences sp = context.getSharedPreferences(fileName, Context.MODE_PRIVATE);
		return sp.getString(keys, null);
	}

	/**
	 * @param fileName
	 * @param context
	 * @param keys
	 * @return
	 */
	private boolean remove(String fileName, Context context, String keys) {
		final SharedPreferences sp = context.getSharedPreferences(fileName, Context.MODE_PRIVATE);
		final SharedPreferences.Editor editor = sp.edit();
		editor.remove(keys);
		return editor.commit();
	}

	/**
	 * @param fileName
	 * @param context
	 * @return
	 */
	private boolean clear(String fileName, Context context) {
		final SharedPreferences sp = context.getSharedPreferences(fileName, Context.MODE_PRIVATE);
		final SharedPreferences.Editor editor = sp.edit();
		editor.clear();
		return editor.commit();
	}

	/**
	 * @param fileName
	 * @param context
	 * @return
	 */
	public Map<String, ?> getAll(String fileName, Context context) {
		final SharedPreferences sp = context.getSharedPreferences(fileName, Context.MODE_PRIVATE);
		return sp.getAll();
	}
}
