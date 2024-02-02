package com.seafile.seadroid2.util;

import java.lang.reflect.ParameterizedType;

public class TUtil {
    public static <T> T getT(Object o, int i) {
        try {
            return (
                    (Class<T>)
                            (
                                    (ParameterizedType)
                                            (
                                                    //get parent class
                                                    o.getClass().getGenericSuperclass()
                                            )
                            ).getActualTypeArguments()[i]
            )
                    .newInstance();
        } catch (InstantiationException | ClassCastException | IllegalAccessException e) {
            SLogs.e(e);
        }
        return null;
    }

    public static Class<?> forName(String className) {
        try {
            return Class.forName(className);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }
}
