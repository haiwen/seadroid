package com.seafile.seadroid;

import android.app.Application;
import android.content.Context;

public class SeadroidApplication extends Application {

    private static Context context;

    public void onCreate() {
        super.onCreate();
        SeadroidApplication.context = getApplicationContext();
    }

    public static Context getAppContext() {
        return SeadroidApplication.context;
    }

}