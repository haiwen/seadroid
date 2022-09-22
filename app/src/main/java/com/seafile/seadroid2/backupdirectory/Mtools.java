package com.seafile.seadroid2.backupdirectory;

import android.content.Context;
import android.util.Log;
import android.widget.Toast;

public class Mtools {
    public static void toast(Context context, String text){
        toast(context,text, Toast.LENGTH_SHORT);
    }
    public static void toast(Context context, String text, int time){
        Toast.makeText(context,text,time).show();
    }

    public static void log(String text){
        log(text,0);
    }
    public static void log(Object text){
        log(String.valueOf(text),0);
    }

    public static void log(String text, int type){
        switch (type){
            case 0:
                Log.e("Mtools--->log--E",text);
                break;
        }
    }



}
