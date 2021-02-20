package com.seafile.seadroid2.util;

import android.content.Context;
import android.os.Environment;
import android.util.Log;

import com.seafile.seadroid2.SeadroidApplication;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class SeafileLog {

    private static Boolean MYLOG_SWITCH = true; // Main switch
    private static Boolean MYLOG_WRITE_TO_FILE = true;// log switch
    private static char MYLOG_TYPE = 'v';
    private static int SDCARD_LOG_FILE_SAVE_DAYS = 0;
    private static String MYLOGFILENAME = "Log.txt";
    private static SimpleDateFormat myLogSdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");// The output format of the log
    private static SimpleDateFormat logfile = new SimpleDateFormat("yyyy-MM-dd");// Log file format
    public Context context;

    public static void w(String tag, Object msg) { // Warning message
        log(tag, msg.toString(), 'w');
    }

    public static void e(String tag, Object msg) { // The error message
        log(tag, msg.toString(), 'e');
    }

    public static void d(String tag, Object msg) {// Debugging information
        log(tag, msg.toString(), 'd');
    }

    public static void i(String tag, Object msg) {//
        log(tag, msg.toString(), 'i');
    }

    public static void v(String tag, Object msg) {
        log(tag, msg.toString(), 'v');
    }

    public static void w(String tag, String text) {
        log(tag, text, 'w');
    }

    public static void e(String tag, String text) {
        log(tag, text, 'e');
    }

    public static void d(String tag, String text) {
        log(tag, text, 'd');
    }

    public static void i(String tag, String text) {
        log(tag, text, 'i');
    }

    public static void v(String tag, String text) {
        log(tag, text, 'v');
    }

    private static void log(String tag, String msg, char level) {
        if (MYLOG_SWITCH) {//Log file master switch
            if ('e' == level && ('e' == MYLOG_TYPE || 'v' == MYLOG_TYPE)) {
                Log.e(tag, msg);
            } else if ('w' == level && ('w' == MYLOG_TYPE || 'v' == MYLOG_TYPE)) {
                Log.w(tag, msg);
            } else if ('d' == level && ('d' == MYLOG_TYPE || 'v' == MYLOG_TYPE)) {
                Log.d(tag, msg);
            } else if ('i' == level && ('d' == MYLOG_TYPE || 'v' == MYLOG_TYPE)) {
                Log.i(tag, msg);
            } else {
                Log.v(tag, msg);
            }
            if (MYLOG_WRITE_TO_FILE)//Log write file switch
                writeLogtoFile(String.valueOf(level), tag, msg);
        }
    }

    /**
     * Open the log file and write to the log
     *
     * @param mylogtype
     * @param tag
     * @param text
     */
    private static void writeLogtoFile(String mylogtype, String tag, String text) {
        Date nowtime = new Date();
        String needWriteFile = logfile.format(nowtime);
        String needWriteMessage = myLogSdf.format(nowtime) + "    " + mylogtype + "    " + tag + "    " + text;
//        File dirsFile = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + "/Seafile/");
        String rootPath = SeadroidApplication.getAppContext().getExternalFilesDir(Environment.DIRECTORY_DOCUMENTS).getAbsolutePath();
        File dirsFile = new File(rootPath + "/Seafile/");
        if (!dirsFile.exists()) {
            dirsFile.mkdirs();
        }
        File file = new File(dirsFile.toString(), needWriteFile + MYLOGFILENAME);// MYLOG_PATH_SDCARD_DIR
        if (!file.exists()) {
            try {
                file.createNewFile();
            } catch (Exception e) {
            }
        }

        try {
            FileWriter filerWriter = new FileWriter(file, true);
            BufferedWriter bufWriter = new BufferedWriter(filerWriter);
            bufWriter.write(needWriteMessage);
            bufWriter.newLine();
            bufWriter.close();
            filerWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Delete the specified log file
     */
    public static void delFile() {
        String needDelFiel = logfile.format(getDateBefore());
        String rootPath = SeadroidApplication.getAppContext().getExternalFilesDir(Environment.DIRECTORY_DOCUMENTS).getAbsolutePath();
//        File dirPath = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + "/Seafile/");
        File dirPath = new File(rootPath + "/Seafile/");
        File file = new File(dirPath, needDelFiel + MYLOGFILENAME);// MYLOG_PATH_SDCARD_DIR
        if (file.exists()) {
            file.delete();
        }
    }

    /**
     * Use to get the file name of the log to delete
     */
    private static Date getDateBefore() {
        Date nowtime = new Date();
        Calendar now = Calendar.getInstance();
        now.setTime(nowtime);
        now.set(Calendar.DATE, now.get(Calendar.DATE) - SDCARD_LOG_FILE_SAVE_DAYS);
        return now.getTime();
    }
}
