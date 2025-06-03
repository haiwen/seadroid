package com.seafile.seadroid2.framework.util;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.PathUtils;
import com.elvishew.xlog.LogConfiguration;
import com.elvishew.xlog.LogLevel;
import com.elvishew.xlog.XLog;
import com.elvishew.xlog.flattener.ClassicFlattener;
import com.elvishew.xlog.printer.AndroidPrinter;
import com.elvishew.xlog.printer.ConsolePrinter;
import com.elvishew.xlog.printer.Printer;
import com.elvishew.xlog.printer.file.FilePrinter;
import com.elvishew.xlog.printer.file.backup.FileSizeBackupStrategy2;
import com.elvishew.xlog.printer.file.clean.FileLastModifiedCleanStrategy;
import com.elvishew.xlog.printer.file.naming.DateFileNameGenerator;
import com.elvishew.xlog.printer.file.naming.FileNameGenerator;
import com.google.common.base.Strings;
import com.seafile.seadroid2.BuildConfig;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import java.util.TimeZone;

import kotlin.text.StringsKt;

/**
 * <p>
 * <br>DevEnv: VERBOSE、DEBUG
 * <br>ProductEnv: INFO、WARN、ERROR
 * </p>
 *
 * <p>
 * <br>   see {@link com.elvishew.xlog.XLog}
 * <br>   see <a href="https://github.com/elvishew/xLog">https://github.com/elvishew/xLog</a>
 * </p>
 */
public class Logs {

    private static final String LOG_TAG = "Seafile-SLog";
    /**
     * will delete log files that have not been modified for a period of time
     */
    private static final long MAX_TIME = 30L * 24 * 60 * 60 * 1000;// 30 days
    private static final long MAX_SIZE = 1024 * 1024 * 50;// 50M

    /**
     * init log
     */
    public static void init() {
        LogConfiguration config = new LogConfiguration.Builder()
                .logLevel(BuildConfig.DEBUG ? LogLevel.ALL : LogLevel.DEBUG)
                .tag(LOG_TAG)
//                .enableThreadInfo()
//                .enableStackTrace(2)
//                .enableBorder()
                .build();
        Printer androidPrinter = new AndroidPrinter(true);

        // /storage/emulated/0/Android/data/package/files/logs
        // /sdcard/Android/data/package/files/logs
        String p = PathUtils.getExternalAppFilesPath();
        String logPath = p + "/logs";
        FileUtils.createOrExistsDir(logPath);

        Printer filePrinter = new FilePrinter
                .Builder(logPath)
                .fileNameGenerator(new LogFileNameGenerator())
                .flattener(new ClassicFlattener())
                .backupStrategy(new FileSizeBackupStrategy2(MAX_SIZE, 30))
                .cleanStrategy(new FileLastModifiedCleanStrategy(MAX_TIME))
                .build();

        XLog.init(config, androidPrinter, filePrinter);
    }

    public static void json(String json) {
        XLog.json(json);
    }

    public static void xml(String json) {
        XLog.xml(json);
    }

    //VERBOSE
    public static void v(Object object) {
        XLog.v(object);
    }

    public static void v(String msg) {
        XLog.v(msg);
    }

    public static void v(String msg, Throwable tr) {
        XLog.v(msg, tr);
    }

    //DEBUG
    public static void d(Object object) {
        XLog.d(object);
    }

    public static void d(String... logs) {
        if (logs == null || logs.length == 0) {
            return;
        }

        if (logs.length == 1) {
            d(logs[0]);
            return;
        }

        StringBuilder logBuilder = new StringBuilder();
        for (String s : logs) {
            logBuilder.append(s).append(", ");
        }
        String log = logBuilder.toString();
        if (log.endsWith(", ")) {
            log = log.substring(0, log.length() - 2);
        }
        d(log);
    }

    public static void d(String msg) {
        XLog.d(msg);
    }

    public static void dDebug(String msg) {
        if (BuildConfig.DEBUG) {
            XLog.d(msg);
        }
    }


    public static void d(String msg, Throwable tr) {
        XLog.d(msg, tr);
    }

    //info
    public static void i(Object object) {
        XLog.i(object);
    }

    public static void i(Object[] array) {
        XLog.i(array);
    }

    public static void i(String format, Object... args) {
        XLog.i(format, args);
    }

    public static void i(String msg) {
        XLog.i(msg);
    }

    public static void i(String msg, Throwable tr) {
        XLog.i(msg, tr);
    }


    //warn
    public static void w(Object object) {
        XLog.w(object);
    }


    public static void w(Object[] array) {
        XLog.w(array);
    }


    public static void w(String format, Object... args) {
        XLog.w(format, args);
    }

    public static void w(String msg) {
        XLog.w(msg);
    }

    public static void w(String msg, Throwable tr) {
        XLog.w(msg, tr);
    }

    //error
    public static void e(Object object) {
        XLog.e(object);
    }

    public static void e(Object[] array) {
        XLog.e(array);
    }

    public static void e(String format, Object... args) {
        XLog.e(format, args);
    }


    public static void e(String... logs) {
        if (logs == null || logs.length == 0) {
            return;
        }

        if (logs.length == 1) {
            d(logs[0]);
            return;
        }

        StringBuilder logBuilder = new StringBuilder();
        for (String s : logs) {
            logBuilder.append(s).append(", ");
        }
        String log = logBuilder.toString();
        if (log.endsWith(", ")) {
            log = log.substring(0, log.length() - 2);
        }
        e(log);
    }

    public static void e(String msg) {
        XLog.e(msg);
    }

    public static void e(String msg, Throwable tr) {
        XLog.e(msg, tr);
    }

    public static void eDebug(String msg) {
        if (BuildConfig.DEBUG) {
            XLog.e(msg);
        }
    }

}
