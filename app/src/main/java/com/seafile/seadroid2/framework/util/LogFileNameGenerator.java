package com.seafile.seadroid2.framework.util;

import com.elvishew.xlog.printer.file.naming.DateFileNameGenerator;

public class LogFileNameGenerator extends DateFileNameGenerator {
    @Override
    public String generateFileName(int logLevel, long timestamp) {
        return super.generateFileName(logLevel, timestamp) + ".log";
    }
}
