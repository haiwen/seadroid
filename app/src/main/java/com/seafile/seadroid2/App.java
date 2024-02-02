package com.seafile.seadroid2;

import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.sp.Sorts;

public class App {
    public static void init() {

        //
        Sorts.init();


        //init slogs
        SLogs.init();


        SLogs.printAppEnvInfo();
    }
}
