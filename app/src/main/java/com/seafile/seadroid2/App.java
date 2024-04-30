package com.seafile.seadroid2;

import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.datastore.sp.Sorts;

public class App {
    public static void init() {

        //
        Sorts.init();


        //init slogs
        SLogs.init();


        SLogs.printAppEnvInfo();
    }
}
