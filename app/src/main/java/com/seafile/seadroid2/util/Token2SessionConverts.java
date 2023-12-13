package com.seafile.seadroid2.util;

import com.seafile.seadroid2.account.SupportAccountManager;

public class Token2SessionConverts {

    public static String buildUrl(String next) {
        String host = SupportAccountManager.getInstance().getCurrentAccount().server;
        return host + "mobile-login/?next=" + next;
    }
}
