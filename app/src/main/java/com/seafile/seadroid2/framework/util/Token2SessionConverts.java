package com.seafile.seadroid2.framework.util;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;

public class Token2SessionConverts {

    public static String buildUrl(String next) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return next;
        }
        String host = account.server;
        return host + "mobile-login/?next=" + next;
    }
}
