package com.seafile.seadroid2.framework.util;

import android.net.Uri;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;

public class Token2SessionConverts {

    public static String buildUrl(String next) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return next;
        }
        String host = account.server;
        return Uri.parse(host + "mobile-login/")
                .buildUpon()
                .appendQueryParameter("next", next)
                .build()
                .toString();
    }
}
