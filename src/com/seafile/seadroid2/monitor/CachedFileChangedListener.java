package com.seafile.seadroid2.monitor;

import java.io.File;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedFile;

interface CachedFileChangedListener {
    void onCachedFiledChanged(Account account, SeafCachedFile cf, File file);
}

