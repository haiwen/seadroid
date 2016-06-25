package com.seafile.seadroid2.monitor;

import java.io.File;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedFile;

interface CachedFileChangedListener {
    void onCachedBlocksChanged(Account account, SeafCachedFile cf, File file, int version);

    void onCachedFileChanged(Account account, SeafCachedFile cf, File file);
}

