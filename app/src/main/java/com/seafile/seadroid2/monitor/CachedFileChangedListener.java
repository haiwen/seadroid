package com.seafile.seadroid2.monitor;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedFile;

import java.io.File;

interface CachedFileChangedListener {
    void onCachedBlocksChanged(Account account, SeafCachedFile cf, File file);

    void onCachedFileChanged(Account account, SeafCachedFile cf, File file);
}

