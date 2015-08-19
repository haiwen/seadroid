package com.horizonbase.seadroid2.monitor;

import java.io.File;

import com.horizonbase.seadroid2.account.Account;
import com.horizonbase.seadroid2.data.SeafCachedFile;

interface CachedFileChangedListener {
    void onCachedFileChanged(Account account, SeafCachedFile cf, File file);
}

