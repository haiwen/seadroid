package com.seafile.seadroid2.framework.datastore.sp;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;

public class AccountDataStoreManager {
    private static Account currentAccount = null;

    public static String getCurrentAccount() {
        return getInstanceAccount().getSignature();
    }

    public static Account getInstanceAccount() {
        if (currentAccount == null) {
            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account == null) {
                throw new IllegalArgumentException("account is null.");
            }

            currentAccount = account;
        }
        return currentAccount;
    }

    public static boolean readIsQuotaLimited() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readBoolean(DataStoreKeys.ACCOUNT_QUOTA_NO_LIMIT_KEY);
    }

    public static void writeIsQuotaLimited(boolean b) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeBoolean(DataStoreKeys.ACCOUNT_QUOTA_NO_LIMIT_KEY, b);
    }
}
