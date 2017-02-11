package com.seafile.seadroid2.cameraupload;

import android.content.ContentResolver;
import android.content.Context;
import android.os.Bundle;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;


/**
 * Contacts Upload Manager.
 * <p/>
 * This class can be used by other parts of Seadroid to enable/configure the contacts upload
 * service.
 */
public class ContactsUploadManager {

    /**
     * The authority of the contacts sync service
     */
    public static final String AUTHORITY = BuildConfig.APPLICATION_ID + ".contactsupload.provider";

    AccountManager accountManager;

    public ContactsUploadManager(Context context) {
        accountManager = new AccountManager(context);
    }

    /**
     * Is contacts upload enabled?
     *
     * @return true if contacts upload is enabled.
     */
    public boolean isContactsUploadEnabled() {
        Account account = getContactsAccount();
        return account != null;
    }

    /**
     * Get the account that is currently the remote target for the camera upload
     *
     * @return the account if contacts is enabled, null otherwise.
     */
    public Account getContactsAccount() {
        for (Account account : accountManager.getAccountList()) {
            int isSyncable = ContentResolver.getIsSyncable(account.getAndroidAccount(), AUTHORITY);
            if (isSyncable > 0)
                return account;
        }
        return null;
    }

    /**
     * Initiate a contacts sync immediately.
     */
    public void performSync() {
        Account contactsAccount = getContactsAccount();
        if (contactsAccount != null)
            ContentResolver.requestSync(contactsAccount.getAndroidAccount(), AUTHORITY, Bundle.EMPTY);
    }

    /**
     * Initiate a contacts sync immediately, upload all media files again.
     */
    public void performFullSync() {
        Bundle b = new Bundle();
        b.putBoolean(ContentResolver.SYNC_EXTRAS_INITIALIZE, true);

        Account contactsAccount = getContactsAccount();
        if (contactsAccount != null)
            ContentResolver.requestSync(contactsAccount.getAndroidAccount(), AUTHORITY, b);
    }

    /**
     * Change the account currently responsible for contacts upload.
     *
     * @param account An account. must not be null.
     */
    public void setContactsAccount(Account account) {
        for (Account a : accountManager.getAccountList()) {
            if (a.equals(account)) {
                // enable contacts upload on this account
                ContentResolver.setIsSyncable(a.getAndroidAccount(), AUTHORITY, 1);
                ContentResolver.setSyncAutomatically(a.getAndroidAccount(), AUTHORITY, true);
            } else {
                // disable on all the others
                ContentResolver.cancelSync(a.getAndroidAccount(), AUTHORITY);
                ContentResolver.setIsSyncable(a.getAndroidAccount(), AUTHORITY, 0);

            }
        }
    }

    /**
     * Disable contacts upload.
     */
    public void disableContactsUpload() {
        for (Account account : accountManager.getAccountList()) {
            ContentResolver.cancelSync(account.getAndroidAccount(), AUTHORITY);
            ContentResolver.setIsSyncable(account.getAndroidAccount(), AUTHORITY, 0);
        }
    }
}
