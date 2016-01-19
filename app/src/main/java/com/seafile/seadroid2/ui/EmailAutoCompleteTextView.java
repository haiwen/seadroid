package com.seafile.seadroid2.ui;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.Context;
import android.support.v7.widget.AppCompatAutoCompleteTextView;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Patterns;
import android.widget.ArrayAdapter;

import com.seafile.seadroid2.R;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Automatically fills in email accounts
 */
public class EmailAutoCompleteTextView extends AppCompatAutoCompleteTextView {

    public EmailAutoCompleteTextView(Context context) {
        super(context);
        init();
    }

    public EmailAutoCompleteTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public EmailAutoCompleteTextView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    private void init() {
        if (isInEditMode()) { return; }
        retrieveAccounts();
    }

    /**
     * Manually retrieve the accounts, typically used for API 23+ after getting the permission. Called automatically
     * on creation, but needs to be recalled if the permission is granted later
     */
    public void retrieveAccounts() {
        Collection<String> accounts = getEmailAccounts();
        if (accounts != null && !accounts.isEmpty()) {
            ArrayAdapter<String> adapter = new ArrayAdapter<>(getContext(),
                    R.layout.support_simple_spinner_dropdown_item,
                    new ArrayList<>(accounts));
            setAdapter(adapter);
        }
    }

    /**
     * Get all the accounts that appear to be email accounts. HashSet so that we do not get duplicates
     * @return list of email accounts
     */
    private Set<String> getEmailAccounts() {
        HashSet<String> emailAccounts = new HashSet<>();
        AccountManager manager = (AccountManager) getContext().getSystemService(Context.ACCOUNT_SERVICE);
        final Account[] accounts = manager.getAccounts();
        for (Account account : accounts) {
            if (!TextUtils.isEmpty(account.name) && Patterns.EMAIL_ADDRESS.matcher(account.name).matches()) {
                emailAccounts.add(account.name);
            }
        }

        ArrayList<String> accountlist = new com.seafile.seadroid2.account.AccountManager(getContext()).getAccountAutoCompleteTexts();
        if (accountlist != null) {
            for (String account : accountlist) {
                emailAccounts.add(account);
            }
        }

        return emailAccounts;
    }
}
