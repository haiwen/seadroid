package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;

class NewLibTask extends TaskDialog.Task {

    private String mLibName;
    private String mDescription;
    private String mPassword;
    private DataManager mDataManager;

    public NewLibTask(String libName, String description, String password, DataManager dataManager) {
        mLibName = libName;
        mDescription = description;
        mPassword = password;
        mDataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            mDataManager.createNewLib(mLibName, mDescription, mPassword);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class NewLibDialog extends TaskDialog {

    private final static String STATE_ACCOUNT = "new_lib_dialog.account";

    // The input fields of the dialog
    private EditText mLibNameText;
    private EditText mDescriptionText;
    //  Use plain text field to avoid having to compare two obfuscated fields
    private EditText mPasswordText;

    private Account mAccount;
    private DataManager mDataManager;

    public void init(Account account) {
        // The DataManager is not parcelable, so we save the intermediate Account instead
        mAccount = account;
    }

    private DataManager getDataManager() {
        if (mDataManager == null) {
            mDataManager = new DataManager(mAccount);
        }

        return mDataManager;
    }

    public String getLibName() { return mLibNameText.getText().toString().trim(); }
    private String getDescription() { return mDescriptionText.getText().toString().trim(); }
    private String getPassword() { return mPasswordText.getText().toString().trim(); }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_new_lib, null);
        mLibNameText = (EditText) view.findViewById(R.id.new_lib_name);
        mDescriptionText = (EditText) view.findViewById(R.id.new_lib_description);
        mPasswordText = (EditText) view.findViewById(R.id.new_lib_password);

        if (savedInstanceState != null) {
            // Restore state
            mAccount = (Account) savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        return view;
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        // Save state
        outState.putParcelable(STATE_ACCOUNT, mAccount);
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(R.string.create_new_lib);
        dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        if (getLibName().length() == 0) {
            throw new Exception(getResources().getString(R.string.lib_name_empty));
        }
    }

    @Override
    protected NewLibTask prepareTask() {
        return new NewLibTask(getLibName(), getDescription(), getPassword(), getDataManager());
    }

    @Override
    protected void disableInput() {
        super.disableInput();
        mLibNameText.setEnabled(false);
        mDescriptionText.setEnabled(false);
        mPasswordText.setEnabled(false);
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        mLibNameText.setEnabled(true);
        mDescriptionText.setEnabled(true);
        mPasswordText.setEnabled(true);
    }
}
