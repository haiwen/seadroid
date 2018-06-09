//package com.seafile.seadroid2.cameraupload;
//
//import android.content.Intent;
//import android.os.Bundle;
//import android.support.v4.app.FragmentManager;
//import android.view.View;
//import android.widget.Button;
//
//import com.seafile.seadroid2.R;
//import com.seafile.seadroid2.account.Account;
//import com.seafile.seadroid2.data.SeafRepo;
//import com.seafile.seadroid2.ui.activity.BaseActivity;
//import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;
//
//
///**
// * Contacts upload configuration helper
// */
//public class ContactsUploadConfigActivity extends BaseActivity implements View.OnClickListener {
//    public static final String DEBUG_TAG = "ContactsUploadConfigActivity";
//    private Button mDoneBtn;
//    private FragmentManager fm;
//    private ContactsSelectionFragment mSelectionFragment;
//    private Account mAccount;
//    private SeafRepo mSeafRepo;
//
//    @Override
//    public void onCreate(Bundle savedInstanceState) {
//        super.onCreate(savedInstanceState);
//        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
//
//        setContentView(R.layout.cuc_remote_library_fragment);
//
//        fm = getSupportFragmentManager();
//        fm.beginTransaction().add(R.id.cuc_remote_library_list_container, getAccountOrReposSelectionFragment()).commit();
//        mDoneBtn = (Button) findViewById(R.id.cuc_remote_library_btn);
//        mDoneBtn.setOnClickListener(this);
//        mDoneBtn.setVisibility(View.VISIBLE);
//
//    }
//
//
//    @Override
//    public void onClick(View view) {
//        saveSettings();
//        finish();
//    }
//
//    /**
//     * Instantiates a new fragment if mSelectionFragment is null.
//     * Returns the current fragment, otherwise.
//     */
//    public ContactsSelectionFragment getAccountOrReposSelectionFragment() {
//        if (mSelectionFragment == null) {
//            mSelectionFragment = new ContactsSelectionFragment();
//        }
//        return mSelectionFragment;
//    }
//
//    public void saveCameraUploadInfo(Account account, SeafRepo seafRepo) {
//        this.mAccount = account;
//        this.mSeafRepo = seafRepo;
//
//    }
//
//
//    public void saveSettings() {
//        // update cloud library data
//        Intent intent = new Intent();
//        if (mSeafRepo != null && mAccount != null) {
//            intent.putExtra(SeafilePathChooserActivity.DATA_REPO_NAME, mSeafRepo.name);
//            intent.putExtra(SeafilePathChooserActivity.DATA_REPO_ID, mSeafRepo.id);
//            intent.putExtra(SeafilePathChooserActivity.DATA_ACCOUNT, mAccount);
//        }
//        setResult(RESULT_OK, intent);
//
//    }
//}
