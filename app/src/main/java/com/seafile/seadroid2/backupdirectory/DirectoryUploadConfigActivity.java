package com.seafile.seadroid2.backupdirectory;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.view.ViewPager;
import android.text.TextUtils;

import com.google.gson.Gson;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;
import com.seafile.seadroid2.ui.fragment.SettingsFragment;

import java.util.ArrayList;
import java.util.List;


public class DirectoryUploadConfigActivity extends BaseActivity {
    public String DEBUG_TAG = "DirectoryUploadConfigActivity";
    public static final String BACKUP_SELECT_REPO = "backup_select_repo";
    public static final String BACKUP_SELECT_PATHS = "backup_select_paths";
    public static final String BACKUP_SELECT_PATHS_ON = "backup_select_paths_on";
    private ViewPager mViewPager;
    private DirectorySelectionFragment mBucketsFragment;
    private DirCloudLibraryFragment mCloudLibFragment;
    private SeafRepo mSeafRepo;
    private Account mAccount;
    private boolean isChooseDirPage;
    private boolean isChooseLibPage;
    private int mCurrentPosition;
    private UploadDirectoryDBHelper databaseHelper;
    private FileDirService fileDirService;
    private AccountManager accountMgr;
    private List<String> dbPaths;
    private Activity mActivity;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        setContentView(R.layout.folder_activity_layout);
        if (getSupportActionBar() != null)
            getSupportActionBar().hide();


        isChooseDirPage = getIntent().getBooleanExtra(SettingsFragment.FOLDER_UPLOAD_REMOTE_DIR, false);
        isChooseLibPage = getIntent().getBooleanExtra(SettingsFragment.FOLDER_UPLOAD_REMOTE_LIBRARY, false);
        mViewPager = (ViewPager) findViewById(R.id.cuc_pager);
        FragmentManager fm = getSupportFragmentManager();
        mViewPager.setAdapter(new CameraUploadConfigAdapter(fm));
        mViewPager.setOffscreenPageLimit(2);
        accountMgr = new AccountManager(this);
        databaseHelper = UploadDirectoryDBHelper.getDatabaseHelper();
        Intent bindIntent = new Intent(this, FileDirService.class);
        bindService(bindIntent, mDirConnection, Context.BIND_AUTO_CREATE);
        mActivity = this;
        String backupPaths = SettingsManager.instance().getBackupPaths();
        if (isChooseDirPage && !TextUtils.isEmpty(backupPaths)) {
            dbPaths = StringTools.getDataList(backupPaths);
        }


    }

    private ServiceConnection mDirConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName className, IBinder binder) {
            FileDirService.FileDirBinder fileDirBinder = (FileDirService.FileDirBinder) binder;
            fileDirService = fileDirBinder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName className) {
            fileDirService = null;
        }

    };


    public void saveDirUploadInfo(Account account, SeafRepo seafRepo) {
        mSeafRepo = seafRepo;
        mAccount = account;
    }


    public UploadDirectoryDBHelper getDatabaseHelper() {
        return databaseHelper;
    }


    public void setFilePathList(List<String> selectFileList) {
        this.dbPaths = selectFileList;

    }

    public List<String> getSelectFilePath() {
        return dbPaths;
    }

    public void saveSettings() {

        if (isChooseLibPage) {
            Intent intent = new Intent();
            // update cloud library data
            if (mSeafRepo != null && mAccount != null) {
                intent.putExtra(SeafilePathChooserActivity.DATA_REPO_NAME, mSeafRepo.name);
                intent.putExtra(SeafilePathChooserActivity.DATA_REPO_ID, mSeafRepo.id);
                intent.putExtra(SeafilePathChooserActivity.DATA_ACCOUNT, mAccount);
                intent.putExtra(BACKUP_SELECT_REPO, true);
                SettingsManager.instance().saveBackupEmail(mAccount.getEmail());
            }

            setResult(RESULT_OK, intent);
            boolean dirAutomaticUpload = SettingsManager.instance().isDirAutomaticUpload();
            if (dirAutomaticUpload && fileDirService != null) {
                fileDirService.uploadFile(mAccount.getEmail());
            }
        }

    }

    public void saveUpdateFolder() {
        if (isChooseDirPage) {
            String backupEmail = SettingsManager.instance().getBackupEmail();
            String strJsonPath = new Gson().toJson(dbPaths);
            SettingsManager.instance().saveBackupPaths(strJsonPath);
            Intent intent = new Intent();
            if (dbPaths != null) {
                intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, (ArrayList<String>) dbPaths);
                intent.putExtra(BACKUP_SELECT_PATHS_ON, true);
            }

            setResult(RESULT_OK, intent);
            boolean dirAutomaticUpload = SettingsManager.instance().isDirAutomaticUpload();
            if (dirAutomaticUpload && fileDirService != null) {
                fileDirService.uploadFile(backupEmail);
            }
        }

    }

    @Override
    public void onBackPressed() {
        if (mCurrentPosition == 0) {
            setResult(RESULT_CANCELED);
            super.onBackPressed();
        }
    }


    public boolean isChooseDirPage() {
        return isChooseDirPage;
    }


    class CameraUploadConfigAdapter extends FragmentStatePagerAdapter {

        public CameraUploadConfigAdapter(FragmentManager fm) {
            super(fm);
        }

        @Override
        public Fragment getItem(int position) {

            if (isChooseLibPage) {
                return position == 0 ? new DirCloudLibraryFragment() : null;
            }

            if (isChooseDirPage) {
                switch (position) {
                    case 0:
                        mBucketsFragment = new DirectorySelectionFragment();
                        return mBucketsFragment;
                    default:
                        return null;
                }

            }


            switch (position) {
                case 0:
                    mCloudLibFragment = new DirCloudLibraryFragment();
                    return mCloudLibFragment;
                case 1:
                    mBucketsFragment = new DirectorySelectionFragment();
                    return mBucketsFragment;
                default:
                    return null;
            }
        }

        @Override
        public int getCount() {
            if (isChooseLibPage || isChooseDirPage)
                return 1;
            else
                return 2;
        }
    }


    @Override
    protected void onDestroy() {
        if (fileDirService != null) {
            unbindService(mDirConnection);
            fileDirService = null;
        }
        super.onDestroy();
    }
}
