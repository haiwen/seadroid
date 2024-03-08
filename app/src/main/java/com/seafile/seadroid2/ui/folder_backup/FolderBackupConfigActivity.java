package com.seafile.seadroid2.ui.folder_backup;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.text.TextUtils;
import android.util.Pair;
import android.view.View;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FragmentUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.databinding.FolderBackupActivityLayoutBinding;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.ui.selector.ObjSelectorFragment;
import com.seafile.seadroid2.ui.selector.folder_selector.FolderSelectorFragment;
import com.seafile.seadroid2.ui.selector.folder_selector.StringTools;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;
import com.seafile.seadroid2.worker.FileSyncService;

import java.util.ArrayList;
import java.util.List;

public class FolderBackupConfigActivity extends BaseActivity {
    public static final String FOLDER_BACKUP_SELECT_MODE = "folder_backup_select_mode";
    public static final String BACKUP_SELECT_PATHS = "backup_select_paths";
    private RepoModel repoModel;
    private Account mAccount;
    private boolean isChooseFolderPage;
    private boolean isChooseRepoPage;

    private FileSyncService mBackupService;
    private List<String> selectFolderPaths;
    private Activity mActivity;
    private String originalBackupPaths;

    private FolderBackupActivityLayoutBinding binding;
    private FolderSelectorFragment folderSelectorFragment;
    private ObjSelectorFragment objSelectorFragment;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = FolderBackupActivityLayoutBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        if (getSupportActionBar() != null)
            getSupportActionBar().hide();

        String selectMode = getIntent().getStringExtra(FOLDER_BACKUP_SELECT_MODE);
        isChooseFolderPage = "folder".equals(selectMode);
        isChooseRepoPage = "repo".equals(selectMode);

        //bind service
        Intent bindIntent = new Intent(this, FileSyncService.class);
        bindService(bindIntent, mFolderBackupConnection, Context.BIND_AUTO_CREATE);

        mActivity = this;

        originalBackupPaths = FolderBackupConfigSPs.getBackupPaths();

        if (isChooseFolderPage && !TextUtils.isEmpty(originalBackupPaths)) {
            selectFolderPaths = StringTools.getJsonToList(originalBackupPaths);
        }

        if (isChooseRepoPage) {
            objSelectorFragment = new ObjSelectorFragment();
            FragmentUtils.add(getSupportFragmentManager(), objSelectorFragment, R.id.container);
        } else if (isChooseFolderPage) {
            folderSelectorFragment = new FolderSelectorFragment();
            FragmentUtils.add(getSupportFragmentManager(), folderSelectorFragment, R.id.container);
        }

        binding.confirmButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (folderSelectorFragment != null) {
                    saveFolderConfig();
                } else if (objSelectorFragment != null) {
                    saveRepoConfig();
                }
            }
        });
    }

    public void saveRepoConfig() {
        if (!isChooseRepoPage) {
            return;
        }

        Pair<Account, RepoModel> pair = objSelectorFragment.getCameraUploadInfo();
        mAccount = pair.first;
        repoModel = pair.second;

        //FIX an issue: When no folder or library is selected, a crash occurs
        if (null == repoModel || null == mAccount) {
            SLogs.d("----------No repo is selected");
            return;
        }

        //update sp
        RepoConfig repoConfig = FolderBackupConfigSPs.getBackupConfigByAccount(mAccount.getSignature());
        if (repoConfig != null) {
            repoConfig.setRepoName(repoModel.repo_name);
            repoConfig.setRepoID(repoModel.repo_id);
        } else {
            repoConfig = new RepoConfig(repoModel.repo_id, repoModel.repo_name, mAccount.getEmail(),mAccount.getSignature());
        }

        FolderBackupConfigSPs.setBackupRepoConfig(repoConfig);

        Intent intent = new Intent();
        intent.putExtra(ObjSelectorActivity.DATA_REPO_NAME, repoModel.repo_name);
        intent.putExtra(ObjSelectorActivity.DATA_REPO_ID, repoModel.repo_id);
        intent.putExtra(ObjSelectorActivity.DATA_ACCOUNT, mAccount);

        String selectMode = getIntent().getStringExtra(FOLDER_BACKUP_SELECT_MODE);
        intent.putExtra(FOLDER_BACKUP_SELECT_MODE, selectMode);

        setResult(RESULT_OK, intent);
        finish();
    }

    public void saveFolderConfig() {
        if (!isChooseFolderPage) {
            return;
        }
        selectFolderPaths = folderSelectorFragment.getSelectedPath();

        if (CollectionUtils.isEmpty(selectFolderPaths)) {
            SLogs.d("----------No folder is selected");

            //clear local storage
            FolderBackupConfigSPs.saveBackupPathsByCurrentAccount("");

            Intent intent = new Intent();
            String selectMode = getIntent().getStringExtra(FOLDER_BACKUP_SELECT_MODE);
            intent.putExtra(FOLDER_BACKUP_SELECT_MODE, selectMode);
            setResult(RESULT_OK, intent);
            return;
        }

        String backupEmail = FolderBackupConfigSPs.getBackupEmail();

        String strJsonPath = GsonUtils.toJson(selectFolderPaths);
        FolderBackupConfigSPs.saveBackupPathsByCurrentAccount(strJsonPath);

        Intent intent = new Intent();
        intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, (ArrayList<String>) selectFolderPaths);

        String selectMode = getIntent().getStringExtra(FOLDER_BACKUP_SELECT_MODE);
        intent.putExtra(FOLDER_BACKUP_SELECT_MODE, selectMode);

        setResult(RESULT_OK, intent);

        finish();
    }

    @Override
    public void onBackPressed() {

        if (folderSelectorFragment != null && folderSelectorFragment.onBackPressed()) {
            return;
        }

        setResult(RESULT_CANCELED);
        super.onBackPressed();
    }

    public boolean isChooseDirPage() {
        return isChooseFolderPage;
    }

    @Override
    protected void onDestroy() {
        if (mBackupService != null) {
            unbindService(mFolderBackupConnection);
            mBackupService = null;
        }
        super.onDestroy();
    }

    private final ServiceConnection mFolderBackupConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder binder) {
            FileSyncService.FileSyncBinder fileBackupBinder = (FileSyncService.FileSyncBinder) binder;
            mBackupService = fileBackupBinder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName className) {
            mBackupService = null;
        }

    };
}