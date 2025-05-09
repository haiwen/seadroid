package com.seafile.seadroid2.ui.folder_backup;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;
import android.view.View;

import androidx.activity.OnBackPressedCallback;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FragmentUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.FolderBackupActivityLayoutBinding;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.ui.selector.ObjSelectorFragment;
import com.seafile.seadroid2.ui.selector.folder_selector.FolderSelectorFragment;

import java.util.ArrayList;
import java.util.List;

public class FolderBackupConfigActivity extends BaseActivity {
    public static final String FOLDER_BACKUP_SELECT_TYPE = "folder_backup_select_type";
    public static final String BACKUP_SELECT_PATHS = "backup_select_paths";

    private boolean isChooseFolderPage;
    private boolean isChooseRepoPage;

    private List<String> initSelectedFolderPaths;
    private Account mAccount;
    private String initRepoId, initAccount;

    private FolderBackupActivityLayoutBinding binding;
    private FolderSelectorFragment folderSelectorFragment;
    private ObjSelectorFragment objSelectorFragment;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = FolderBackupActivityLayoutBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        if (getSupportActionBar() != null) {
            getSupportActionBar().hide();
        }

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (folderSelectorFragment != null && folderSelectorFragment.onBackPressed()) {
                    return;
                }

                setResult(RESULT_CANCELED);
                finish();
            }
        });


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

        initFragment();
    }

    private void initFragment() {
        String selectMode = getIntent().getStringExtra(FOLDER_BACKUP_SELECT_TYPE);
        isChooseFolderPage = "folder".equals(selectMode);
        isChooseRepoPage = "repo".equals(selectMode);

        if (isChooseRepoPage) {
            RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
            if (null != repoConfig) {
                initRepoId = repoConfig.getRepoId();
                initAccount = repoConfig.getSignature();
            }

            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            objSelectorFragment = ObjSelectorFragment.newInstance(account);
            FragmentUtils.add(getSupportFragmentManager(), objSelectorFragment, R.id.container);

        } else if (isChooseFolderPage) {
            initSelectedFolderPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();

            folderSelectorFragment = new FolderSelectorFragment();
            FragmentUtils.add(getSupportFragmentManager(), folderSelectorFragment, R.id.container);
        }
    }

    public void saveRepoConfig() {
        if (!isChooseRepoPage) {
            return;
        }

        Pair<Account, RepoModel> pair = objSelectorFragment.getBackupInfo();
        mAccount = pair.first;
        RepoModel selectedRepo = pair.second;

        Intent intent = new Intent();

        String selectType = getIntent().getStringExtra(FOLDER_BACKUP_SELECT_TYPE);
        intent.putExtra(FOLDER_BACKUP_SELECT_TYPE, selectType);

        if (TextUtils.isEmpty(initRepoId) && null == selectedRepo) {
            setResult(RESULT_CANCELED, intent);
        } else if (!TextUtils.isEmpty(initRepoId) && null == selectedRepo) {
            setResult(RESULT_OK, intent);//just clear the repo config
        } else if (TextUtils.isEmpty(initRepoId) && null != selectedRepo) {
            intent.putExtra(ObjSelectorActivity.DATA_REPO_NAME, selectedRepo.repo_name);
            intent.putExtra(ObjSelectorActivity.DATA_REPO_ID, selectedRepo.repo_id);
            intent.putExtra(ObjSelectorActivity.DATA_ACCOUNT, mAccount);
            setResult(RESULT_OK, intent);
        } else if (!TextUtils.isEmpty(initRepoId) && null != selectedRepo) {
            if (TextUtils.equals(selectedRepo.repo_id, initRepoId)) {
                setResult(RESULT_CANCELED, intent);
            } else {
                intent.putExtra(ObjSelectorActivity.DATA_REPO_NAME, selectedRepo.repo_name);
                intent.putExtra(ObjSelectorActivity.DATA_REPO_ID, selectedRepo.repo_id);
                intent.putExtra(ObjSelectorActivity.DATA_ACCOUNT, mAccount);
                setResult(RESULT_OK, intent);
            }
        }


        finish();
    }

    public void saveFolderConfig() {
        if (!isChooseFolderPage) {
            return;
        }


        Intent intent = new Intent();

        //set select type
        String selectMode = getIntent().getStringExtra(FOLDER_BACKUP_SELECT_TYPE);
        intent.putExtra(FOLDER_BACKUP_SELECT_TYPE, selectMode);

        List<String> selectedFolderPaths = folderSelectorFragment.getSelectedPath();
        if (CollectionUtils.isEmpty(selectedFolderPaths) && CollectionUtils.isEmpty(initSelectedFolderPaths)) {
            intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, null);
            setResult(RESULT_CANCELED, intent);

        } else if (CollectionUtils.isEmpty(selectedFolderPaths) && !CollectionUtils.isEmpty(initSelectedFolderPaths)) {
            intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, null);
            setResult(RESULT_OK, intent);

        } else if (!CollectionUtils.isEmpty(selectedFolderPaths) && CollectionUtils.isEmpty(initSelectedFolderPaths)) {
            intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, (ArrayList<String>) selectedFolderPaths);
            setResult(RESULT_OK, intent);

        } else {
            intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, (ArrayList<String>) selectedFolderPaths);

            boolean isChanged = !selectedFolderPaths.equals(initSelectedFolderPaths);
            setResult(isChanged ? RESULT_OK : RESULT_CANCELED, intent);
        }

        finish();
    }

}