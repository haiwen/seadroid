package com.seafile.seadroid2.ui.folder_backup;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;

import androidx.activity.OnBackPressedCallback;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FragmentUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.FolderBackupActivityLayoutBinding;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.selector.folder_selector.FolderSelectorFragment;
import com.seafile.seadroid2.ui.settings.TabSettings2Fragment;

import java.util.ArrayList;
import java.util.List;

public class FolderBackupConfigActivity extends BaseActivity {
    public static final String BACKUP_SELECT_PATHS = "backup_select_paths";
    private List<String> initSelectedFolderPaths;

    private FolderBackupActivityLayoutBinding binding;
    private FolderSelectorFragment folderSelectorFragment;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = FolderBackupActivityLayoutBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());

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
                saveFolderConfig();
            }
        });

        initFragment();
    }

    private void initFragment() {
        initSelectedFolderPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();

        folderSelectorFragment = new FolderSelectorFragment();
        FragmentUtils.add(getSupportFragmentManager(), folderSelectorFragment, R.id.container);
    }


    public void saveFolderConfig() {
        Intent intent = new Intent();

        //set select type
        intent.putExtra(TabSettings2Fragment.FB_SELECT_TYPE, "folder");

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