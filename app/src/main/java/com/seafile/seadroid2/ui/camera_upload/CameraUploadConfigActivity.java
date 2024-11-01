package com.seafile.seadroid2.ui.camera_upload;

import android.content.Intent;
import android.os.Bundle;
import android.util.Pair;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.tabs.TabLayoutMediator;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.CucActivityLayoutBinding;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SystemSwitchUtils;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.BucketsFragment;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.ConfigWelcomeFragment;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.HowToUploadFragment;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.ReadyToScanFragment;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.WhatToUploadFragment;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.selector.ObjSelectorFragment;

import java.util.ArrayList;
import java.util.List;


/**
 * Camera upload configuration helper
 */
public class CameraUploadConfigActivity extends BaseActivity {
    public static final String CAMERA_UPLOAD_REMOTE_LIBRARY = "com.seafile.seadroid2.camera.upload.library";
    public static final String CAMERA_UPLOAD_LOCAL_DIRECTORIES = "com.seafile.seadroid2.camera.upload.directories";


    private RepoModel repoModel;
    private Account mAccount;

    /**
     * handling data from cloud library page
     */
    private boolean isChooseRepoPage;
    /**
     * handling data from local directory page
     */
    private boolean isChooseDirPage;
    private int mCurrentPosition;

    private CucActivityLayoutBinding binding;
    private final List<Fragment> fragmentList = new ArrayList<>();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = CucActivityLayoutBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        if (getSupportActionBar() != null)
            getSupportActionBar().hide();

        isChooseRepoPage = getIntent().getBooleanExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, false);
        isChooseDirPage = getIntent().getBooleanExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, false);

        initOnBackPressedDispatcher();

        initView();
    }

    private void initView() {
        fragmentList.clear();

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (isChooseRepoPage) {
            fragmentList.add(ObjSelectorFragment.newInstance(account));
            binding.tabs.setVisibility(View.GONE);
        } else if (isChooseDirPage) {
            fragmentList.add(new BucketsFragment());
            binding.tabs.setVisibility(View.GONE);
        } else {
            fragmentList.add(new ConfigWelcomeFragment());
            fragmentList.add(new HowToUploadFragment());
            fragmentList.add(new WhatToUploadFragment());
            fragmentList.add(new BucketsFragment());
            fragmentList.add(ObjSelectorFragment.newInstance(account));
            fragmentList.add(new ReadyToScanFragment());
        }

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragmentList);
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setOffscreenPageLimit(6);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                mCurrentPosition = position;

                checkLastPosition(position);
            }
        });

        new TabLayoutMediator(binding.tabs, binding.pager, (tab, position) -> {
        }).attach();

        binding.confirmButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                saveSettings();
            }
        });

        checkLastPosition(0);
    }

    private void checkLastPosition(int position) {
        int size = fragmentList.size();
        if (size == 1) {
            binding.confirmButton.setVisibility(View.VISIBLE);
        } else if (position == size - 1) {
            binding.confirmButton.setVisibility(View.VISIBLE);
        } else {
            binding.confirmButton.setVisibility(View.GONE);
        }
    }

    private void saveSettings() {

        for (Fragment fragment : fragmentList) {
            if (fragment instanceof HowToUploadFragment howToUploadFragment) {

                AlbumBackupSharePreferenceHelper.writeAllowDataPlanSwitch(howToUploadFragment.getHowToUpload());
            } else if (fragment instanceof WhatToUploadFragment whatToUploadFragment) {
                AlbumBackupSharePreferenceHelper.writeAllowVideoSwitch(whatToUploadFragment.getWhatToUpload());

            } else if (fragment instanceof BucketsFragment bucketsFragment) {
                List<String> selectedBuckets = bucketsFragment.getSelectedBuckets();
                if (bucketsFragment.isAutoScanSelected()) {
                    selectedBuckets.clear();
                }

                AlbumBackupSharePreferenceHelper.writeBucketIds(selectedBuckets);
            } else if (fragment instanceof ObjSelectorFragment cloudLibrarySelectorFragment) {
                Pair<Account, RepoModel> pair = cloudLibrarySelectorFragment.getBackupInfo();
                mAccount = pair.first;
                repoModel = pair.second;

                if (repoModel == null || mAccount == null) {
                    SLogs.d("----------No repo is selected");
                    return;
                }

                RepoConfig config = new RepoConfig(repoModel.repo_id, repoModel.repo_name, mAccount.email, mAccount.getSignature());
                AlbumBackupSharePreferenceHelper.writeRepoConfig(config);
            }
        }

        //TODO improve
//        CameraUploadManager.getInstance().setCameraAccount(mAccount);
        SystemSwitchUtils.getInstance(this).syncSwitchUtils();

        Intent intent = new Intent();
        intent.putExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, isChooseRepoPage);
        intent.putExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, isChooseDirPage);
        setResult(RESULT_OK, intent);
        finish();
    }

    private void initOnBackPressedDispatcher() {
        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (mCurrentPosition == 0) {
                    Intent intent = new Intent();
                    intent.putExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, isChooseDirPage);
                    intent.putExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, isChooseRepoPage);
                    setResult(RESULT_CANCELED, intent);
                    finish();
                } else {
                    // navigate to previous page when press back button
                    mCurrentPosition -= 1;
                    binding.pager.setCurrentItem(mCurrentPosition);
                }
            }
        });
    }


    public boolean isChooseRepoPage() {
        return isChooseRepoPage;
    }

    public boolean isChooseDirPage() {
        return isChooseDirPage;
    }
}
