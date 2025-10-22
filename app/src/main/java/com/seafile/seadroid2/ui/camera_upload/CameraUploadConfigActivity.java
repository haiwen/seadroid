package com.seafile.seadroid2.ui.camera_upload;

import android.content.Intent;
import android.os.Bundle;
import android.util.Pair;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.tabs.TabLayoutMediator;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.databinding.CucActivityLayoutBinding;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.util.SystemSwitchUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.BucketsFragment;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.HowToUploadFragment;
import com.seafile.seadroid2.ui.camera_upload.config_fragment.WhatToUploadFragment;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.selector.obj.RepoSelectorFragment;

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

        applyEdgeToEdge(binding.getRoot());

        if (getSupportActionBar() != null)
            getSupportActionBar().hide();

        isChooseRepoPage = getIntent().getBooleanExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, false);
        isChooseDirPage = getIntent().getBooleanExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, false);

        initOnBackPressedDispatcher();

        initView();
    }

    private void initView() {
        fragmentList.clear();

        if (isChooseRepoPage) {
            fragmentList.add(RepoSelectorFragment.newInstance());
            binding.tabs.setVisibility(View.GONE);
        } else if (isChooseDirPage) {
            fragmentList.add(new BucketsFragment());
            binding.tabs.setVisibility(View.GONE);
        } else {
            fragmentList.add(RepoSelectorFragment.newInstance());
            fragmentList.add(new BucketsFragment());
            fragmentList.add(new WhatToUploadFragment());
            fragmentList.add(new HowToUploadFragment());
        }

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragmentList);
        binding.pager.setAdapter(viewPager2Adapter);

        if (isChooseRepoPage || isChooseDirPage) {
            binding.pager.setOffscreenPageLimit(1);
        } else {
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
        }

        binding.confirmButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                int size = fragmentList.size();
                if (size == 1) {
                    saveSettings();
                } else if (binding.pager.getCurrentItem() == 0) {
                    binding.pager.setCurrentItem(1);
                } else if (binding.pager.getCurrentItem() == size - 1) {
                    saveSettings();
                }
            }
        });

        binding.nextView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                binding.pager.setCurrentItem(binding.pager.getCurrentItem() + 1);
            }
        });

        checkLastPosition(0);
    }

    private void checkLastPosition(int position) {
        int size = fragmentList.size();
        if (size == 1) {
            binding.confirmButton.setVisibility(View.VISIBLE);
            binding.confirmButton.setText(R.string.confirm);
            binding.nextView.setVisibility(View.GONE);

        } else if (position == 0) {
            binding.confirmButton.setText(R.string.start);
            binding.confirmButton.setVisibility(View.GONE);

            binding.nextView.setVisibility(View.VISIBLE);
        } else if (position == size - 1) {
            binding.confirmButton.setText(R.string.confirm);
            binding.confirmButton.setVisibility(View.VISIBLE);

            binding.nextView.setVisibility(View.GONE);
        } else {
            binding.nextView.setVisibility(View.VISIBLE);
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
                boolean isAutoScan = bucketsFragment.isAutoScanSelected();
                if (isAutoScan || CollectionUtils.isEmpty(selectedBuckets)) {
                    AlbumBackupSharePreferenceHelper.writeBucketIds(null);
                    AlbumBackupSharePreferenceHelper.writeCustomAlbumSwitch(false);
                } else {
                    AlbumBackupSharePreferenceHelper.writeBucketIds(selectedBuckets);
                    AlbumBackupSharePreferenceHelper.writeCustomAlbumSwitch(true);
                }
            } else if (fragment instanceof RepoSelectorFragment repoSelectorFragment) {
                Pair<Account, RepoModel> pair = repoSelectorFragment.getBackupInfo();
                mAccount = pair.first;
                repoModel = pair.second;

                if (repoModel == null || mAccount == null) {
                    Toasts.show(R.string.settings_camera_upload_repo_hint);
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
