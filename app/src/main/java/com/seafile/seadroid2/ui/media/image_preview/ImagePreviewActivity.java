package com.seafile.seadroid2.ui.media.image_preview;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.WindowInsets;
import android.view.WindowInsetsController;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Nullable;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowInsetsControllerCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.databinding.ActivityImagePreviewBinding;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import io.reactivex.functions.Consumer;

public class ImagePreviewActivity extends BaseActivityWithVM<ImagePreviewViewModel> {
    private ActivityImagePreviewBinding binding;
    private ViewPager2Adapter adapter;
    private DirentModel currentDirent;
    private List<DirentModel> direntList;
    private boolean isDataOperated = false;

    public static Intent startThisFromRepo(Context context, DirentModel direntModel) {
        Intent intent = new Intent(context, ImagePreviewActivity.class);
        intent.putExtra("dirent", direntModel);
        intent.putExtra("query_db", true);
        return intent;
    }

    public static Intent startThisFromStarred(Context context, StarredModel starredModel) {
        Intent intent = new Intent(context, ImagePreviewActivity.class);
        intent.putExtra("dirent", StarredModel.converterThis2DirentModel(starredModel));
        return intent;
    }

    public static Intent startThisFromActivity(Context context, ActivityModel starredModel) {
        Intent intent = new Intent(context, ImagePreviewActivity.class);
        intent.putExtra("dirent", ActivityModel.converterThis2DirentModel(starredModel));
        return intent;
    }


    public static void startThisFromSearch(Context context, SearchModel starredModel) {
        Intent intent = new Intent(context, ImagePreviewActivity.class);
        intent.putExtra("dirent", SearchModel.converterThis2DirentModel(starredModel));
        context.startActivity(intent);
    }


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        BarUtils.setNavBarVisibility(this, false);
        BarUtils.setStatusBarVisibility(this, false);

        initData();

        initView();
        initViewModel();

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (isDataOperated) {
                    setResult(RESULT_OK);
                }
                finish();
            }
        });
    }

    @Override
    protected void onPostCreate(@Nullable Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);

        loadData();
    }

    private void initData() {

        if (getIntent() == null) {
            throw new IllegalArgumentException("Intent is null");
        }

        currentDirent = getIntent().getParcelableExtra("dirent");
    }

    private void initView() {
        View.OnClickListener onClickListener = v -> {
            int id = v.getId();
            if (id == R.id.gallery_download_photo) {
                downloadFile();
            } else if (id == R.id.gallery_delete_photo) {
                deleteFile();
            } else if (id == R.id.gallery_star_photo) {
                starFile();
            } else if (id == R.id.gallery_share_photo) {
                shareFile();
            }
        };

        binding.galleryDownloadPhoto.setOnClickListener(onClickListener);
        binding.galleryDeletePhoto.setOnClickListener(onClickListener);
        binding.galleryStarPhoto.setOnClickListener(onClickListener);
        binding.gallerySharePhoto.setOnClickListener(onClickListener);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showProgressDialog();
                } else {
                    dismissProgressDialog();
                }
            }
        });

        getViewModel().getStarLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    isDataOperated = true;
                }
                ToastUtils.showLong(aBoolean ? R.string.star_file_succeed : R.string.star_file_failed);
            }
        });

        getViewModel().getListLiveData().observe(this, new Observer<List<DirentModel>>() {
            @Override
            public void onChanged(List<DirentModel> direntModels) {

                if (CollectionUtils.isEmpty(direntModels)) {
                    direntModels = CollectionUtils.newArrayList(currentDirent);
                }
                direntList = direntModels;

                notifyFragmentList();
            }
        });
    }

    private void loadData() {
        getViewModel().getRepoModelFromDB(currentDirent.repo_id, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) {
                if (!repoModel.hasWritePermission()) {
                    binding.galleryDeletePhoto.setVisibility(View.GONE);
                }

                getViewModel().loadData(currentDirent.repo_id, currentDirent.parent_dir);

            }
        });
    }

    private void notifyFragmentList() {
        if (CollectionUtils.isEmpty(direntList)) {
            return;
        }

        adapter = new ViewPager2Adapter(this);
        List<Fragment> fragments = new ArrayList<>();
        for (DirentModel direntModel : direntList) {
            PhotoFragment photoFragment = PhotoFragment.newInstance(direntModel);
            photoFragment.setOnPhotoTapListener((view, x, y) -> hideOrShowToolBar());
            fragments.add(photoFragment);
        }

        adapter.addFragments(fragments);

        binding.pager.setAdapter(adapter);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);

                String fs = String.format(Locale.ROOT, "%d/%d", (position + 1), direntList.size());
                binding.galleryPageIndex.setText(fs);

                DirentModel model = direntList.get(position);
                binding.galleryPageName.setText(model.name);
            }
        });

        navToSelectedPage();
    }

    private boolean showToolBar = false;

    private void hideOrShowToolBar() {
        binding.galleryToolBar.setVisibility(!showToolBar ? View.VISIBLE : View.GONE);
        binding.pageIndexContainer.setVisibility(!showToolBar ? View.VISIBLE : View.GONE);
        showToolBar = !showToolBar;
    }

    /**
     * Dynamically navigate to the starting page index selected by user
     * by default the starting page index is 0
     */
    private void navToSelectedPage() {
        int size = direntList.size();
        int mPageIndex = -1;
        for (int i = 0; i < size; i++) {
            if (direntList.get(i).name.equals(currentDirent.name)) {
                binding.pager.setCurrentItem(i);
                binding.galleryPageIndex.setText(String.valueOf(i + 1));
                mPageIndex = i;
                break;
            }
        }

        if (mPageIndex != -1) {
            binding.galleryPageIndex.setText(String.format(Locale.ROOT, "%d/%d", (mPageIndex + 1), size));
        }
    }

    private DirentModel getSelectedDirent() {
        int index = binding.pager.getCurrentItem();
        return direntList.get(index);
    }

    private void deleteFile() {

        int position = binding.pager.getCurrentItem();

        DeleteFileDialogFragment dialogFragment = DeleteFileDialogFragment.newInstance();

        DirentModel direntModel = getSelectedDirent();
        dialogFragment.initData(direntModel);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    isDataOperated = true;

                    ToastUtils.showLong(R.string.delete_successful);
                    adapter.removeFragment(position);
                    adapter.notifyItemRemoved(position);

                    if (adapter.getItemCount() == 0) {
                        setResult(RESULT_OK);
                        finish();
                    } else {
                        String fs = String.format(Locale.ROOT, "%d/%d", (position + 1), adapter.getFragments().size());
                        binding.galleryPageIndex.setText(fs);
                    }
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), DeleteFileDialogFragment.class.getSimpleName());
    }

    private void starFile() {
        if (!NetworkUtils.isConnected()) {
            ToastUtils.showLong(R.string.network_down);
            return;
        }

        DirentModel direntModel = getSelectedDirent();
        if (direntModel.starred) {
            getViewModel().unStar(direntModel.repo_id, direntModel.full_path);
        } else {
            getViewModel().star(direntModel.repo_id, direntModel.full_path);
        }
    }

    private void shareFile() {
        DirentModel direntModel = getSelectedDirent();
        Objs.showCreateShareLinkDialog(this, getSupportFragmentManager(), direntModel, false);
    }


    private void downloadFile() {
        isDataOperated = true;

        DirentModel direntModel = getSelectedDirent();
        getViewModel().download(direntModel.repo_id, direntModel.full_path);
    }


    private Dialog dialog;

    private void showProgressDialog() {
        if (dialog == null) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setView(R.layout.layout_dialog_progress_bar);
            dialog = builder.create();
        }

        if (!dialog.isShowing()) {
            dialog.show();
        }
    }

    private void dismissProgressDialog() {
        if (dialog != null) {
            dialog.dismiss();
        }
    }
}
