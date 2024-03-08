package com.seafile.seadroid2.ui.media.image_preview;

import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.view.WindowInsets;
import android.view.WindowInsetsController;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.databinding.ActivityImagePreviewBinding;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.util.Objs;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;

public class ImagePreviewActivity extends BaseActivity {
    private ActivityImagePreviewBinding binding;
    private ImagePreviewViewModel viewModel;
    private ViewPager2Adapter adapter;
    private String repoId, fullPath, parentPath, fileName;
    private List<DirentModel> dataList;

    public static void startThis(Context context, String repoId, String fullPath) {
        Intent intent = new Intent(context, ImagePreviewActivity.class);
        intent.putExtra("repoId", repoId);
        intent.putExtra("fullPath", fullPath);
        context.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        //TODO
        // 在 Android 11 及更高版本中，使用 WindowInsetsController 来隐藏状态栏和导航栏
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            final WindowInsetsController controller = getWindow().getInsetsController();
            if (controller != null) {
                controller.hide(WindowInsets.Type.statusBars() | WindowInsets.Type.navigationBars());
                controller.setSystemBarsBehavior(WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
            }
        } else {
            getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_FULLSCREEN | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION);
        }

        binding = ActivityImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        initData();

        initView();
        initViewModel();
        initAdapter();

        loadData();
    }

    private void initData() {
        if (getIntent() == null) {
            throw new IllegalArgumentException("Intent is null");
        }

        repoId = getIntent().getStringExtra("repoId");
        fullPath = getIntent().getStringExtra("fullPath");
        parentPath = Utils.getParentPath(fullPath);
        fileName = Utils.getFileNameFromPath(fullPath);
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

    private int mPageIndex;

    private void initAdapter() {
        adapter = new ViewPager2Adapter(this);
        binding.pager.setAdapter(adapter);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);

                // page index starting from 1 instead of 0 in user interface, so plus one here
                binding.galleryPageIndex.setText(String.valueOf(position + 1));
                mPageIndex = position;
                // fixed IndexOutOfBoundsException when accessing list
                if (mPageIndex == dataList.size()) return;
                fileName = dataList.get(mPageIndex).name;

                binding.galleryPageName.setText(fileName);
            }
        });
    }

    private void initViewModel() {
        viewModel = new ViewModelProvider(this).get(ImagePreviewViewModel.class);
        viewModel.getListLiveData().observe(this, new Observer<List<DirentModel>>() {
            @Override
            public void onChanged(List<DirentModel> direntModels) {
                notifyFragmentList(direntModels);
            }
        });

        viewModel.getStarLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                ToastUtils.showLong(aBoolean ? R.string.star_file_succeed : R.string.star_file_failed);
            }
        });
    }

    private void loadData() {
        viewModel.loadData(repoId, parentPath);
    }

    private void notifyFragmentList(List<DirentModel> direntModels) {
        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        dataList = direntModels;

        List<Fragment> fragments = new ArrayList<>();
        for (DirentModel direntModel : direntModels) {
            PhotoFragment photoFragment = PhotoFragment.newInstance(
                    direntModel.repo_id,
                    direntModel.parent_dir,
                    direntModel.name);
            photoFragment.setOnPhotoTapListener((view, x, y) -> hideOrShowToolBar());

            fragments.add(photoFragment);
        }

        adapter.addFragments(fragments);
        adapter.notifyDataSetChanged();

        navToSelectedPage();
    }

    private boolean showToolBar = false;

    /**
     * This method will get called when tapping at the center of a photo,
     * tool bar will auto hide when open the gallery,
     * and will show or hide alternatively when tapping.
     */
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
        int size = dataList.size();

        for (int i = 0; i < size; i++) {
            if (dataList.get(i).name.equals(fileName)) {
                binding.pager.setCurrentItem(i);
                binding.galleryPageIndex.setText(String.valueOf(i + 1));
                mPageIndex = i;
                binding.galleryPageName.setText(fileName);
                break;
            }
        }
        binding.galleryPageCount.setText(String.valueOf(size));
    }

    private String getCurFullPath() {
        return dataList.get(binding.pager.getCurrentItem()).full_path;
    }

    private void deleteFile() {
        String curPath = getCurFullPath();
        int curItem = binding.pager.getCurrentItem();

        DirentModel direntModel = new DirentModel();
        direntModel.repo_id = repoId;
        direntModel.full_path = curPath;

        DeleteFileDialogFragment dialogFragment = DeleteFileDialogFragment.newInstance();
        dialogFragment.initData(direntModel);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    dataList.remove(curItem);
                    adapter.removeFragment(curItem);
                    adapter.notifyItemRemoved(curItem);

                    ToastUtils.showLong(R.string.delete_successful);
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), DeleteFileDialogFragment.class.getSimpleName());
    }

    private void starFile() {
        if (!Utils.isNetworkOn()) {
            ToastUtils.showLong(R.string.network_down);
            return;
        }

        viewModel.star(repoId, getCurFullPath());
    }

    private void shareFile() {
        DirentModel direntModel = new DirentModel();
        direntModel.repo_id = repoId;
        direntModel.full_path = fullPath;
        direntModel.type = "file";

        Objs.showCreateEncryptShareLinkDialog(this, getSupportFragmentManager(), direntModel, false);
    }

    private void downloadFile() {
        //TODO
        ToastUtils.showLong("TODO: 下载文件");
//        progressDialog = ProgressDialog.show(this, "", getString(R.string.notification_download_started_title));
//        final String filePath = Utils.pathJoin(dirPath, fileName);
//        GallerySeeOriginals(repoName, repoID, filePath);
    }
}
