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

import com.blankj.utilcode.util.CollectionUtils;
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
    private List<DirentModel> dataList = CollectionUtils.newArrayList();
    private boolean isDataOperated = false;

    public static Intent startThisFromRepo(Context context, DirentModel direntModel, ArrayList<DirentModel> direntModels) {
        Intent intent = new Intent(context, ImagePreviewActivity.class);
        intent.putExtra("dirent", direntModel);
        intent.putParcelableArrayListExtra("dirent_list", direntModels);
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

        changeBarStatus(false);

        binding = ActivityImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        initData();

        initView();
        initViewModel();
        initAdapter();

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

    private void changeBarStatus(boolean isShow) {
        WindowCompat.setDecorFitsSystemWindows(getWindow(), false);

        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.R) {
            WindowInsetsController controller = getWindow().getInsetsController();
            if (controller != null) {
                if (!isShow) {
                    controller.hide(WindowInsets.Type.statusBars());
                    controller.hide(WindowInsets.Type.navigationBars());
                } else {
                    controller.show(WindowInsets.Type.statusBars());
                    controller.show(WindowInsets.Type.navigationBars());
                }
            }
        } else {
            WindowInsetsControllerCompat controllerCompat = ViewCompat.getWindowInsetsController(binding.getRoot());
            if (controllerCompat != null) {
                if (!isShow) {
                    controllerCompat.hide(WindowInsetsCompat.Type.statusBars());
                    controllerCompat.hide(WindowInsetsCompat.Type.navigationBars());
                } else {
                    controllerCompat.show(WindowInsetsCompat.Type.statusBars());
                    controllerCompat.show(WindowInsetsCompat.Type.navigationBars());
                }
            }
        }

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

        if (getIntent().hasExtra("dirent_list")) {
            ArrayList<DirentModel> ds = getIntent().getParcelableArrayListExtra("dirent_list");
            dataList.addAll(ds);
        }

        if (CollectionUtils.isEmpty(dataList)) {
            dataList.add(currentDirent);
        }
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

    private void initAdapter() {
        adapter = new ViewPager2Adapter(this);
        binding.pager.setAdapter(adapter);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);

                if (CollectionUtils.isEmpty(dataList)) {
                    binding.galleryPageIndex.setText("1/1");
                    return;
                }

                String fs = String.format(Locale.getDefault(), "%d/%d", (position + 1), dataList.size());
                binding.galleryPageIndex.setText(fs);
            }
        });
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

                boolean isStar = dataList.get(binding.pager.getCurrentItem()).starred;
                dataList.get(binding.pager.getCurrentItem()).starred = !isStar;

                ToastUtils.showLong(aBoolean ? R.string.star_file_succeed : R.string.star_file_failed);
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

                notifyFragmentList(dataList);
            }
        });
    }

    private void notifyFragmentList(List<DirentModel> direntModels) {
        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        List<Fragment> fragments = new ArrayList<>();
        for (DirentModel direntModel : direntModels) {
            PhotoFragment photoFragment = PhotoFragment.newInstance(
                    direntModel.repo_id,
                    direntModel.repo_name,
                    direntModel.full_path);
            photoFragment.setOnPhotoTapListener((view, x, y) -> hideOrShowToolBar());

            fragments.add(photoFragment);
        }

        adapter.addFragments(fragments);
        adapter.notifyDataSetChanged();

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
        int size = dataList.size();
        int mPageIndex = -1;
        for (int i = 0; i < size; i++) {
            if (dataList.get(i).name.equals(currentDirent.name)) {
                binding.pager.setCurrentItem(i);
                binding.galleryPageIndex.setText(String.valueOf(i + 1));
                mPageIndex = i;
                break;
            }
        }

        if (mPageIndex != -1) {
            binding.galleryPageIndex.setText(String.format(Locale.getDefault(), "%d/%d", (mPageIndex + 1), size));
        }
    }

    private DirentModel getSelectedDirent() {
        return dataList.get(binding.pager.getCurrentItem());
    }

    private void deleteFile() {

        int curItem = binding.pager.getCurrentItem();

        DeleteFileDialogFragment dialogFragment = DeleteFileDialogFragment.newInstance();

        DirentModel direntModel = getSelectedDirent();
        dialogFragment.initData(direntModel);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    isDataOperated = true;

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
