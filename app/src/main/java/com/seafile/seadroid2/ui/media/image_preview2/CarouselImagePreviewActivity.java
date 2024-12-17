package com.seafile.seadroid2.ui.media.image_preview2;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.databinding.ActivityCarouselImagePreviewBinding;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.data.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.CopyMoveDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.file_profile.FileProfileDialog;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewViewModel;
import com.seafile.seadroid2.ui.media.image_preview.PhotoFragment;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class CarouselImagePreviewActivity extends BaseActivityWithVM<ImagePreviewViewModel> implements Toolbar.OnMenuItemClickListener {
    private ActivityCarouselImagePreviewBinding binding;

    private ViewPager2Adapter adapter;
    private CarouselAdapter carouselAdapter;

    private List<DirentModel> direntList;
    private List<DirentModel> carouselDirentList;
    private boolean isLightMode = true;
    private boolean isDataOperated = false;

    private String repoId, repoName, parentDir, name;
    private boolean load_other_images_in_same_directory = false;

//    public static Intent startThisFromDocsComment(Context context, String url) {
//        Intent intent = new Intent(context, CarouselImagePreviewActivity.class);
//        intent.putExtra("image_url", url);//Load other images in the same folder
//        intent.putExtra("load_other_images_in_same_directory", false);//Load other images in the same folder
//        return intent;
//    }

    public static Intent startThisFromObjs(Context context, DirentModel direntModel) {
        Intent intent = new Intent(context, CarouselImagePreviewActivity.class);
        intent.putExtra("repo_id", direntModel.repo_id);
        intent.putExtra("repo_name", direntModel.repo_name);
        intent.putExtra("parent_dir", direntModel.parent_dir);
        intent.putExtra("name", direntModel.name);
        intent.putExtra("load_other_images_in_same_directory", true);//Load other images in the same folder
        return intent;
    }

    public static Intent startThisFromStarred(Context context, StarredModel model) {
        Intent intent = new Intent(context, CarouselImagePreviewActivity.class);
        intent.putExtra("repo_id", model.repo_id);
        intent.putExtra("repo_name", model.repo_name);
        intent.putExtra("parent_dir", Utils.getParentPath(model.path));
        intent.putExtra("name", model.obj_name);
        intent.putExtra("load_other_images_in_same_directory", false);//Load other images in the same folder
        return intent;
    }

    public static Intent startThisFromActivity(Context context, ActivityModel model) {
        Intent intent = new Intent(context, CarouselImagePreviewActivity.class);
        intent.putExtra("repo_id", model.repo_id);
        intent.putExtra("repo_name", model.repo_name);
        intent.putExtra("parent_dir", Utils.getParentPath(model.path));
        intent.putExtra("name", model.name);
        intent.putExtra("load_other_images_in_same_directory", false);//Load other images in the same folder
        return intent;
    }

    public static Intent startThisFromSearch(Context context, SearchModel model) {
        Intent intent = new Intent(context, CarouselImagePreviewActivity.class);
        intent.putExtra("repo_id", model.repo_id);
        intent.putExtra("repo_name", model.repo_name);
        intent.putExtra("parent_dir", Utils.getParentPath(model.fullpath));
        intent.putExtra("name", model.name);
        intent.putExtra("load_other_images_in_same_directory", false);//Load other images in the same folder
        return intent;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityCarouselImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        // full screen
        getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN | View.SYSTEM_UI_FLAG_LAYOUT_STABLE);

        //init status bar
        int color = ContextCompatKt.getColorCompat(this, R.color.material_grey_100_translucent);
        BarUtils.setStatusBarColor(this, color);
        BarUtils.setStatusBarLightMode(this, isLightMode);

        //init toolbar margin top
        ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams) binding.toolbarActionbar.getLayoutParams();
        layoutParams.topMargin = BarUtils.getStatusBarHeight();

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(null);

            toolbar.setNavigationOnClickListener(v -> {
                if (isDataOperated) {
                    setResult(RESULT_OK);
                }
                finish();
            });
        }

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (isDataOperated) {
                    setResult(RESULT_OK);
                }
                finish();
            }
        });

        initParams();
        initView();


        initAdapter();
        initCarouselAdapter();
        bindPager();

        initViewModel();

        getViewModel().load(repoId, parentDir, name, load_other_images_in_same_directory);
    }

    private void initParams() {
        Intent intent = getIntent();
        if (intent == null) {
            throw new RuntimeException("intent is null");
        }

        repoId = intent.getStringExtra("repo_id");
        if (TextUtils.isEmpty(repoId)) {
            throw new RuntimeException("repoId is empty");
        }


        repoName = intent.getStringExtra("repo_name");
        parentDir = intent.getStringExtra("parent_dir");
        name = intent.getStringExtra("name");

        load_other_images_in_same_directory = intent.getBooleanExtra("load_other_images_in_same_directory", false);
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

        getViewModel().getRepoAndListLiveData().observe(this, new Observer<Pair<RepoModel, List<DirentModel>>>() {
            @Override
            public void onChanged(Pair<RepoModel, List<DirentModel>> pair) {
                submitData(pair);
            }
        });

        getViewModel().getStarLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                isDataOperated = true;

                ToastUtils.showLong(aBoolean ? R.string.star_file_succeed : R.string.star_file_failed);

                int index = binding.pager.getCurrentItem();
                direntList.get(index).starred = aBoolean;

                notifyCurrentStarredStatus();
            }
        });

        getViewModel().getFileDetailLiveData().observe(this, new Observer<FileProfileConfigModel>() {
            @Override
            public void onChanged(FileProfileConfigModel configModel) {

                DirentModel direntModel = getSelectedDirent();
                if (direntModel == null) {
                    return;
                }

                String key = direntModel.full_path;
                fileDetailHashMap.put(key, configModel);

                showProfileDialog(configModel);
            }
        });
    }

    private void initAdapter() {
        adapter = new ViewPager2Adapter(this);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                notifyCurrentStarredStatus();
            }
        });
        binding.pager.setAdapter(adapter);
    }

    private final LinearLayoutManager layoutManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
    private final GravitySnapHelper gravitySnapHelper = new GravitySnapHelper(Gravity.CENTER);

    private void initCarouselAdapter() {
        carouselAdapter = new CarouselAdapter(this, new CarouselAdapter.CarouselItemListener() {
            @Override
            public void onItemClicked(DirentModel item, int snapPosition) {
                gravitySnapHelper.smoothScrollToPosition(snapPosition);
            }
        });

        binding.recyclerView.setAdapter(carouselAdapter);
        binding.recyclerView.setLayoutManager(layoutManager);

        binding.recyclerView.addOnScrollListener(new CenterScaleXYRecyclerViewScrollListener(this));

        gravitySnapHelper.attachToRecyclerView(binding.recyclerView);
    }

    private void bindPager() {
        PagerSnapBinders.bind(binding.pager, gravitySnapHelper);
    }

    private void submitData(Pair<RepoModel, List<DirentModel>> pair) {
        if (pair == null) {
            return;
        }

        RepoModel repoModel = pair.first;
        if (repoModel == null) {
            return;
        }

        if (!repoModel.hasWritePermission()) {
            binding.galleryDeletePhoto.setVisibility(View.GONE);
        }

        direntList = pair.second;
        //

        List<Fragment> fragments = new ArrayList<>();
        for (DirentModel direntModel : direntList) {
            PhotoFragment photoFragment = PhotoFragment.newInstance(direntModel);
            photoFragment.setOnPhotoTapListener((view, x, y) -> hideOrShowToolBar());
            fragments.add(photoFragment);
        }

        adapter.addFragments(fragments);
        adapter.notifyItemRangeInserted(0, direntList.size());

        carouselDirentList = new ArrayList<>();
        carouselDirentList.add(new DirentModel());
        carouselDirentList.addAll(direntList);
        carouselDirentList.add(new DirentModel());

        carouselAdapter.submitList(carouselDirentList);

        binding.recyclerView.postDelayed(new Runnable() {
            @Override
            public void run() {
                navToSelectedPage();
            }
        }, 100);
    }

    private void hideOrShowToolBar() {
        binding.galleryToolBar.setVisibility(isLightMode ? View.GONE : View.VISIBLE);
        binding.recyclerView.setVisibility(isLightMode ? View.GONE : View.VISIBLE);
        binding.toolbarActionbar.setVisibility(isLightMode ? View.INVISIBLE : View.VISIBLE);

        if (isLightMode) {
            binding.pager.setBackgroundColor(ContextCompatKt.getColorCompat(this, R.color.material_grey_919));
        } else {
            binding.pager.setBackgroundColor(ContextCompatKt.getColorCompat(this, R.color.material_grey_100));
        }

        int color = ContextCompatKt.getColorCompat(this, isLightMode ? R.color.material_grey_919_ff : R.color.material_grey_100_translucent);
        BarUtils.setNavBarColor(this, color);
        BarUtils.setStatusBarColor(this, color);
        BarUtils.setStatusBarLightMode(this, !isLightMode);
        BarUtils.setNavBarLightMode(this, !isLightMode);

        isLightMode = !isLightMode;
    }

    /**
     * Dynamically navigate to the starting page index selected by user
     * by default the starting page index is 0
     */
    private void navToSelectedPage() {
        int size = direntList.size();
        int index = -1;

        for (int i = 0; i < size; i++) {
            if (direntList.get(i).name.equals(name)) {
                index = i;
                break;
            }
        }

        if (index != -1) {
            binding.pager.setCurrentItem(index);
        }
    }

    private void notifyCurrentStarredStatus() {
        DirentModel direntModel = getSelectedDirent();
        if (direntModel == null) {
            return;
        }

        if (direntModel.starred) {
            binding.galleryStarPhoto.setImageResource(R.drawable.baseline_star_filled_24);
        } else {
            binding.galleryStarPhoto.setImageResource(R.drawable.baseline_star_outline_24);
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        Toolbar toolbar = getActionBarToolbar();
        toolbar.inflateMenu(R.menu.menu_image_list_preview);
        toolbar.setOnMenuItemClickListener(this);

        return true;
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
        } else if (item.getItemId() == R.id.copy) {
            copy();
        } else if (item.getItemId() == R.id.info) {
            preShowProfileDialog();
        }

        return super.onOptionsItemSelected(item);
    }

    private DirentModel getSelectedDirent() {
        if (CollectionUtils.isEmpty(direntList)) {
            return null;
        }

        int index = binding.pager.getCurrentItem();
        if (index > direntList.size() - 1) {
            return null;
        }

        return direntList.get(index);
    }

    private void deleteFile() {

        int position = binding.pager.getCurrentItem();
        DirentModel direntModel = getSelectedDirent();

        DeleteFileDialogFragment dialogFragment = DeleteFileDialogFragment.newInstance(CollectionUtils.newArrayList(direntModel.uid));
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    isDataOperated = true;

                    direntList.remove(position);
                    adapter.removeFragment(position);
                    adapter.notifyItemRemoved(position);

                    int carouselIndex = position + 1;
                    carouselDirentList.remove(carouselIndex);
                    carouselAdapter.notifyItemRemoved(carouselIndex);

                    ToastUtils.showLong(R.string.delete_successful);

                    if (adapter.getItemCount() == 0) {
                        setResult(RESULT_OK);
                        finish();
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


    private HashMap<String, FileProfileConfigModel> fileDetailHashMap = new HashMap<>();

    private void preShowProfileDialog() {
        DirentModel direntModel = getSelectedDirent();
        if (direntModel == null) {
            return;
        }

        String key = direntModel.full_path;
        if (fileDetailHashMap.containsKey(key)) {
            showProfileDialog(fileDetailHashMap.get(key));
        } else {
            getViewModel().getFileDetail(repoId, key);
        }
    }

    private void showProfileDialog(FileProfileConfigModel model) {
        FileProfileDialog detailDialog = FileProfileDialog.newInstance(model.detail, model.users.user_list, true);
        detailDialog.show(getSupportFragmentManager(), FileProfileDialog.class.getSimpleName());
    }

    private CopyMoveContext copyMoveContext = null;

    private void copy() {
        DirentModel direntModel = getSelectedDirent();
        if (direntModel == null) {
            return;
        }

        chooseCopyMoveDest(direntModel, OpType.COPY);
    }

    /**
     * Choose copy/move destination for multiple files
     */
    private void chooseCopyMoveDest(DirentModel direntModel, OpType op) {

        copyMoveContext = new CopyMoveContext(repoId, repoName, parentDir, CollectionUtils.newArrayList(direntModel), op);
        copyMoveLauncher.launch(ObjSelectorActivity.getStartIntent(this));
    }

    private final ActivityResultLauncher<Intent> copyMoveLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK || o.getData() == null) {
                return;
            }

            String dstRepoId = o.getData().getStringExtra(ObjSelectorActivity.DATA_REPO_ID);
            String dstDir = o.getData().getStringExtra(ObjSelectorActivity.DATA_DIR);
            String disRepoName = o.getData().getStringExtra(ObjSelectorActivity.DATA_REPO_NAME);

            copyMoveContext.setDest(dstRepoId, dstDir, disRepoName);

            doCopyMove();
        }
    });

    private void doCopyMove() {
        if (copyMoveContext == null) {
            return;
        }

        if (!copyMoveContext.checkCopyMoveToSubfolder()) {
            ToastUtils.showLong(copyMoveContext.isCopy()
                    ? R.string.cannot_copy_folder_to_subfolder
                    : R.string.cannot_move_folder_to_subfolder);
            return;
        }

        CopyMoveDialogFragment dialogFragment = CopyMoveDialogFragment.newInstance();
        dialogFragment.initData(copyMoveContext);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    ToastUtils.showLong(copyMoveContext.isCopy() ? R.string.copied_successfully : R.string.moved_successfully);
                    isDataOperated = true;
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), CopyMoveDialogFragment.class.getSimpleName());
    }

}
