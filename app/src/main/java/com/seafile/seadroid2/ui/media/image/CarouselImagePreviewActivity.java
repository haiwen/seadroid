package com.seafile.seadroid2.ui.media.image;

import android.animation.ValueAnimator;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.graphics.Color;
import android.os.Bundle;
import android.os.PersistableBundle;
import android.text.TextUtils;
import android.util.Pair;
import android.util.TypedValue;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.databinding.ActivityCarouselImagePreviewBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.CopyMoveDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.selector.versatile.VersatileSelectorActivity;
import com.seafile.seadroid2.view.photoview.ScrollDirection;
import com.seafile.seadroid2.view.photoview.ScrollStatus;

import java.util.ArrayList;
import java.util.List;

public class CarouselImagePreviewActivity extends BaseActivityWithVM<ImagePreviewViewModel> {
    private ActivityCarouselImagePreviewBinding binding;
    private Toolbar toolbar;
    private ViewPager2Adapter pagerAdapter;
    private ThumbnailAdapter thumbnailAdapter;
    private final String KEY_CURRENT_PAGE = "current_page";
    public static int actionbarHeight = -1;

    /**
     * actionbar: toolBar/statusBar/navBar/bottomActionBar/thumbnailListBar
     */
    private boolean isActionBarVisible = true;
    private boolean isNightMode = false;

    private String repoId, repoName, parentDir, name;
    private boolean load_other_images_in_same_directory = false;
    private ActivityResultLauncher<Intent> copyMoveLauncher;
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

    public static Intent startThisFromObjs(Context context, DirentModel direntModel, boolean load_other_images_in_same_directory) {
        Intent intent = new Intent(context, CarouselImagePreviewActivity.class);
        intent.putExtra("repo_id", direntModel.repo_id);
        intent.putExtra("repo_name", direntModel.repo_name);
        intent.putExtra("parent_dir", direntModel.parent_dir);
        intent.putExtra("name", direntModel.name);
        intent.putExtra("load_other_images_in_same_directory", load_other_images_in_same_directory);//Load other images in the same folder
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

    public static Intent startThisFromActivities(Context context, ActivityModel model) {
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
    public void onSaveInstanceState(@NonNull Bundle outState, @NonNull PersistableBundle outPersistentState) {
        super.onSaveInstanceState(outState, outPersistentState);
        outState.putInt(KEY_CURRENT_PAGE, binding.pager.getCurrentItem());
        outState.putString("repo_id", repoId);
        outState.putString("repo_name", repoName);
        outState.putString("parent_dir", parentDir);
        outState.putString("name", name);
        outState.putBoolean("load_other_images_in_same_directory", load_other_images_in_same_directory);
    }

    public static int getActionBarSize(Context context) {
        TypedValue tv = new TypedValue();
        if (context.getTheme().resolveAttribute(androidx.appcompat.R.attr.actionBarSize, tv, true)) {
            return TypedValue.complexToDimensionPixelSize(
                    tv.data, context.getResources().getDisplayMetrics());
        }
        return 0;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityCarouselImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());
        registerCopyMoveLauncher();

        initParams(savedInstanceState);

        initStatusBar();
        initNightMode();
        initToolbar();
        initView();
        initPager();
        initThumbnailList();

        initViewModel();


        if (actionbarHeight == -1) {
            actionbarHeight = getActionBarSize(this);
        }

        getViewModel().load(repoId, repoName, parentDir, name, load_other_images_in_same_directory);
    }

    private void registerCopyMoveLauncher() {
        copyMoveLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
            @Override
            public void onActivityResult(ActivityResult o) {
                if (o.getResultCode() != Activity.RESULT_OK || o.getData() == null) {
                    return;
                }

                String dstRepoId = o.getData().getStringExtra(ObjKey.REPO_ID);
                String disRepoName = o.getData().getStringExtra(ObjKey.REPO_NAME);
                String dstDir = o.getData().getStringExtra(ObjKey.DIR);

                copyMoveContext.setDest(dstRepoId, dstDir, disRepoName);

                doCopyMove();
            }
        });
    }

    private void checkBack() {
        PhotoFragment photoFragment = getCurrentPhotoFragment();
        if (photoFragment != null && photoFragment.isBottomShowing()) {
            alphaBar(1f);
            photoFragment.toggle();
            return;
        }

        setResult(RESULT_OK);

        finish();
    }


    private void initParams(Bundle savedInstanceState) {
        if (savedInstanceState != null) {
            int currentPage = savedInstanceState.getInt(KEY_CURRENT_PAGE, 0);
            binding.pager.setCurrentItem(currentPage, false);

            repoId = savedInstanceState.getString("repo_id");
            repoName = savedInstanceState.getString("repo_name");
            parentDir = savedInstanceState.getString("parent_dir");
            name = savedInstanceState.getString("name");

            load_other_images_in_same_directory = savedInstanceState.getBoolean("load_other_images_in_same_directory", false);
        } else {
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


    }

    private void initStatusBar() {
        //init status bar
        int color = ContextCompatKt.getColorCompat(this, R.color.bar_background_color);
        BarUtils.setStatusBarColor(this, color);
        BarUtils.setStatusBarLightMode(this, !isNightMode);
        BarUtils.setNavBarColor(this, color);
        BarUtils.setNavBarLightMode(this, !isNightMode);
    }

    private void initNightMode() {
        int currentNightMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        isNightMode = currentNightMode == Configuration.UI_MODE_NIGHT_YES;
    }

    private void initToolbar() {
        toolbar = getActionBarToolbar();
        toolbar.setNavigationOnClickListener(v -> {
            checkBack();
        });

        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(null);
        }
        //init toolbar margin top
        ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams) binding.toolbarActionbar.getLayoutParams();
        layoutParams.topMargin = BarUtils.getStatusBarHeight();

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                checkBack();
            }
        });
    }

    private void initView() {
        View.OnClickListener onClickListener = v -> {
            int id = v.getId();
            if (id == R.id.gallery_delete_photo) {
                deleteFile();
            } else if (id == R.id.gallery_star_photo) {
                starFile();
            } else if (id == R.id.gallery_share_photo) {
                shareFile();
            } else if (id == R.id.gallery_detail) {
                toggleChildFragmentDetailLayout();
            } else if (id == R.id.gallery_copy_photo) {
                copy();
            }
        };

        binding.galleryDeletePhoto.setOnClickListener(onClickListener);
        binding.galleryDetail.setOnClickListener(onClickListener);
        binding.galleryStarPhoto.setOnClickListener(onClickListener);
        binding.gallerySharePhoto.setOnClickListener(onClickListener);
        binding.galleryCopyPhoto.setOnClickListener(onClickListener);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoadingDialog(aBoolean);
            }
        });

        getViewModel().getRepoAndListLiveData().observe(this, new Observer<Pair<RepoModel, List<DirentModel>>>() {
            @Override
            public void onChanged(Pair<RepoModel, List<DirentModel>> pair) {
                submitData(pair);
            }
        });

        getViewModel().getStarredLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
//                Toasts.show(aBoolean ? R.string.star_file_succeed : R.string.star_file_failed);

                int index = binding.pager.getCurrentItem();
                thumbnailAdapter.getItems().get(index + 1).starred = aBoolean;

                notifyCurrentStarredStatus();
            }
        });

        getViewModel().getScrolling().observe(this, new Observer<DetailLayoutShowModel>() {
            @Override
            public void onChanged(DetailLayoutShowModel model) {
                gradientLayout(model);
            }
        });

        getViewModel().getTapLiveData().observe(this, new Observer<Integer>() {
            @Override
            public void onChanged(Integer integer) {
                hideOrShowToolbar();
            }
        });
    }

    private void initPager() {
        pagerAdapter = new ViewPager2Adapter(this);
        binding.pager.setOffscreenPageLimit(7);
        binding.pager.setAdapter(pagerAdapter);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                if (whoControlSwipe == 1) {
                    whoControlSwipe = -1;
                    return;
                }

                if (toolbar != null) {
                    toolbar.setTitle(thumbnailAdapter.getItems().get(position + 1).name);
                }

                whoControlSwipe = 0;
                animateToolbar();
                notifyCurrentStarredStatus();
                getCenterScaleLayoutManager().scrollToPositionWithCenter(position + 1);
            }
        });
        setPagerColor(!isNightMode);
    }

    /**
     * 0: pager
     * 1: thumbnailRecyclerView
     */
    private int whoControlSwipe = -1;

    private void initThumbnailList() {
        thumbnailAdapter = new ThumbnailAdapter(this, getServerUrl());
        thumbnailAdapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<DirentModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<DirentModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                int curPosition = binding.pager.getCurrentItem();
                if (curPosition + 1 == i) {
                    return;
                }
                getCenterScaleLayoutManager().scrollToPositionWithCenter(i);
            }
        });
        binding.thumbnailRecyclerView.setAdapter(thumbnailAdapter);

        binding.thumbnailRecyclerView.setLayoutManager(getCenterScaleLayoutManager());

        CenterSnapHelper snapHelper = new CenterSnapHelper();
        snapHelper.attachToRecyclerView(binding.thumbnailRecyclerView);
    }

    private CenterScaleLayoutManager decorationManager;

    @NonNull
    private CenterScaleLayoutManager getCenterScaleLayoutManager() {
        if (decorationManager == null) {
            decorationManager = new CenterScaleLayoutManager(this);
            decorationManager.setOnCenterItemChangedListener(new CenterScaleLayoutManager.OnCenterItemChangedListener() {
                @Override
                public void onCenterItemChanged(int position) {
                    if (whoControlSwipe == 0) {
                        whoControlSwipe = -1;
                        return;
                    }

                    whoControlSwipe = 1;
                    animateToolbar();
                    notifyCurrentStarredStatus();
                    binding.pager.setCurrentItem(position - 1, false);
                }
            });
        }
        return decorationManager;
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

        List<DirentModel> direntList = pair.second;
        if (CollectionUtils.isEmpty(direntList)) {
            return;
        }

        //
        List<Fragment> fragments = new ArrayList<>();
        for (DirentModel direntModel : direntList) {
            PhotoFragment photoFragment = PhotoFragment.newInstance(getServerUrl(), direntModel);
            fragments.add(photoFragment);
        }

        pagerAdapter.addFragments(fragments);
        pagerAdapter.notifyItemRangeInserted(0, direntList.size());

        List<DirentModel> newList = new ArrayList<>();

        //notice:Placeholder item
        //There may be a more elegant way to do this, but this is the simplest
        newList.add(new DirentModel(ItemPositionEnum.START));
        newList.addAll(direntList);
        //Placeholder item
        newList.add(new DirentModel(ItemPositionEnum.END));
        thumbnailAdapter.notify(newList);

        navToSelectedPage();
    }

    private String server_url;
    private final boolean isLogin = SupportAccountManager.getInstance().isLogin();

    private String getServerUrl() {
        if (!TextUtils.isEmpty(server_url)) {
            return server_url;
        }

        if (!isLogin) {
            return null;
        }

        server_url = HttpIO.getCurrentInstance().getServerUrl();
        return server_url;
    }

    private final float fraction = 0.02f;

    private PhotoFragment getCurrentPhotoFragment() {
        if (pagerAdapter.getFragments().isEmpty()) {
            return null;
        }

        int i = binding.pager.getCurrentItem();
        if (i >= pagerAdapter.getFragments().size()) {
            return null;
        }

        return (PhotoFragment) pagerAdapter.getFragments().get(i);
    }

    private PhotoFragment getSpecialPhotoFragment(int position) {
        int i = binding.pager.getCurrentItem();
        return (PhotoFragment) pagerAdapter.getFragments().get(position);
    }

    private void toggleChildFragmentDetailLayout() {
        PhotoFragment photoFragment = getCurrentPhotoFragment();
        if (photoFragment == null) {
            return;
        }

        if (photoFragment.isBottomShowing()) {
//            binding.galleryDetail.setImageResource(R.drawable.icon_info);
            alphaBar(1f);
        } else {
//            binding.galleryDetail.setImageResource(R.drawable.icon_info);
            alphaBar(0f);
        }

        photoFragment.toggle();
    }

    private void gradientLayout(DetailLayoutShowModel showModel) {
        ScrollDirection direction = showModel.direction;
        int totalDistance = showModel.distance;
        ScrollStatus scrollStatus = showModel.status;
        boolean isDetailShowing = showModel.isShowing;

        if (scrollStatus == ScrollStatus.CANCELLED) {
            animateToolbar(!isDetailShowing);
            return;
        }

        if (scrollStatus == ScrollStatus.FINISHED) {
            alphaBar(isDetailShowing ? 1f : 0f);
            return;
        }

        if (!isActionBarVisible) {
            return;
        }

        float a = getToolbarAlpha();
        if (direction == ScrollDirection.DOWN) {
            //0 - 1
            a += fraction;
        } else if (direction == ScrollDirection.UP) {
            //1 - 0
            a -= fraction;
        }

        if (a > 1) {
            a = 1;
        } else if (a < 0) {
            a = 0;
        }

        alphaBar(a);
    }

    private float getToolbarAlpha() {
        return binding.toolbarActionbar.getAlpha();
    }

    private void alphaBar(float a) {
        if (!isActionBarVisible) {
            return;
        }

        if (a == 0f) {
            int v = binding.toolbarActionbar.getVisibility();
            if (v != View.GONE) {
                binding.toolbarActionbar.setVisibility(View.GONE);
                binding.toolbarActionbar.setAlpha(0f);
                binding.thumbnailRecyclerView.setVisibility(View.GONE);
                binding.thumbnailRecyclerView.setAlpha(0f);

//                binding.galleryDetail.setImageResource(R.drawable.baseline_info_grey_24);

                setStatusBarAlpha(0);
            }

        } else if (a == 1f) {
            float ca = binding.toolbarActionbar.getAlpha();
            if (ca != 1f) {
                binding.toolbarActionbar.setVisibility(View.VISIBLE);
                binding.toolbarActionbar.setAlpha(1f);
                binding.thumbnailRecyclerView.setVisibility(View.VISIBLE);
                binding.thumbnailRecyclerView.setAlpha(1f);

//                binding.galleryDetail.setImageResource(R.drawable.baseline_info_24);
                setStatusBarAlpha(255);
            }
        } else {
            binding.toolbarActionbar.setAlpha(a);
            binding.thumbnailRecyclerView.setAlpha(a);

            setStatusBarAlpha(a);

            int v = binding.toolbarActionbar.getVisibility();
            if (v != View.VISIBLE) {

                binding.thumbnailRecyclerView.setVisibility(View.VISIBLE);
                binding.toolbarActionbar.setVisibility(View.VISIBLE);
            }
        }

    }

    private void setStatusBarAlpha(float a) {
        int alpha = (int) (a * 255);
        setStatusBarAlpha(alpha);
    }

    private void setStatusBarAlpha(int alpha) {
        BarUtils.setNavBarColor(this, isNightMode
                ? ImagePreviewUtils.getGreyAlpha911(this, alpha)
                : ImagePreviewUtils.getGreyAlpha000(this, alpha));

        BarUtils.setStatusBarColor(this, isNightMode
                ? ImagePreviewUtils.getGreyAlpha911(this, alpha)
                : ImagePreviewUtils.getGreyAlpha000(this, alpha));
    }

    /**
     * when view pager changed, animate toolbar
     *
     */
    private void animateToolbar() {
        if (!isActionBarVisible) {
            return;
        }

        int position = binding.pager.getCurrentItem();
        PhotoFragment photoFragment = getSpecialPhotoFragment(position);
        if (photoFragment == null) {
            return;
        }
        boolean isDetailShowing = photoFragment.isBottomShowing();
        float currentToolbarAlpha = getToolbarAlpha();
        if (currentToolbarAlpha == 0f) {
            if (isDetailShowing) {
                //do nothing
            } else {
                animateToolbar(true);
            }
        } else if (currentToolbarAlpha == 1f) {
            if (isDetailShowing) {
                animateToolbar(false);
            } else {
                //do nothing
            }
        }
    }

    private void animateToolbar(boolean showToolbar) {
        float tlba = getToolbarAlpha();
        ValueAnimator animator = ValueAnimator.ofFloat(tlba, showToolbar ? 1f : 0f);
        animator.setDuration(200);
        animator.setInterpolator(new LinearInterpolator());
        animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
            @Override
            public void onAnimationUpdate(@NonNull ValueAnimator animation) {
                float a = (float) animation.getAnimatedValue();
                alphaBar(a);
            }
        });
        animator.start();
    }


    // on tap
    private void hideOrShowToolbar() {
        isActionBarVisible = !isActionBarVisible;
        hideOrShowToolbar(isActionBarVisible);
    }

    private void hideOrShowToolbar(boolean visible) {
        binding.galleryToolBar.setVisibility(visible ? View.VISIBLE : View.GONE);
        binding.thumbnailRecyclerView.setVisibility(visible ? View.VISIBLE : View.GONE);
        binding.toolbarActionbar.setVisibility(visible ? View.VISIBLE : View.INVISIBLE);

        if (isNightMode) {
            //The background color has been set in the #initPager(), and no longer updated in night mode
        } else {
            setBarLightMode(visible);
            setPagerColor(visible);
        }
    }

    private void setPagerColor(boolean isGrey) {
        int pagerColor = isGrey ? ImagePreviewUtils.getGrey100(this) : ImagePreviewUtils.getGrey911(this);
        binding.pager.setBackgroundColor(pagerColor);
    }

    private void setBarLightMode(boolean visible) {
        int color = visible ? ImagePreviewUtils.getGrey000(this) : ImagePreviewUtils.getGrey911(this);
        BarUtils.setNavBarColor(this, visible ? color : Color.TRANSPARENT);
        BarUtils.setStatusBarColor(this, visible ? color : Color.TRANSPARENT);

        BarUtils.setStatusBarLightMode(this, visible);
        BarUtils.setNavBarLightMode(this, visible);
    }


    /**
     * Dynamically navigate to the starting page index selected by user
     * by default the starting page index is 0
     */
    private void navToSelectedPage() {
        int size = thumbnailAdapter.getItems().size();
        int index = -1;

        for (int i = 0; i < size; i++) {
            if (TextUtils.equals(thumbnailAdapter.getItems().get(i).name, name)) {
                index = i;
                break;
            }
        }

        if (index != -1) {
            binding.pager.setCurrentItem(index - 1, false);
            getCenterScaleLayoutManager().scrollToPositionWithCenter(index);
        }
    }

    private void notifyCurrentStarredStatus() {
        DirentModel direntModel = getSelectedDirent();
        if (direntModel == null) {
            return;
        }

        if (direntModel.starred) {
            binding.galleryStarPhoto.setImageResource(R.drawable.icon_starred);
        } else {
            binding.galleryStarPhoto.setImageResource(R.drawable.icon_unstarred);
        }
    }


    private DirentModel getSelectedDirent() {
        List<DirentModel> direntList = thumbnailAdapter.getItems();
        if (CollectionUtils.isEmpty(direntList)) {
            return null;
        }

        int index = binding.pager.getCurrentItem();
        if (index > direntList.size() - 2) {
            return null;
        }

        return direntList.get(index + 1);
    }

    private void deleteFile() {
        DirentModel direntModel = getSelectedDirent();

        DeleteFileDialogFragment dialogFragment = DeleteFileDialogFragment.newInstance(CollectionUtils.newArrayList(direntModel.uid));
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (!isDone) {
                    return;
                }

                setResult(RESULT_OK);

                if (thumbnailAdapter.getItems().size() == 3) {
                    finish();
                    return;
                }

                int currentPosition = binding.pager.getCurrentItem();
                thumbnailAdapter.removeAt(currentPosition + 1);

                pagerAdapter.removeFragment(currentPosition);
                pagerAdapter.notifyItemRemoved(currentPosition);
            }
        });
        dialogFragment.show(getSupportFragmentManager(), DeleteFileDialogFragment.class.getSimpleName());
    }

    private void starFile() {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_down);
            return;
        }

        DirentModel direntModel = getSelectedDirent();
        if (direntModel == null) {
            return;
        }

        if (direntModel.starred) {
            getViewModel().unStar(direntModel.repo_id, direntModel.full_path);
        } else {
            getViewModel().star(direntModel.repo_id, direntModel.full_path);
        }
    }

    private void shareFile() {
        DirentModel direntModel = getSelectedDirent();
        WidgetUtils.showCreateShareLinkDialog(this, getSupportFragmentManager(), direntModel, false);
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

//        Intent intent = ObjSelectorActivity.getCurrentAccountIntent(this, ObjSelectType.REPO, ObjSelectType.DIR);
//        copyMoveLauncher.launch(intent);


        String fileName = direntModel.name;

        Intent intent = VersatileSelectorActivity.getCurrentAccountIntent(this, direntModel.repo_id, direntModel.parent_dir, fileName, op == OpType.COPY);
        copyMoveLauncher.launch(intent);
    }

    private void doCopyMove() {
        if (copyMoveContext == null) {
            return;
        }

        if (!copyMoveContext.checkCopyMoveToSubfolder()) {
            Toasts.show(copyMoveContext.isCopy()
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
                    setResult(RESULT_OK);
                    Toasts.show(copyMoveContext.isCopy() ? R.string.copied_successfully : R.string.moved_successfully);
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), CopyMoveDialogFragment.class.getSimpleName());
    }

}
