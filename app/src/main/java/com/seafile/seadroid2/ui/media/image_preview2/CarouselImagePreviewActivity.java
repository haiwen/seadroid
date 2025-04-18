package com.seafile.seadroid2.ui.media.image_preview2;

import android.animation.ValueAnimator;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.graphics.Color;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
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
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.databinding.ActivityCarouselImagePreviewBinding;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.CopyMoveDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewViewModel;
import com.seafile.seadroid2.ui.media.image_preview.PhotoFragment;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.view.photoview.ScrollDirection;
import com.seafile.seadroid2.view.photoview.ScrollStatus;
import com.seafile.seadroid2.view.snap_recyclerview.GravitySnapHelper;

import java.util.ArrayList;
import java.util.List;

public class CarouselImagePreviewActivity extends BaseActivityWithVM<ImagePreviewViewModel> implements Toolbar.OnMenuItemClickListener {
    private ActivityCarouselImagePreviewBinding binding;

    private ViewPager2Adapter adapter;
    private CarouselAdapter carouselAdapter;

    private List<DirentModel> direntList;
    private List<DirentModel> carouselDirentList;

    /**
     * actionbar: toolBar/statusBar/navBar/bottomActionBar/thumbnailListBar
     */
    private boolean isActionBarVisible = true;
    private boolean isNightMode = false;

    private String repoId, repoName, parentDir, name;
    private boolean load_other_images_in_same_directory = false;
    private int carouselItemWidth = 0;
    private int carouselItemMargin = 0;

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
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityCarouselImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        // full screen
        getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN | View.SYSTEM_UI_FLAG_LAYOUT_STABLE);

        int currentNightMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        isNightMode = currentNightMode == Configuration.UI_MODE_NIGHT_YES;

        //init status bar
        int color = ContextCompatKt.getColorCompat(this, R.color.bar_background_color);
        BarUtils.setStatusBarColor(this, color);
        BarUtils.setStatusBarLightMode(this, !isNightMode);
        BarUtils.setNavBarColor(this, color);
        BarUtils.setNavBarLightMode(this, !isNightMode);


        //init toolbar margin top
        ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams) binding.toolbarActionbar.getLayoutParams();
        layoutParams.topMargin = BarUtils.getStatusBarHeight();


        carouselItemWidth = getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        carouselItemMargin = getResources().getDimensionPixelSize(R.dimen.carousel_item_margin);

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(null);

            toolbar.setNavigationOnClickListener(v -> {
                setResult(RESULT_OK);

                checkBack();
            });
        }

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                checkBack();
            }
        });

        initParams();
        initView();


        initPager();
        initCarouselRecyclerView();

//        bindPager();

        initViewModel();

        getViewModel().load(repoId, repoName, parentDir, name, load_other_images_in_same_directory);
    }

    private void checkBack() {
        PhotoFragment photoFragment = getCurrentPhotoFragment();
        if (photoFragment != null && photoFragment.isShowing()) {
            alphaBar(1f);
            photoFragment.toggle();
            return;
        }

        setResult(RESULT_OK);

        finish();
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
            if (id == R.id.gallery_delete_photo) {
                deleteFile();
            } else if (id == R.id.gallery_star_photo) {
                starFile();
            } else if (id == R.id.gallery_share_photo) {
                shareFile();
            } else if (id == R.id.gallery_detail) {
                toggleChildFragmentDetailLayout();
            }
        };

        binding.galleryDeletePhoto.setOnClickListener(onClickListener);
        binding.galleryDetail.setOnClickListener(onClickListener);
        binding.galleryStarPhoto.setOnClickListener(onClickListener);
        binding.gallerySharePhoto.setOnClickListener(onClickListener);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showLoadingDialog();
                } else {
                    dismissLoadingDialog();
                }
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
//                ToastUtils.showLong(aBoolean ? R.string.star_file_succeed : R.string.star_file_failed);

                int index = binding.pager.getCurrentItem();
                direntList.get(index).starred = aBoolean;

                notifyCurrentStarredStatus();
            }
        });

        getViewModel().getScrolling().observe(this, new Observer<DetailLayoutShowModel>() {
            @Override
            public void onChanged(DetailLayoutShowModel model) {
                gradientLayout(model);
            }
        });
    }

    private void initPager() {
        adapter = new ViewPager2Adapter(this);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                notifyCurrentStarredStatus();

            }
        });
        binding.pager.setOffscreenPageLimit(7);
        binding.pager.setAdapter(adapter);

        //
        if (isNightMode) {
            int color = ContextCompatKt.getColorCompat(this, R.color.bar_background_color);
            binding.pager.setBackgroundColor(color);
        }
    }

    private final LinearLayoutManager layoutManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
    private final GravitySnapHelper snapHelper = new GravitySnapHelper(Gravity.CENTER);

    private void initCarouselRecyclerView() {
        carouselAdapter = new CarouselAdapter(this, new CarouselAdapter.CarouselItemListener() {
            @Override
            public void onItemClicked(DirentModel item, int snapPosition) {
                if (snapHelper.getCurrentSnappedPosition() == snapPosition) {
                    ToastUtils.showLong("same");
                    return;
                }

                snapHelper.smoothScrollToPosition(snapPosition);
            }
        });

        binding.thumbnailRecyclerView.setAdapter(carouselAdapter);
        binding.thumbnailRecyclerView.setLayoutManager(layoutManager);

        binding.thumbnailRecyclerView.addOnScrollListener(new CenterScaleXYRecyclerViewScrollListener(this));

        int screenWidth = ScreenUtils.getAppScreenWidth();//1080/3=360
        int sidePadding = (screenWidth - carouselItemWidth) / 2 - carouselItemMargin * 2;//170-4=166
        binding.thumbnailRecyclerView.addItemDecoration(new LinearEdgeDecoration(sidePadding, sidePadding, RecyclerView.HORIZONTAL, false));

        snapHelper.attachToRecyclerView(binding.thumbnailRecyclerView);
    }

    private void bindPager() {
        bindPager(binding.pager, snapHelper);
    }

    private int whoScroll = -1;

    public void bindPager(ViewPager2 pager, GravitySnapHelper snapHelper) {
        pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                if (whoScroll == 1) {
                    whoScroll = -1;
                    return;
                }

                animateToolbar(position);
                whoScroll = 0;
                SLogs.e("currentPagerPosition: " + position);
                snapHelper.smoothScrollToPosition(position);
            }
        });

        snapHelper.setSnapListener(new GravitySnapHelper.SnapListener() {
            @Override
            public void onSnap(int snapPosition) {

                if (whoScroll == 0) {
                    whoScroll = -1;
                    return;
                }

                animateToolbar(snapPosition);

                whoScroll = 1;
                SLogs.e("currentSnapPosition: " + snapPosition);
                pager.setCurrentItem(snapPosition, false);
            }
        });
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
        if (CollectionUtils.isEmpty(direntList)) {
            return;
        }

        //
        List<Fragment> fragments = new ArrayList<>();
        for (DirentModel direntModel : direntList) {
            PhotoFragment photoFragment = PhotoFragment.newInstance(getServerUrl(), direntModel);
            photoFragment.setOnPhotoTapListener((view, x, y) -> hideOrShowToolbar());
            fragments.add(photoFragment);
        }

        adapter.addFragments(fragments);
//        adapter.notifyItemRangeInserted(0, direntList.size());
        adapter.notifyDataSetChanged();

        carouselDirentList = new ArrayList<>();
//        carouselDirentList.add(new DirentModel());
        carouselDirentList.addAll(direntList);
//        carouselDirentList.add(new DirentModel());

        carouselAdapter.submitList(carouselDirentList);

        binding.thumbnailRecyclerView.postDelayed(new Runnable() {
            @Override
            public void run() {
                navToSelectedPage();
            }
        }, 50);
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
        if (adapter.getFragments().isEmpty()) {
            return null;
        }

        int i = binding.pager.getCurrentItem();
        if (i >= adapter.getFragments().size()) {
            return null;
        }

        return (PhotoFragment) adapter.getFragments().get(i);
    }

    private PhotoFragment getSpecialPhotoFragment(int position) {
        int i = binding.pager.getCurrentItem();
        return (PhotoFragment) adapter.getFragments().get(position);
    }

    private void toggleChildFragmentDetailLayout() {
        PhotoFragment photoFragment = getCurrentPhotoFragment();
        if (photoFragment == null) {
            return;
        }

        if (photoFragment.isShowing()) {
            binding.galleryDetail.setImageResource(R.drawable.baseline_info_24);
            alphaBar(1f);
        } else {
            binding.galleryDetail.setImageResource(R.drawable.baseline_info_grey_24);
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
//            alphaToolbar(isDetailShowing ? 0f : 1f);
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

                binding.galleryDetail.setImageResource(R.drawable.baseline_info_grey_24);

                setStatusBarAlpha(0);
            }

        } else if (a == 1f) {
            float ca = binding.toolbarActionbar.getAlpha();
            if (ca != 1f) {
                binding.toolbarActionbar.setVisibility(View.VISIBLE);
                binding.toolbarActionbar.setAlpha(1f);
                binding.thumbnailRecyclerView.setVisibility(View.VISIBLE);
                binding.thumbnailRecyclerView.setAlpha(1f);

                binding.galleryDetail.setImageResource(R.drawable.baseline_info_24);
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
        BarUtils.setNavBarColor(this, isNightMode ? getGreyAlpha911(alpha) : getGreyAlpha100(alpha));
        BarUtils.setStatusBarColor(this, isNightMode ? getGreyAlpha911(alpha) : getGreyAlpha100(alpha));
    }

    private void animateToolbar(int position) {
        if (!isActionBarVisible) {
            return;
        }

        PhotoFragment photoFragment = getSpecialPhotoFragment(position);
        if (photoFragment == null) {
            return;
        }
        boolean isDetailShowing = photoFragment.isShowing();
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
        }
    }

    private void setBarLightMode(boolean visible) {
        int color = visible ? getGrey100() : getGrey911();
        binding.pager.setBackgroundColor(color);

        BarUtils.setNavBarColor(this, visible ? color : Color.TRANSPARENT);
        BarUtils.setStatusBarColor(this, visible ? color : Color.TRANSPARENT);

        BarUtils.setStatusBarLightMode(this, visible);
        BarUtils.setNavBarLightMode(this, visible);
    }

    private int grey100 = 0;
    private int grey911 = 0;

    public int getGrey100() {
        if (grey100 == 0) {
            grey100 = ContextCompat.getColor(this, R.color.material_grey_100);
        }
        return grey100;
    }

    public int getGreyAlpha100(int alpha) {
        int red = Color.red(getGrey100());
        int green = Color.green(getGrey100());
        int blue = Color.blue(getGrey100());
        return Color.argb(alpha, red, green, blue);
    }

    public int getGrey911() {
        if (grey911 == 0) {
            grey911 = ContextCompatKt.getColorCompat(this, R.color.material_grey_911);
        }
        return grey911;
    }

    public int getGreyAlpha911(int alpha) {
        int red = Color.red(getGrey911());
        int green = Color.green(getGrey911());
        int blue = Color.blue(getGrey911());
        return Color.argb(alpha, red, green, blue);
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
            int x = (carouselItemWidth + carouselItemMargin * 2) * index;
            binding.thumbnailRecyclerView.scrollBy(x, 0);
            binding.pager.setCurrentItem(index, false);
        }

        bindPager();
    }

    private void notifyCurrentStarredStatus() {
        DirentModel direntModel = getSelectedDirent();
        if (direntModel == null) {
            return;
        }

        if (direntModel.starred) {
            binding.galleryStarPhoto.setImageResource(R.drawable.baseline_starred_filled_24);
        } else {
            binding.galleryStarPhoto.setImageResource(R.drawable.baseline_starred_outline_24);
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
            checkBack();
        } else if (item.getItemId() == R.id.copy) {
            copy();
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
        Objs.showCreateShareLinkDialog(this, getSupportFragmentManager(), direntModel, false);
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
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), CopyMoveDialogFragment.class.getSimpleName());
    }

}
