package com.seafile.seadroid2.ui.media.image_preview2;

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
import androidx.appcompat.widget.Toolbar;
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
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.databinding.ActivityCarouselImagePreviewBinding;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewViewModel;
import com.seafile.seadroid2.ui.media.image_preview.PhotoFragment;

import java.util.ArrayList;
import java.util.List;

public class CarouselImagePreviewActivity extends BaseActivityWithVM<ImagePreviewViewModel> implements Toolbar.OnMenuItemClickListener {
    private ActivityCarouselImagePreviewBinding binding;

    private ViewPager2Adapter adapter;
    private CarouselAdapter carouselAdapter;

    private List<DirentModel> direntList;

    private boolean isLightMode = true;
    private boolean isDataOperated = false;

    private String repoId, repoName, parentDir, name;
    private boolean load_other_images_in_same_directory = false;
    private int carouselItemWidth, carouselItemMargin;

    public static Intent startThisFromRepo(Context context, DirentModel direntModel) {
        Intent intent = new Intent(context, CarouselImagePreviewActivity.class);
        intent.putExtra("repo_id", direntModel.repo_id);
        intent.putExtra("repo_name", direntModel.repo_name);
        intent.putExtra("parent_dir", direntModel.parent_dir);
        intent.putExtra("name", direntModel.name);
        intent.putExtra("load_other_images_in_same_directory", true);//Load other images in the same folder
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

        carouselItemWidth = getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        carouselItemMargin = getResources().getDimensionPixelSize(R.dimen.carousel_item_margin);


        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(null);
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
    }

    private void initAdapter() {
        adapter = new ViewPager2Adapter(this);

        binding.pager.setAdapter(adapter);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);

                gravitySnapHelper.scrollToPosition(position);

                notifyCurrentStarredStatus();
            }
        });
    }

    private final LinearLayoutManager layoutManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
    private final GravitySnapHelper gravitySnapHelper = new GravitySnapHelper(Gravity.CENTER);

    private void initCarouselAdapter() {

        carouselAdapter = new CarouselAdapter(this, new CarouselAdapter.CarouselItemListener() {
            @Override
            public void onItemClicked(DirentModel item, int snapPosition) {
                ToastUtils.showShort("onItemClicked: " + snapPosition);
                int pageIndex = binding.pager.getCurrentItem();
                if (pageIndex == snapPosition) {
                    return;
                }
                binding.pager.setCurrentItem(snapPosition, true);
            }
        });

        binding.recyclerView.setAdapter(carouselAdapter);
        binding.recyclerView.setLayoutManager(layoutManager);

        int itemWidth = getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        int screenWidth = ScreenUtils.getAppScreenWidth();
        int itemMargin = getResources().getDimensionPixelSize(R.dimen.carousel_item_margin);
        int slidePadding = (screenWidth - itemWidth) / 2 - itemMargin;

        LinearEdgeDecoration decoration = new LinearEdgeDecoration(slidePadding, slidePadding, RecyclerView.HORIZONTAL, false);
        binding.recyclerView.addItemDecoration(decoration);

        binding.recyclerView.addOnScrollListener(new CenterScaleXYRecyclerViewScrollListener(this));

        gravitySnapHelper.setSnapListener(new GravitySnapHelper.SnapListener() {
            @Override
            public void onSnap(int snapPosition) {
                int pageIndex = binding.pager.getCurrentItem();
                if (pageIndex == snapPosition) {
                    return;
                }

                binding.pager.setCurrentItem(snapPosition, true);
            }
        });
//        gravitySnapHelper.setSnapLastItem(true);
        gravitySnapHelper.attachToRecyclerView(binding.recyclerView);
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

        carouselAdapter.submitList(direntList);

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
            binding.pager.setCurrentItem(index, true);
            gravitySnapHelper.scrollToPosition(index);
        }
    }

    private void notifyCurrentStarredStatus() {
        DirentModel direntModel = getSelectedDirent();
        if (direntModel == null) {
            return;
        }
        if (direntModel.starred) {
            binding.galleryStarPhoto.setImageResource(R.drawable.baseline_starred_32);
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
            ToastUtils.showLong(R.string.file_action_copy);
        } else if (item.getItemId() == R.id.info) {
            ToastUtils.showLong(R.string.file_action_copy);
        }

        return super.onOptionsItemSelected(item);
    }

    private DirentModel getSelectedDirent() {
        int index = binding.pager.getCurrentItem();
        return carouselAdapter.getItem(index);
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
                    carouselAdapter.notifyItemRemoved(position);

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

}
