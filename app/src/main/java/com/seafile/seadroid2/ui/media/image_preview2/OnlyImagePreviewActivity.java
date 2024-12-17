package com.seafile.seadroid2.ui.media.image_preview2;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.ActivityImagePreviewBinding;
import com.seafile.seadroid2.databinding.ActivityOnlyImagePreviewBinding;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewViewModel;
import com.seafile.seadroid2.ui.media.image_preview.PhotoFragment;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import io.reactivex.functions.Consumer;

public class OnlyImagePreviewActivity extends BaseActivityWithVM<ImagePreviewViewModel> {
    private ActivityOnlyImagePreviewBinding binding;
    private ViewPager2Adapter adapter;
    private List<String> imageUrls;
    private int position;

    public static void startThis(Context context, String url) {
        Intent intent = new Intent(context, OnlyImagePreviewActivity.class);
        intent.putStringArrayListExtra("image_urls", CollectionUtils.newArrayList(url));
        intent.putExtra("position", 0);
        context.startActivity(intent);
    }

    public static void startThis(Context context, ArrayList<String> urls, int position) {
        Intent intent = new Intent(context, OnlyImagePreviewActivity.class);
        intent.putStringArrayListExtra("image_urls", urls);
        intent.putExtra("position", position);
        context.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityOnlyImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        BarUtils.setNavBarVisibility(this, false);
        BarUtils.setStatusBarVisibility(this, false);

        initData();

        initView();
        initViewModel();

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                finish();
            }
        });
    }

    @Override
    protected void onPostCreate(@Nullable Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);

        notifyFragmentList();
    }

    private void initData() {
        if (getIntent() == null) {
            throw new IllegalArgumentException("Intent is null");
        }

        imageUrls = getIntent().getStringArrayListExtra("image_urls");
        position = getIntent().getIntExtra("position", 0);
    }

    private void initView() {

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
    }

    private void notifyFragmentList() {
        if (CollectionUtils.isEmpty(imageUrls)) {
            return;
        }

        adapter = new ViewPager2Adapter(this);
        List<Fragment> fragments = new ArrayList<>();
        for (String url : imageUrls) {
            PhotoFragment photoFragment = PhotoFragment.newInstance(url);
            photoFragment.setOnPhotoTapListener((view, x, y) -> hideOrShowToolBar());
            fragments.add(photoFragment);
        }

        adapter.addFragments(fragments);

        binding.pager.setAdapter(adapter);
//        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
//            @Override
//            public void onPageSelected(int position) {
//                super.onPageSelected(position);
//
//                String fs = String.format(Locale.ROOT, "%d/%d", (position + 1), direntList.size());
//                binding.galleryPageIndex.setText(fs);
//
//                DirentModel model = direntList.get(position);
//                binding.galleryPageName.setText(model.name);
//            }
//        });

        binding.pager.setCurrentItem(position);

    }

    private boolean showToolBar = false;

    private void hideOrShowToolBar() {
//        binding.galleryToolBar.setVisibility(!showToolBar ? View.VISIBLE : View.GONE);
//        binding.pageIndexContainer.setVisibility(!showToolBar ? View.VISIBLE : View.GONE);
        showToolBar = !showToolBar;
    }
}
