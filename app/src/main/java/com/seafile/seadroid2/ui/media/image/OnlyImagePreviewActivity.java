package com.seafile.seadroid2.ui.media.image;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.databinding.ActivityOnlyImagePreviewBinding;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;

import java.util.ArrayList;
import java.util.List;

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
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putStringArrayList("image_urls", new ArrayList<>(imageUrls));
        outState.putInt("position", position);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityOnlyImagePreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        BarUtils.setNavBarVisibility(this, false);
        BarUtils.setStatusBarVisibility(this, false);

        if (savedInstanceState != null) {
            imageUrls = savedInstanceState.getStringArrayList("image_urls");
            position = savedInstanceState.getInt("position");
        } else {
            if (getIntent() == null) {
                throw new IllegalArgumentException("Intent is null");
            }

            imageUrls = getIntent().getStringArrayListExtra("image_urls");
            position = getIntent().getIntExtra("position", 0);
        }

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


    private void initView() {

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
