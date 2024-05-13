package com.seafile.seadroid2.ui.media.image_preview;

import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.SPStaticUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.load.DataSource;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.load.engine.GlideException;
import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.request.RequestListener;
import com.bumptech.glide.request.RequestOptions;
import com.bumptech.glide.request.target.Target;
import com.github.chrisbanes.photoview.OnPhotoTapListener;
import com.github.chrisbanes.photoview.PhotoView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;

import java.io.File;

public class PhotoFragment extends BaseFragment {

    private Account account;
    private DirentModel direntModel;

    private OnPhotoTapListener onPhotoTapListener;

    public void setOnPhotoTapListener(OnPhotoTapListener onPhotoTapListener) {
        this.onPhotoTapListener = onPhotoTapListener;
    }

    public void setDirentModel(DirentModel direntModel) {
        this.direntModel = direntModel;
    }

    public DirentModel getDirentModel() {
        return direntModel;
    }

    public static PhotoFragment newInstance() {
        PhotoFragment fragment = new PhotoFragment();
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        account = SupportAccountManager.getInstance().getCurrentAccount();

        if (null == direntModel) {
            throw new IllegalArgumentException("DirentModel is null");
        }
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        return LayoutInflater.from(getContext()).inflate(R.layout.fragment_photo_view, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        PhotoView photoView = view.findViewById(R.id.photo_view);
        photoView.setZoomable(true);
        photoView.setZoomTransitionDuration(300);
        photoView.setMaximumScale(5f);
        photoView.setMinimumScale(0.8f);
        photoView.setOnPhotoTapListener(new OnPhotoTapListener() {
            @Override
            public void onPhotoTap(ImageView view, float x, float y) {
                if (onPhotoTapListener != null) {
                    onPhotoTapListener.onPhotoTap(view, x, y);
                }
            }
        });

        ProgressBar progressBar = view.findViewById(R.id.progress_bar);

        File file = DataManager.getLocalRepoFile(account, direntModel.repo_id, direntModel.repo_name, direntModel.full_path);
        if (file.exists()) {
            progressBar.setVisibility(View.GONE);

            GlideApp.with(requireContext())
                    .load(file)
                    .into(photoView);
            return;
        }

        String url = getUrl();
        SLogs.d(url);
        if (url == null) {
            photoView.setImageResource(R.drawable.icon_image_error_filled);
        } else {
            RequestOptions opt = new RequestOptions()
                    .skipMemoryCache(true)
                    .error(R.drawable.icon_image_error_filled)
                    .diskCacheStrategy(DiskCacheStrategy.NONE);

            GlideUrl glideUrl1 = GlideLoadConfig.getGlideUrl(url);

            GlideApp.with(requireContext())
                    .load(glideUrl1)
                    .apply(opt)
                    .fitCenter()
                    .listener(new RequestListener<Drawable>() {
                        @Override
                        public boolean onLoadFailed(@Nullable GlideException e, Object model, Target<Drawable> target, boolean isFirstResource) {
                            progressBar.setVisibility(View.GONE);
                            return false;
                        }

                        @Override
                        public boolean onResourceReady(Drawable resource, Object model, Target<Drawable> target, DataSource dataSource, boolean isFirstResource) {
                            progressBar.setVisibility(View.GONE);
                            return false;
                        }
                    })
                    .into(photoView);
        }
    }

    private String getUrl() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return null;
        }

//        return String.format("%srepo/%s/raw%s", account.getServer(), repoId, fileFullPath);
        int size = SizeUtils.dp2px(300);
        return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%s", account.getServer(), direntModel.repo_id, direntModel.full_path, size);
    }
}
