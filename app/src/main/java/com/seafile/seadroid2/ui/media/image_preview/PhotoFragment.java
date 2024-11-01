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
import androidx.webkit.CookieManagerCompat;
import androidx.webkit.UserAgentMetadata;

import com.blankj.utilcode.util.EncodeUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.load.DataSource;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.load.engine.GlideException;
import com.bumptech.glide.load.resource.gif.GifDrawable;
import com.bumptech.glide.request.RequestListener;
import com.bumptech.glide.request.RequestOptions;
import com.bumptech.glide.request.target.Target;
import com.github.chrisbanes.photoview.OnPhotoTapListener;
import com.github.chrisbanes.photoview.PhotoView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.FragmentPhotoViewBinding;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;

import java.io.File;
import java.util.Locale;

public class PhotoFragment extends BaseFragment {

    private Account account;
    private DirentModel direntModel;

    private OnPhotoTapListener onPhotoTapListener;
    private FragmentPhotoViewBinding binding;

    public void setOnPhotoTapListener(OnPhotoTapListener onPhotoTapListener) {
        this.onPhotoTapListener = onPhotoTapListener;
    }

    public static PhotoFragment newInstance(DirentModel direntModel) {
        Bundle args = new Bundle();
        args.putParcelable("dirent", direntModel);
        PhotoFragment fragment = new PhotoFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        account = SupportAccountManager.getInstance().getCurrentAccount();

        Bundle args = getArguments();
        if (args == null) {
            return;
        }

        direntModel = args.getParcelable("dirent");
        if (null == direntModel) {
            throw new IllegalArgumentException("DirentModel is null");
        }

    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentPhotoViewBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);


        binding.photoView.setZoomable(true);
        binding.photoView.setZoomTransitionDuration(300);
        binding.photoView.setMaximumScale(5f);
        binding.photoView.setMinimumScale(0.8f);
        binding.photoView.setOnPhotoTapListener(new OnPhotoTapListener() {
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
                    .into(binding.photoView);
            return;
        }

        String url = getUrl();
        if (url == null) {
            binding.photoView.setImageResource(R.drawable.icon_image_error_filled);
            return;
        }

        RequestOptions opt = new RequestOptions()
                .skipMemoryCache(true)
                .error(R.drawable.icon_image_error_filled)
                .diskCacheStrategy(DiskCacheStrategy.NONE);
        GlideApp.with(requireContext())
                .load(url)
                .apply(opt)
                .fitCenter()
                .listener(new RequestListener<Drawable>() {
                    @Override
                    public boolean onLoadFailed(@Nullable GlideException e, Object model, Target<Drawable> target, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(Drawable resource, Object model, Target<Drawable> target, DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        return false;
                    }
                })
                .into(binding.photoView);
    }


    private String getUrl() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return null;
        }

//        //https://dev.seafile.com/seafhttp/repos/4809a6f3-250c-4435-bdd8-b68f34c128d1/files//6f64603fd19f9ec45d05ec379e69e22.gif/?op=download
//        //https://dev.seafile.com/seahub/repo/4809a6f3-250c-4435-bdd8-b68f34c128d1/raw/6f64603fd19f9ec45d05ec379e69e22.gif
//        if (direntModel.name.toLowerCase().endsWith(".gif")) {
//            return String.format(Locale.ROOT, "%srepo/%s/raw/%s", account.getServer(), direntModel.repo_id, direntModel.name);
//        }

//        return String.format("%srepo/%s/raw%s", account.getServer(), repoId, fileFullPath);
        int size = SizeUtils.dp2px(300);
        return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%s", account.getServer(), direntModel.repo_id, EncodeUtils.urlEncode(direntModel.full_path), size);
    }
}
