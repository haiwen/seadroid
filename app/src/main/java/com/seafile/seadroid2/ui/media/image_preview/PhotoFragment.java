package com.seafile.seadroid2.ui.media.image_preview;

import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.text.style.ClickableSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.MimeTypeMap;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.SpanUtils;
import com.bumptech.glide.load.DataSource;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.load.engine.GlideException;
import com.bumptech.glide.load.resource.drawable.DrawableTransitionOptions;
import com.bumptech.glide.load.resource.gif.GifDrawable;
import com.bumptech.glide.request.RequestListener;
import com.bumptech.glide.request.target.Target;
import com.bumptech.glide.signature.ObjectKey;
import com.github.chrisbanes.photoview.OnPhotoTapListener;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.config.OriGlideUrl;
import com.seafile.seadroid2.databinding.FragmentPhotoViewBinding;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.framework.util.GlideRequest;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.ThumbnailUtils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;

import kotlin.Pair;

public class PhotoFragment extends BaseFragmentWithVM<PhotoViewModel> {

    private FragmentPhotoViewBinding binding;

    private String repoId, repoName, fullPath;
    private String imageUrl;

    private OnPhotoTapListener onPhotoTapListener;
    private String serverUrl;

    public void setOnPhotoTapListener(OnPhotoTapListener onPhotoTapListener) {
        this.onPhotoTapListener = onPhotoTapListener;
    }

    public static PhotoFragment newInstance(String url) {
        Bundle args = new Bundle();
        args.putString("image_url", url);
        PhotoFragment fragment = new PhotoFragment();
        fragment.setArguments(args);
        return fragment;
    }

    public static PhotoFragment newInstance(String serverUrl, String repoId, String repoName, String fullPath) {
        Bundle args = new Bundle();
        args.putString("repoId", repoId);
        args.putString("repoName", repoName);
        args.putString("fullPath", fullPath);
        args.putString("serverUrl", serverUrl);
        PhotoFragment fragment = new PhotoFragment();
        fragment.setArguments(args);
        return fragment;
    }

    public static PhotoFragment newInstance(String serverUrl, DirentModel direntModel) {
        return newInstance(serverUrl, direntModel.repo_id, direntModel.repo_name, direntModel.full_path);
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle args = getArguments();
        if (args == null) {
            return;
        }

        repoId = args.getString("repoId");
        repoName = args.getString("repoName");
        fullPath = args.getString("fullPath");
        imageUrl = args.getString("image_url");
        serverUrl = args.getString("serverUrl");

        if (TextUtils.isEmpty(repoId) && TextUtils.isEmpty(imageUrl)) {
            throw new IllegalStateException("the args is invalid");
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

        TextView descTextView = binding.errorView.findViewById(R.id.desc);
        SpanUtils.with(descTextView)
                .append(getString(R.string.error_image_load))
                .setForegroundColor(ContextCompatKt.getColorCompat(requireContext(), R.color.black))
                .append(",")
                .append("  ")
                .append(getString(R.string.retry_with_click))
                .setClickSpan(ContextCompatKt.getColorCompat(requireContext(), R.color.fancy_orange), true, new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        load();
                    }
                })
                .create();

        binding.photoView.setZoomable(true);
        binding.photoView.setZoomTransitionDuration(300);
        binding.photoView.setMaximumScale(3f);
        binding.photoView.setMinimumScale(1f);
        binding.photoView.setOnPhotoTapListener(new OnPhotoTapListener() {
            @Override
            public void onPhotoTap(ImageView view, float x, float y) {
                if (onPhotoTapListener != null) {
                    onPhotoTapListener.onPhotoTap(view, x, y);
                }
            }
        });

        intViewModel();

        load();
    }


    private void intViewModel() {
        getViewModel().getSeafExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                binding.progressBar.setVisibility(View.GONE);
                binding.errorView.setVisibility(View.VISIBLE);
            }
        });

        getViewModel().getCheckLocalLiveData().observe(getViewLifecycleOwner(), new Observer<Pair<DirentModel, FileTransferEntity>>() {
            @Override
            public void onChanged(Pair<DirentModel, FileTransferEntity> pair) {
                DirentModel direntModel = pair.getFirst();

                if (direntModel == null) {
                    binding.photoView.setImageResource(R.drawable.icon_image_error_filled);
                    binding.progressBar.setVisibility(View.GONE);
                    return;
                }

                FileTransferEntity transferEntity = pair.getSecond();
                if (transferEntity != null && FileUtils.isFileExists(transferEntity.target_path)) {
                    //no exists local file
                    if (isGif(fullPath)) {
                        loadOriGifUrl(transferEntity.target_path);
                    } else {
                        loadOriUrl(transferEntity.target_path);
                    }
                } else {
//                    if (isGif(fullPath)) {
//                        getViewModel().download(direntModel);
//                    } else {
//                        getViewModel().requestOriginalUrl(direntModel);
//                    }
                    getViewModel().download(direntModel);
                }
            }
        });

        getViewModel().getOriginalUrlLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String oriUrl) {
                loadOriUrl(oriUrl);
            }
        });

        getViewModel().getDownloadedPathLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String rawPath) {
                if (isGif(fullPath)) {
                    loadOriGifUrl(rawPath);
                } else {
                    loadOriUrl(rawPath);
                }
            }
        });
    }

    private void load() {
        if (binding.progressBar.getVisibility() == View.GONE) {
            binding.progressBar.setVisibility(View.VISIBLE);
        }

        if (binding.errorView.getVisibility() == View.VISIBLE) {
            binding.errorView.setVisibility(View.GONE);
        }

        if (!TextUtils.isEmpty(imageUrl)) {
            loadUrl(imageUrl);
        } else {
            loadThumbnailAndRequestRawUrl();
        }
    }

    private void loadUrl(String url) {
        GlideApp.with(this)
                .load(url)
                .diskCacheStrategy(DiskCacheStrategy.AUTOMATIC)
                .transition(DrawableTransitionOptions.withCrossFade())
                .listener(new RequestListener<Drawable>() {
                    @Override
                    public boolean onLoadFailed(@Nullable GlideException e, @Nullable Object model, @NonNull Target<Drawable> target, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        binding.errorView.setVisibility(View.VISIBLE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(@NonNull Drawable resource, @NonNull Object model, Target<Drawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        return false;
                    }
                })
                .into(binding.photoView);
    }

    private void loadThumbnailAndRequestRawUrl() {
        getViewModel().checkLocal(repoId, fullPath);
    }

    private void loadOriUrl(String oriUrl) {
//        String thumbnailUrl = convertThumbnailUrl(fullPath);
//        String thumbKey = EncryptUtils.encryptMD5ToString(thumbnailUrl);
//        // load thumbnail first
//        GlideRequest<Drawable> thumbnailRequest = GlideApp.with(this)
//                .load(thumbnailUrl)
//                .diskCacheStrategy(DiskCacheStrategy.ALL)
//                .signature(new ObjectKey(thumbKey))
//                .addListener(new RequestListener<Drawable>() {
//                    @Override
//                    public boolean onLoadFailed(@Nullable GlideException e, @Nullable Object model, @NonNull Target<Drawable> target, boolean isFirstResource) {
//                        return false;
//                    }
//
//                    @Override
//                    public boolean onResourceReady(@NonNull Drawable resource, @NonNull Object model, Target<Drawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
////                        SLogs.e("缩略图：" + dataSource.name() + ": " + isFirstResource + ": " + thumbKey + ": " + thumbnailUrl);
//                        return false;
//                    }
//                });

//        String oriCacheKey = EncryptUtils.encryptMD5ToString(repoId + fullPath);
        //new OriGlideUrl()
        GlideApp.with(this)
                .load(oriUrl)
                .diskCacheStrategy(DiskCacheStrategy.NONE)
                .transition(DrawableTransitionOptions.withCrossFade())
                .listener(new RequestListener<Drawable>() {
                    @Override
                    public boolean onLoadFailed(@Nullable GlideException e, @Nullable Object model, @NonNull Target<Drawable> target, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        binding.errorView.setVisibility(View.VISIBLE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(@NonNull Drawable resource, @NonNull Object model, Target<Drawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        // 图片加载成功
//                        SLogs.e("原图：" + dataSource.name() + ": " + isFirstResource + ": " + oriCacheKey + ": " + oriUrl);
                        return false;
                    }
                })
                .into(binding.photoView);
    }


    private void loadOriGifUrl(String rawUrl) {
        GlideApp.with(this)
                .asGif()
                .load(rawUrl)
                .diskCacheStrategy(DiskCacheStrategy.NONE)//
                .placeholder(binding.photoView.getDrawable())
                .listener(new RequestListener<GifDrawable>() {
                    @Override
                    public boolean onLoadFailed(@Nullable GlideException e, @Nullable Object model, @NonNull Target<GifDrawable> target, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        binding.errorView.setVisibility(View.VISIBLE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(@NonNull GifDrawable resource, @NonNull Object model, Target<GifDrawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        // 图片加载成功
                        SLogs.e(dataSource.name() + ": " + isFirstResource + ": " + rawUrl);
                        return false;
                    }
                })
                .into(binding.photoView);
    }

    private boolean isGif(String fileName) {
        if (TextUtils.isEmpty(fileName)) {
            return false;
        }

        String f = MimeTypeMap.getFileExtensionFromUrl(fileName);
        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(f);
        return mime != null && mime.equalsIgnoreCase("image/gif");
    }

    private String convertThumbnailUrl(String fullPath) {
        if (TextUtils.isEmpty(serverUrl)) {
            return null;
        }

        return ThumbnailUtils.convertThumbnailUrl(serverUrl, repoId, fullPath);
    }

}
