package com.seafile.seadroid2.ui.media.image;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.animation.ValueAnimator;
import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Color;
import android.graphics.RectF;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.os.Build;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.OptIn;
import androidx.core.content.ContextCompat;
import androidx.exifinterface.media.ExifInterface;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.media3.common.MediaItem;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.ui.PlayerView;

import com.adobe.internal.xmp.XMPException;
import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.blankj.utilcode.util.SpanUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.bumptech.glide.Glide;
import com.bumptech.glide.load.DataSource;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.load.engine.GlideException;
import com.bumptech.glide.load.resource.drawable.DrawableTransitionOptions;
import com.bumptech.glide.load.resource.gif.GifDrawable;
import com.bumptech.glide.request.RequestListener;
import com.bumptech.glide.request.target.CustomTarget;
import com.bumptech.glide.request.target.Target;
import com.bumptech.glide.request.transition.Transition;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.FragmentPhotoViewBinding;
import com.seafile.seadroid2.databinding.ViewImageExifContainerBinding;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.motionphoto.MotionPhotoDescriptor;
import com.seafile.seadroid2.framework.motionphoto.MotionPhotoDetector;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.ThumbnailUtils;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.jni.HeicNative;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.ui.media.data_source.MotionPhotoDataSourceFactory;
import com.seafile.seadroid2.view.DocProfileView;
import com.seafile.seadroid2.view.photoview.OnPhotoTapListener;
import com.seafile.seadroid2.view.photoview.OnViewActionEndListener;
import com.seafile.seadroid2.view.photoview.OnViewDragListener;
import com.seafile.seadroid2.view.photoview.ScrollDirection;
import com.seafile.seadroid2.view.photoview.ScrollStatus;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;

@OptIn(markerClass = UnstableApi.class)
public class PhotoFragment extends BaseFragment {
    public static final String TAG = "PhotoFragment";
    private FragmentPhotoViewBinding binding;

    private String repoId, repoName, fullPath;
    private String imageUrl;
    private boolean isNightMode = false;
    private boolean canScrollBottomLayout = true;

    private ImagePreviewHelper imagePreviewHelper;
    private OnPhotoTapListener onPhotoTapListener;
    private String serverUrl;
    private File destinationFile;

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

    private ImagePreviewViewModel parentViewModel;
    private PhotoViewModel viewModel;

    public PhotoViewModel getViewModel() {
        return viewModel;
    }

    public ImagePreviewViewModel getParentViewModel() {
        return parentViewModel;
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("repoId", repoId);
        outState.putString("repoName", repoName);
        outState.putString("fullPath", fullPath);
        outState.putString("image_url", imageUrl);
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (savedInstanceState != null) {
            repoId = savedInstanceState.getString("repoId");
            repoName = savedInstanceState.getString("repoName");
            fullPath = savedInstanceState.getString("fullPath");
            imageUrl = savedInstanceState.getString("image_url");
        } else {
            Bundle args = getArguments();
            if (args == null) {
                return;
            }

            repoId = args.getString("repoId");
            repoName = args.getString("repoName");
            fullPath = args.getString("fullPath");
            imageUrl = args.getString("image_url");
            serverUrl = args.getString("serverUrl");
        }

        int currentNightMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        isNightMode = currentNightMode == Configuration.UI_MODE_NIGHT_YES;

        if (TextUtils.isEmpty(repoId) && TextUtils.isEmpty(imageUrl)) {
            throw new IllegalStateException("the args is invalid");
        }

        if (!TextUtils.isEmpty(imageUrl)) {
            canScrollBottomLayout = false;
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

        viewModel = new ViewModelProvider(this).get(PhotoViewModel.class);
        parentViewModel = new ViewModelProvider(requireActivity()).get(ImagePreviewViewModel.class);

        intViewModel();

        initView();

        initHelper();

        load();
    }

    private void initHelper() {
        if (canScrollBottomLayout) {
            imagePreviewHelper = new ImagePreviewHelper(requireContext());
        }
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        return DataManager.getLocalFileCachePath(account, repoId, repoName, fullPathInRepo);
    }

    private void intViewModel() {
        getViewModel().getSeafExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                binding.progressBar.setVisibility(GONE);
                binding.errorView.setVisibility(VISIBLE);
            }
        });

        getViewModel().getFileDetailExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                binding.bottomProgressBar.setVisibility(GONE);
                binding.bottomErrorView.setVisibility(VISIBLE);
                binding.bottomErrorDesc.setText(e.getMessage());

            }
        });

        getViewModel().getCheckLocalLiveData().observe(getViewLifecycleOwner(), new Observer<DirentModel>() {
            @Override
            public void onChanged(DirentModel direntModel) {
                if (direntModel == null) {
                    binding.photoView.setImageResource(R.drawable.icon_image_error_filled);
                    binding.progressBar.setVisibility(GONE);
                    return;
                }

                if (TextUtils.isEmpty(direntModel.local_file_id)) {
                    getViewModel().download(direntModel);
                } else {
                    destinationFile = getLocalDestinationFile(direntModel.repo_id, direntModel.repo_name, direntModel.full_path);
                    if (FileUtils.isFileExists(destinationFile)) {
                        if (Utils.isGif(fullPath)) {
                            loadLocalGifFile(destinationFile.getAbsolutePath());
                        } else {
                            loadLocalImageFile(destinationFile.getAbsolutePath());
                        }
                    } else {
                        getViewModel().download(direntModel);
                    }
                }
            }
        });


        getViewModel().getDownloadedPathLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String rawPath) {
                destinationFile = new File(rawPath);
                if (Utils.isGif(fullPath)) {
                    loadLocalGifFile(destinationFile.getAbsolutePath());
                } else {
                    loadLocalImageFile(destinationFile.getAbsolutePath());
                }
            }
        });

        getViewModel().getFileDetailLiveData().observe(getViewLifecycleOwner(), new Observer<FileProfileConfigModel>() {
            @Override
            public void onChanged(FileProfileConfigModel configModel) {
                binding.bottomProgressBar.setVisibility(GONE);

                DocProfileView detailView = new DocProfileView(requireContext());
                detailView.parseData(configModel);

                LinearLayout.LayoutParams lp = new LinearLayout.LayoutParams(-1, -2);
                binding.bottomDetailsContainer.addView(detailView, 0, lp);
            }
        });

        getParentViewModel().getTapLiveData().observe(requireActivity(), new Observer<Integer>() {
            @Override
            public void onChanged(Integer unused) {
                if (motionPhotoType == HeicNative.MOTION_PHOTO_TYPE_NONE || motionPhotoType == -1) {
                    return;
                }
                if (imagePreviewHelper != null) {
                    imagePreviewHelper.tap();
                }
            }
        });
    }

    private void initView() {

        // live photo
        FrameLayout.LayoutParams fl = (FrameLayout.LayoutParams) binding.btnLivePhoto.getLayoutParams();
        fl.topMargin = BarUtils.getStatusBarHeight() + CarouselImagePreviewActivity.actionbarHeight + Constants.DP.DP_8;
        fl.leftMargin = Constants.DP.DP_8;
        binding.btnLivePhoto.setLayoutParams(fl);

        // desc
        TextView descTextView = binding.errorView.findViewById(R.id.desc);
        SpanUtils.with(descTextView)
                .append(getString(R.string.error_image_load))
                .setForegroundColor(ContextCompatKt.getColorCompat(requireContext(), isNightMode ? R.color.material_grey_100 : R.color.material_grey_911))
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

        initPhotoView();
        initBottomDetailLayout();
    }

    private void initPhotoView() {
        binding.photoView.setZoomable(true);
        binding.photoView.setZoomTransitionDuration(300);
        binding.photoView.setMaximumScale(3f);
        binding.photoView.setMinimumScale(1f);
        binding.photoView.setOnViewActionEndListener(new OnViewActionEndListener() {
            @Override
            public void onEnd() {
                stopPlay();

                onActionUp();
            }
        });

        binding.photoView.setOnPhotoTapListener(new OnPhotoTapListener() {
            @Override
            public void onPhotoTap(ImageView view, float x, float y) {
                if (isBottomShowing) {
                    return;
                }

                getParentViewModel().getTapLiveData().setValue(0);
            }
        });

        binding.photoView.setOnViewDragListener(new OnViewDragListener() {
            @Override
            public void onDrag(ScrollDirection direction, float dx, float dy) {
                onPhotoViewDrag(direction, dy);
            }
        });

        binding.photoView.setOnLongClickListener(new View.OnLongClickListener() {
            @Override
            public boolean onLongClick(View v) {
                playLivePhotoVideo();
                return false;
            }
        });
    }

    /**
     * bottomLayout's height is 2/3 of screen height
     */
    private void initBottomDetailLayout() {
        int height = screenHeight / 3 * 2;
        FrameLayout.LayoutParams flp = (FrameLayout.LayoutParams) binding.bottomLayout.getLayoutParams();
        flp.height = height;
        binding.bottomLayout.setLayoutParams(flp);
        binding.bottomLayout.setTranslationY(screenHeight);

        binding.bottomScrollView.setOnViewActionEndListener(new OnViewActionEndListener() {
            @Override
            public void onEnd() {
                onActionUp();
            }
        });

        binding.bottomScrollView.setOnViewDragListener(new OnViewDragListener() {
            @Override
            public void onDrag(ScrollDirection direction, float dx, float dy) {
                // 修改滚动阈值判断逻辑
                if (direction == ScrollDirection.RIGHT || direction == ScrollDirection.LEFT) {
                    binding.bottomScrollView.requestDisallowInterceptTouchEvent(false);
                } else {
                    binding.bottomScrollView.requestDisallowInterceptTouchEvent(true);
                    onPhotoViewDrag(direction, dy);
                }
            }
        });
    }

    private boolean isBottomShowing = false;

    public boolean isBottomShowing() {
        return isBottomShowing;
    }

    private final int screenHeight = ScreenUtils.getScreenHeight();

    private int photoTranslationY;
    private int bottomTranslationY = screenHeight;

    private final int animateDuration = 200;
    /**
     * current scroll distance
     */
    private float totalDistance = 0;

    /**
     * if the scroll exceeds this distance, when the gesture is ACTION_UP, it will automatically expand or close
     */
    private final int triggerDistance = SizeUtils.dp2px(100);

    private void onActionUp() {
        if (!canScrollBottomLayout) {
            return;
        }

        if (totalDistance == 0) {
            return;
        }

        ScrollDirection scrollDirection;
        if (isBottomShowing) {
            scrollDirection = ScrollDirection.DOWN;
        } else {
            scrollDirection = ScrollDirection.UP;
        }

        binding.bottomLayout.requestDisallowInterceptTouchEvent(false);
        if (totalDistance < triggerDistance) {

            DetailLayoutShowModel showModel = new DetailLayoutShowModel((int) totalDistance, scrollDirection, ScrollStatus.CANCELLED, isBottomShowing);
            getParentViewModel().getScrolling().setValue(showModel);

            if (imagePreviewHelper != null) {
                imagePreviewHelper.gradientLayout(showModel);
            }

            //notice:
            //toggle first, in order to show the animation
            toggleBottomShowingValue();

            //
            toggleDetailLayout();

            //toggle again
            toggleBottomShowingValue();
            return;
        }

        DetailLayoutShowModel showModel = new DetailLayoutShowModel((int) totalDistance, scrollDirection, ScrollStatus.FINISHED, isBottomShowing);
        getParentViewModel().getScrolling().setValue(showModel);

        if (imagePreviewHelper != null) {
            imagePreviewHelper.gradientLayout(showModel);
        }

        toggleDetailLayout();
        toggleBottomShowingValue();
    }

    private void onPhotoViewDrag(ScrollDirection scrollDirection, float dY) {
        if (!canScrollBottomLayout) {
            return;
        }

        //When the bottomLayout is already fully displayed, no further scrolling up is allowed
        if (bottomTranslationY == screenHeight && dY > 0) {
            return;
        }

        //When the bottomLayout is already fully hidden, no further scrolling down is allowed
        if (bottomTranslationY <= screenHeight / 3 && dY < 0) {
            return;
        }

        totalDistance += Math.abs(dY);

        DetailLayoutShowModel showModel = new DetailLayoutShowModel((int) totalDistance, scrollDirection, ScrollStatus.SCROLLING, isBottomShowing);
        getParentViewModel().getScrolling().setValue(showModel);

        if (imagePreviewHelper != null) {
            imagePreviewHelper.gradientLayout(showModel);
        }

        photoTranslationY += (int) (dY);
        bottomTranslationY += (int) (dY * 2f);

        binding.photoView.setTranslationY(photoTranslationY);
        binding.bottomLayout.setTranslationY(bottomTranslationY);

//        SLogs.e("totalDis = " + totalDistance + ", photoTY = " + photoTranslationY + ", bottomTY = " + bottomTranslationY + ", dY = " + dY);
    }

    public void toggle() {
        toggleDetailLayout();
        toggleBottomShowingValue();
    }

    private void toggleDetailLayout() {
        if (!canScrollBottomLayout) {
            return;
        }

//        if (binding.errorView.getVisibility() == VISIBLE) {
//            return;
//        }

        if (isBottomShowing) {
            //Automatically scroll to the bottom of the screen
            photoTranslationY = 0;
            bottomTranslationY = screenHeight;
        } else {
            //Automatically scrolls to 1/3 of the screen
            bottomTranslationY = screenHeight / 3;

            //
            RectF displayRect = binding.photoView.getDisplayRect();
            if (displayRect != null) {
                float imageCenterY = (displayRect.top + displayRect.bottom) / 2;
                photoTranslationY = (int) (screenHeight / 6f - imageCenterY);
            }
        }
        totalDistance = 0;

        binding.photoView.animate()
                .translationY(photoTranslationY)
                .setDuration(animateDuration)
                .start();

        binding.bottomLayout.animate()
                .translationY(bottomTranslationY)
                .setDuration(animateDuration)
                .start();
    }

    private void toggleBottomShowingValue() {
        isBottomShowing = !isBottomShowing;
    }


    private void load() {
        if (binding.progressBar.getVisibility() == GONE) {
            binding.progressBar.setVisibility(VISIBLE);
        }

        if (binding.errorView.getVisibility() == VISIBLE) {
            binding.errorView.setVisibility(GONE);
        }

        if (!TextUtils.isEmpty(imageUrl)) {
            loadUrl(imageUrl);
        } else {
            loadThumbnailAndRequestRawUrl();
        }

        getViewModel().getFileDetail(repoId, fullPath);
    }

    //remote url
    private void loadUrl(String url) {
        GlideApp.with(this)
                .load(url)
                .diskCacheStrategy(DiskCacheStrategy.AUTOMATIC)
                .transition(DrawableTransitionOptions.withCrossFade())
                .listener(new RequestListener<Drawable>() {
                    @Override
                    public boolean onLoadFailed(@Nullable GlideException e, @Nullable Object model, @NonNull Target<Drawable> target, boolean isFirstResource) {
                        binding.progressBar.setVisibility(GONE);
                        binding.errorView.setVisibility(VISIBLE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(@NonNull Drawable resource, @NonNull Object model, Target<Drawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(GONE);
                        return false;
                    }
                })
                .into(binding.photoView);
    }

    private void loadThumbnailAndRequestRawUrl() {
        getViewModel().checkLocal(repoId, fullPath);
    }

    // local image file
    private void loadLocalImageFile(String oriUrl) {
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
        //new GlideImage()
        GlideApp.with(this)
                .load(oriUrl)
                .diskCacheStrategy(DiskCacheStrategy.NONE)
                .transition(DrawableTransitionOptions.withCrossFade())
                .listener(new RequestListener<Drawable>() {
                    @Override
                    public boolean onLoadFailed(@Nullable GlideException e, @Nullable Object model, @NonNull Target<Drawable> target, boolean isFirstResource) {
                        binding.progressBar.setVisibility(GONE);
                        binding.errorView.setVisibility(VISIBLE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(@NonNull Drawable resource, @NonNull Object model, Target<Drawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(GONE);
                        try {
                            //
                            checkMotionPhoto(oriUrl);

                            //
                            HashMap<String, String> hashMap = loadExifMeta(oriUrl);
                            addTextView(hashMap);
                        } catch (Exception e) {
                            SLogs.e(e.getMessage());
                        }
                        return false;
                    }
                })
                .into(binding.photoView);
    }


    // no load yet.
    private int motionPhotoType = -1;

    private void checkMotionPhoto(String localPath) {
        if (motionPhotoType == -1) {
            if (Utils.isJpeg(localPath)) {
                MotionPhotoDescriptor descriptor = MotionPhotoDetector.extractJpegXmp(new File(localPath));
                if (descriptor.isMotionPhoto()) {
                    motionPhotoType = HeicNative.MOTION_PHOTO_TYPE_JPEG;
                }
            } else if (Utils.isHeic(localPath)) {
                MotionPhotoDescriptor descriptor = MotionPhotoDetector.extractHeicXmp(new File(localPath));
                if (descriptor.isMotionPhoto()) {
                    motionPhotoType = HeicNative.MOTION_PHOTO_TYPE_HEIC;
                }
            }
        }

        if (motionPhotoType == HeicNative.MOTION_PHOTO_TYPE_HEIC) {
            binding.btnLivePhoto.setVisibility(VISIBLE);
            if (canScrollBottomLayout) {
                imagePreviewHelper.setActionViews(binding.btnLivePhoto);
            }
        } else if (motionPhotoType == HeicNative.MOTION_PHOTO_TYPE_JPEG) {
            binding.btnLivePhoto.setVisibility(VISIBLE);
            if (canScrollBottomLayout) {
                imagePreviewHelper.setActionViews(binding.btnLivePhoto);
            }
        } else {
            binding.btnLivePhoto.setVisibility(GONE);
        }
    }

    private ExoPlayer exoPlayer;

    @Override
    public void onDestroyView() {
        super.onDestroyView();

        releasePlayer();
    }

    @Override
    public void onPause() {
        super.onPause();

        stopPlay();
    }

    @OptIn(markerClass = Unstable.class)
    private void playLivePhotoVideo() {

        try {
            if (motionPhotoType == -1) {
                return;
            }

            if (exoPlayer != null && exoPlayer.isPlaying()) {
                return;
            }

            MediaSource source = buildMotionPhotoMediaSource(destinationFile);
            if (source == null) {
                return;
            }

            if (exoPlayer == null) {
                exoPlayer = new ExoPlayer.Builder(requireContext()).build();
                exoPlayer.setRepeatMode(Player.REPEAT_MODE_ALL);
                exoPlayer.addListener(new Player.Listener() {
                    @Override
                    public void onPlaybackStateChanged(int playbackState) {
                        switch (playbackState) {
                            case Player.STATE_BUFFERING: //loading

                                break;
                            case Player.STATE_READY:
                                binding.playerView.setVisibility(View.VISIBLE);
                                binding.photoView.setVisibility(GONE);
                                break;
                            case Player.STATE_ENDED:
                                binding.photoView.setVisibility(VISIBLE);
                                binding.playerView.setVisibility(View.GONE);
                                break;
                        }
                    }
                });
//                Glide.with(requireContext()).load(destinationFile)
//                        .into(new CustomTarget<Drawable>() {
//                            @Override
//                            public void onResourceReady(@NonNull Drawable resource, @Nullable Transition<? super Drawable> transition) {
//                                binding.playerView.setDefaultArtwork(resource);
//                                binding.playerView.setArtworkDisplayMode(PlayerView.IMAGE_DISPLAY_MODE_FIT);
//                            }
//
//                            @Override
//                            public void onLoadCleared(@Nullable Drawable placeholder) {
//                                binding.playerView.setDefaultArtwork(null);
//                            }
//                        });
                binding.playerView.setPlayer(exoPlayer);
            }

            exoPlayer.setMediaSource(source);
            exoPlayer.prepare();
            exoPlayer.play();
            //
            vibrateOnce();
        } catch (IOException | XMPException e) {
            throw new RuntimeException(e);
        }
    }

    private void vibrateOnce() {
        try {
            Vibrator vibrator = (Vibrator) requireContext().getSystemService(Context.VIBRATOR_SERVICE);
            if (vibrator == null) return;
            long durationMs = 30L;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                VibrationEffect effect = VibrationEffect.createOneShot(durationMs, VibrationEffect.DEFAULT_AMPLITUDE);
                vibrator.vibrate(effect);
            } else {
                vibrator.vibrate(durationMs);
            }
        } catch (Throwable ignored) {
        }
    }

    private void stopPlay() {
        if (exoPlayer == null) {
            return;
        }

        if (!exoPlayer.isPlaying()) {
            return;
        }

        exoPlayer.stop();
        exoPlayer.clearMediaItems();

        binding.playerView.setVisibility(GONE);
        binding.photoView.setVisibility(VISIBLE);
    }

    private void releasePlayer() {
        if (exoPlayer != null) {
            exoPlayer.release();
            exoPlayer = null;
        }
    }


    private MediaSource buildMotionPhotoMediaSource(File imageFile) throws IOException, XMPException {
        if (motionPhotoType == -1) {
            motionPhotoType = HeicNative.nativeCheckMotionPhotoType(imageFile.getAbsolutePath());
        }

        byte[] videoBytes = null;
        if (motionPhotoType == HeicNative.MOTION_PHOTO_TYPE_HEIC) {
            videoBytes = HeicNative.nativeExtractHeicMotionPhotoVideo(imageFile.getAbsolutePath());
        } else if (motionPhotoType == HeicNative.MOTION_PHOTO_TYPE_JPEG) {
            videoBytes = HeicNative.nativeExtractJpegMotionPhotoVideo(imageFile.getAbsolutePath());
        }

        if (videoBytes == null || videoBytes.length == 0) {
            return null;
        }

        androidx.media3.datasource.DataSource.Factory factory = new MotionPhotoDataSourceFactory(videoBytes);

        MediaItem mediaItem = new MediaItem.Builder()
                .setUri(Uri.fromFile(imageFile))
                .build();

        return new ProgressiveMediaSource.Factory(factory)
                .createMediaSource(mediaItem);
    }


    // local gif file
    private void loadLocalGifFile(String rawUrl) {
        GlideApp.with(this)
                .asGif()
                .load(rawUrl)
                .diskCacheStrategy(DiskCacheStrategy.NONE)//
                .placeholder(binding.photoView.getDrawable())
                .listener(new RequestListener<GifDrawable>() {
                    @Override
                    public boolean onLoadFailed(@Nullable GlideException e, @Nullable Object model, @NonNull Target<GifDrawable> target, boolean isFirstResource) {
                        binding.progressBar.setVisibility(GONE);
                        binding.errorView.setVisibility(VISIBLE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(@NonNull GifDrawable resource, @NonNull Object model, Target<GifDrawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(GONE);
                        // 图片加载成功
                        SLogs.d(TAG, "loadOriGifUrl()", dataSource.name() + ": " + isFirstResource + ": " + rawUrl);
                        return false;
                    }
                })
                .into(binding.photoView);
    }

    private String convertThumbnailUrl(String fullPath) {
        if (TextUtils.isEmpty(serverUrl)) {
            return null;
        }

        return ThumbnailUtils.convertThumbnailUrl(serverUrl, repoId, fullPath);
    }

    private HashMap<String, String> loadExifMeta(String localPath) {
        HashMap<String, String> exifMap = new HashMap<>();

        try {
            ExifInterface exifInterface = new ExifInterface(localPath);

            if (!exifInterface.hasAttribute(ExifInterface.TAG_MODEL)) {
                return exifMap;
            }

            // 1. 读取相机型号
            String cameraModel = exifInterface.getAttribute(ExifInterface.TAG_MODEL);
            SLogs.d(TAG, "ExifData", "相机型号: " + cameraModel);
            if (!TextUtils.isEmpty(cameraModel)) {
                exifMap.put("_model", cameraModel);
            }

            // 2. 读取创建时间
            String dateTime = exifInterface.getAttribute(ExifInterface.TAG_DATETIME);
            if (TextUtils.isEmpty(dateTime)) {
                dateTime = exifInterface.getAttribute(ExifInterface.TAG_DATETIME_ORIGINAL);
            }
            SLogs.d(TAG, "ExifData", "创建时间: " + dateTime);
            if (!TextUtils.isEmpty(dateTime)) {
                long m = TimeUtils.string2Millis(dateTime, "yyyy:MM:dd HH:mm:ss");
                String d = TimeUtils.millis2String(m, "yyyy-MM-dd HH:mm:ss");
                exifMap.put("_datetime", d);
            }


            // 3. 读取尺寸（需要额外处理）
            int width = exifInterface.getAttributeInt(ExifInterface.TAG_IMAGE_WIDTH, 0);
            int height = exifInterface.getAttributeInt(ExifInterface.TAG_IMAGE_LENGTH, 0);
            String wh = width + "x" + height;
            exifMap.put("_width_height", wh);
            SLogs.d(TAG, "ExifData", "尺寸: " + wh);

            // 4. 读取色彩空间
            String colorSpace = exifInterface.getAttribute(ExifInterface.TAG_COLOR_SPACE);
            SLogs.d(TAG, "ExifData", "色彩空间: " + colorSpace);
            if (!TextUtils.isEmpty(colorSpace)) {
                exifMap.put("_color_space", colorSpace);
            }

//            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
//                ImageDecoder.Source source = ImageDecoder.createSource(new File(localPath));
//                ImageDecoder.decodeBitmap(source, new ImageDecoder.OnHeaderDecodedListener() {
//                    @Override
//                    public void onHeaderDecoded(@NonNull ImageDecoder decoder, @NonNull ImageDecoder.ImageInfo info, @NonNull ImageDecoder.Source source) {
//                        ColorSpace colorSpace1 = info.getColorSpace();
//                        colorSpace1.getName();
//                    }
//                });
//            }

            // 5. 读取焦距
            String focalLength = exifInterface.getAttribute(ExifInterface.TAG_FOCAL_LENGTH);
            SLogs.d(TAG, "ExifData", "焦距: " + focalLength);
            if (!TextUtils.isEmpty(focalLength)) {
                exifMap.put("_focal_length", focalLength);
            }

            // 6. 读取光圈值
            String apertureValue = exifInterface.getAttribute(ExifInterface.TAG_APERTURE_VALUE);
            SLogs.d(TAG, "ExifData", "光圈值: " + apertureValue);
            if (!TextUtils.isEmpty(apertureValue)) {
                exifMap.put("_aperture_value", apertureValue);
            }

            // 7. 读取光圈数（F-number）
            String fNumber = exifInterface.getAttribute(ExifInterface.TAG_F_NUMBER);
            SLogs.d(TAG, "ExifData", "光圈数: " + fNumber);
            if (!TextUtils.isEmpty(fNumber)) {
                double fDouble = Double.parseDouble(fNumber);
                exifMap.put("_f_nubmer", String.format(Locale.getDefault(), "%.2f", fDouble));
            }

            // 8. 读取曝光时间
            String exposureTime = exifInterface.getAttribute(ExifInterface.TAG_EXPOSURE_TIME);
            SLogs.d(TAG, "ExifData", "曝光时间: " + exposureTime);
            if (TextUtils.isEmpty(exposureTime)) {
                double exposureValue = Double.parseDouble(exposureTime);

                String formattedExposureTime;
                if (exposureValue < 1.0) {
                    double reciprocal = 1.0 / exposureValue;
                    long roundedReciprocal = Math.round(reciprocal);
                    formattedExposureTime = "1/" + roundedReciprocal;
                } else {
                    formattedExposureTime = String.format(Locale.getDefault(), "%.2f sec", exposureTime);
                }

                SLogs.d(TAG, "ExifData", "Formatted Exposure Time: " + formattedExposureTime);
                exifMap.put("_exposure_time", formattedExposureTime);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return exifMap;
    }

    private String getMapContent(HashMap<String, String> map, String key) {
        if (map == null || map.isEmpty()) {
            return "-";
        }
        String value = map.get(key);
        if (TextUtils.isEmpty(value)) {
            return "-";
        }

        return value;
    }

    private void addTextView(HashMap<String, String> map) throws Exception {
        if (map == null || map.isEmpty()) {
            return;
        }

        View view = LayoutInflater.from(requireContext()).inflate(R.layout.view_image_exif_container, null);
        ViewImageExifContainerBinding exifBinding = ViewImageExifContainerBinding.bind(view);

        String imageCaptureTime = getResources().getString(R.string.image_capture_time);
        String imageDimensions = getResources().getString(R.string.image_dimensions);
        String captureTime = imageCaptureTime + ": " + getMapContent(map, "_datetime");
        String imageSize = imageDimensions + ": " + getMapContent(map, "_width_height");

        exifBinding.exifModel.setText(getMapContent(map, "_model"));
        exifBinding.exifDatetime.setText(captureTime);
        exifBinding.exifWh.setText(imageSize);

        String colorSpace = getMapContent(map, "_color_space");
        if (TextUtils.equals(colorSpace, "-")) {
            exifBinding.exifColorSpace.setText(R.string.image_color_space_undefined);
        } else {
            int colorSpaceValue = Integer.parseInt(colorSpace);
            switch (colorSpaceValue) {
                case ExifInterface.COLOR_SPACE_S_RGB:
                    exifBinding.exifColorSpace.setText(R.string.image_color_space_rgb);
                    break;
                case ExifInterface.COLOR_SPACE_UNCALIBRATED:
                    exifBinding.exifColorSpace.setText(R.string.image_color_space_uncalibrated);
                    break;
                default:
                    exifBinding.exifColorSpace.setText(R.string.image_color_space_undefined);
                    break;
            }
        }

        String focalLength = getMapContent(map, "_focal_length");
        if (TextUtils.equals(focalLength, "-")) {
            exifBinding.exifFocalLength.setText(focalLength);
        } else if (focalLength.contains("/")) {
            String[] parts = focalLength.split("/");
            if (parts.length == 2) {
                float numerator = Float.parseFloat(parts[0]);
                float denominator = Float.parseFloat(parts[1]);
                float focalLengthValue = numerator / denominator;
                exifBinding.exifFocalLength.setText(focalLengthValue + " mm");

            } else {
                SLogs.d(TAG, "ExifData", "焦距: " + focalLength + " mm");
                exifBinding.exifFocalLength.setText(focalLength + " mm");
            }
        } else {
            exifBinding.exifFocalLength.setText(focalLength + " mm");
        }

        String apertureValue = getMapContent(map, "_aperture_value");
        if (TextUtils.equals(apertureValue, "-")) {
            exifBinding.exifApertureValue.setText(apertureValue);
        } else if (apertureValue.contains("/")) {
            String[] parts = apertureValue.split("/");
            if (parts.length == 2) {
                float numerator = Float.parseFloat(parts[0]);
                float denominator = Float.parseFloat(parts[1]);
                float aperture = numerator / denominator;
                String r = String.format(Locale.getDefault(), "%.2f", aperture);
                exifBinding.exifApertureValue.setText(r);

            } else {
                exifBinding.exifApertureValue.setText(apertureValue);
            }
        } else {
            exifBinding.exifApertureValue.setText(apertureValue);
        }

        //
        String fNumber = getMapContent(map, "_f_nubmer");
        if (TextUtils.equals(fNumber, "-")) {
            exifBinding.exifFNumber.setText(fNumber);
        } else {
            exifBinding.exifFNumber.setText("f/" + fNumber);
        }

        String formattedExposureTime = getMapContent(map, "_exposure_time");
        if (TextUtils.equals(formattedExposureTime, "-")) {
            exifBinding.exifExposureTime.setText(formattedExposureTime);
        } else {
            exifBinding.exifExposureTime.setText(formattedExposureTime + "s");
        }

        if (isNightMode) {
            exifBinding.getRoot().setBackgroundResource(R.drawable.shape_solid_grey900_radius_8);
            exifBinding.exifModel.setBackgroundResource(R.drawable.shape_solid_grey700_radius_8);
        } else {
            exifBinding.getRoot().setBackgroundResource(R.drawable.shape_solid_grey200_radius_8);
            exifBinding.exifModel.setBackgroundResource(R.drawable.shape_solid_grey309_radius_8);
        }

        LinearLayout.LayoutParams vl = new LinearLayout.LayoutParams(-1, -2);
        vl.topMargin = Constants.DP.DP_16;
        vl.leftMargin = Constants.DP.DP_16;
        vl.rightMargin = Constants.DP.DP_16;

        binding.bottomDetailsContainer.addView(exifBinding.getRoot(), vl);
    }


}
