package com.seafile.seadroid2.ui.media.image_preview;

import static android.view.View.VISIBLE;

import android.content.res.Configuration;
import android.graphics.RectF;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.MimeTypeMap;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.exifinterface.media.ExifInterface;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.blankj.utilcode.util.SpanUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.bumptech.glide.load.DataSource;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.load.engine.GlideException;
import com.bumptech.glide.load.resource.drawable.DrawableTransitionOptions;
import com.bumptech.glide.load.resource.gif.GifDrawable;
import com.bumptech.glide.request.RequestListener;
import com.bumptech.glide.request.target.Target;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.databinding.FragmentPhotoViewBinding;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.ThumbnailUtils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.ui.media.image_preview2.DetailLayoutShowModel;
import com.seafile.seadroid2.view.SDocDetailView;
import com.seafile.seadroid2.view.photoview.OnPhotoTapListener;
import com.seafile.seadroid2.view.photoview.OnViewActionEndListener;
import com.seafile.seadroid2.view.photoview.OnViewDragListener;
import com.seafile.seadroid2.view.photoview.ScrollDirection;
import com.seafile.seadroid2.view.photoview.ScrollStatus;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;

public class PhotoFragment extends BaseFragment {

    private FragmentPhotoViewBinding binding;

    private String repoId, repoName, fullPath;
    private String imageUrl;
    private boolean isNightMode = false;
    private boolean canScrollDetailLayout = true;

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

    private ImagePreviewViewModel parentViewModel;
    private PhotoViewModel viewModel;

    public PhotoViewModel getViewModel() {
        return viewModel;
    }

    public ImagePreviewViewModel getParentViewModel() {
        return parentViewModel;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle args = getArguments();
        if (args == null) {
            return;
        }

        int currentNightMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        isNightMode = currentNightMode == Configuration.UI_MODE_NIGHT_YES;


        repoId = args.getString("repoId");
        repoName = args.getString("repoName");
        fullPath = args.getString("fullPath");
        imageUrl = args.getString("image_url");
        serverUrl = args.getString("serverUrl");

        if (TextUtils.isEmpty(repoId) && TextUtils.isEmpty(imageUrl)) {
            throw new IllegalStateException("the args is invalid");
        }

        if (!TextUtils.isEmpty(imageUrl)) {
            canScrollDetailLayout = false;
        }

        viewModel = new ViewModelProvider(this).get(PhotoViewModel.class);
        parentViewModel = new ViewModelProvider(requireActivity()).get(ImagePreviewViewModel.class);
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

        intViewModel();

        initView();

        load();
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        return DataManager.getLocalRepoFile(account, repoId, repoName, fullPathInRepo);
    }

    private void intViewModel() {
        getViewModel().getSeafExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                binding.progressBar.setVisibility(View.GONE);
                binding.errorView.setVisibility(VISIBLE);
            }
        });

        getViewModel().getCheckLocalLiveData().observe(getViewLifecycleOwner(), new Observer<DirentModel>() {
            @Override
            public void onChanged(DirentModel direntModel) {
                if (direntModel == null || TextUtils.isEmpty(direntModel.uid)) {
                    binding.photoView.setImageResource(R.drawable.icon_image_error_filled);
                    binding.progressBar.setVisibility(View.GONE);
                    return;
                }

                File file = getLocalDestinationFile(direntModel.repo_id, direntModel.repo_name, direntModel.full_path);
                if (FileUtils.isFileExists(file)) {
                    if (isGif(fullPath)) {
                        loadOriGifUrl(file.getAbsolutePath());
                    } else {
                        loadOriUrl(file.getAbsolutePath());
                    }
                } else {
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

        getViewModel().getFileDetailLiveData().observe(getViewLifecycleOwner(), new Observer<FileProfileConfigModel>() {
            @Override
            public void onChanged(FileProfileConfigModel configModel) {

                SDocDetailView detailView = new SDocDetailView(requireContext());
                detailView.setData(configModel);

                binding.detailsContainer.setVisibility(VISIBLE);
                binding.detailsContainer.removeAllViews();

                LinearLayout.LayoutParams lp = new LinearLayout.LayoutParams(-1, -2);
                binding.detailsContainer.addView(detailView, lp);
            }
        });

        getViewModel().getSecondRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.detailsProgressBar.setVisibility(aBoolean ? VISIBLE : View.GONE);
            }
        });
    }

    private void initView() {
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
                onActionUp();
            }
        });

        binding.photoView.setOnPhotoTapListener(new OnPhotoTapListener() {
            @Override
            public void onPhotoTap(ImageView view, float x, float y) {
                if (isShowing) {
                    return;
                }

                if (onPhotoTapListener != null) {
                    onPhotoTapListener.onPhotoTap(view, x, y);
                }
            }
        });

        binding.photoView.setOnViewDragListener(new OnViewDragListener() {
            @Override
            public void onDrag(ScrollDirection direction, float dx, float dy) {
                onPhotoViewDrag(direction, dy);
            }
        });
    }

    /**
     * customFrameLayout's height is 2/3 of screen height
     */
    private void initBottomDetailLayout() {
        //translation
        int height = screenHeight / 3 * 2;
        FrameLayout.LayoutParams flp = (FrameLayout.LayoutParams) binding.customFrameLayout.getLayoutParams();
        flp.height = height;
        binding.customFrameLayout.setLayoutParams(flp);
        binding.customFrameLayout.setTranslationY(screenHeight);

        if (isNightMode) {
            binding.detailsContainer2.setBackgroundResource(R.drawable.shape_solid_grey900_radius_8);
            binding.exifModel.setBackgroundResource(R.drawable.shape_solid_grey700_radius_8);
        } else {
            binding.detailsContainer2.setBackgroundResource(R.drawable.shape_solid_grey200_radius_8);
            binding.exifModel.setBackgroundResource(R.drawable.shape_solid_grey309_radius_8);
        }

        binding.customFrameLayout.setOnViewActionEndListener(new OnViewActionEndListener() {
            @Override
            public void onEnd() {
                onActionUp();
            }
        });

        binding.customFrameLayout.setOnViewDragListener(new OnViewDragListener() {
            @Override
            public void onDrag(ScrollDirection direction, float dx, float dy) {
                if (direction == ScrollDirection.RIGHT || direction == ScrollDirection.LEFT) {
                    binding.customFrameLayout.requestDisallowInterceptTouchEvent(false);
                } else {
                    binding.customFrameLayout.requestDisallowInterceptTouchEvent(true);
                    onPhotoViewDrag(direction, dy);
                }
            }
        });
    }

    private boolean isShowing = false;

    public boolean isShowing() {
        return isShowing;
    }

    private final int screenHeight = ScreenUtils.getScreenHeight();

    private int photoTranslationY;
    private int detailTranslationY = screenHeight;

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
        if (!canScrollDetailLayout) {
            return;
        }

        if (totalDistance == 0) {
            return;
        }

        ScrollDirection scrollDirection;
        if (isShowing) {
            scrollDirection = ScrollDirection.DOWN;
        } else {
            scrollDirection = ScrollDirection.UP;
        }

        binding.customFrameLayout.requestDisallowInterceptTouchEvent(false);
        if (totalDistance < triggerDistance) {

            DetailLayoutShowModel showModel = new DetailLayoutShowModel((int) totalDistance, scrollDirection, ScrollStatus.CANCELLED, isShowing);
            getParentViewModel().getScrolling().setValue(showModel);

            //notice:
            //toggle first, in order to show the animation
            toggleShowingValue();

            //
            toggleDetailLayout();

            //toggle again
            toggleShowingValue();
            return;
        }

        DetailLayoutShowModel showModel = new DetailLayoutShowModel((int) totalDistance, scrollDirection, ScrollStatus.FINISHED, isShowing);
        getParentViewModel().getScrolling().setValue(showModel);

        toggleDetailLayout();
        toggleShowingValue();
    }

    private void onPhotoViewDrag(ScrollDirection scrollDirection, float dY) {
        if (!canScrollDetailLayout) {
            return;
        }

        if (detailTranslationY == screenHeight && dY > 0) {
            return;
        }

        if (detailTranslationY <= screenHeight / 3 && dY < 0) {
            return;
        }

        totalDistance += Math.abs(dY);

        DetailLayoutShowModel showModel = new DetailLayoutShowModel((int) totalDistance, scrollDirection, ScrollStatus.SCROLLING, isShowing);
        getParentViewModel().getScrolling().setValue(showModel);

        photoTranslationY += (int) (dY);
        detailTranslationY += (int) (dY * 2f);

        binding.photoView.setTranslationY(photoTranslationY);
        binding.customFrameLayout.setTranslationY(detailTranslationY);

//        SLogs.e("totalDis = " + totalDistance + ", pY = " + photoTranslationY + ", bY = " + detailTranslationY + ", dY = " + dY);
    }

    public void toggle() {
        toggleDetailLayout();
        toggleShowingValue();
    }

    private void toggleDetailLayout() {
        if (!canScrollDetailLayout) {
            return;
        }

        if (isShowing) {
            photoTranslationY = 0;
            detailTranslationY = screenHeight;
        } else {
            detailTranslationY = screenHeight / 3;

            RectF displayRect = binding.photoView.getDisplayRect();
            float imageCenterY = (displayRect.top + displayRect.bottom) / 2;
            photoTranslationY = (int) (screenHeight / 6f - imageCenterY);
        }
        totalDistance = 0;

        binding.photoView.animate()
                .translationY(photoTranslationY)
                .setDuration(animateDuration)
                .start();

        binding.customFrameLayout.animate()
                .translationY(detailTranslationY)
                .setDuration(animateDuration)
                .start();
    }

    private void toggleShowingValue() {
        isShowing = !isShowing;
    }


    private void load() {
        if (binding.progressBar.getVisibility() == View.GONE) {
            binding.progressBar.setVisibility(VISIBLE);
        }

        if (binding.errorView.getVisibility() == VISIBLE) {
            binding.errorView.setVisibility(View.GONE);
        }

        if (!TextUtils.isEmpty(imageUrl)) {
            loadUrl(imageUrl);
        } else {
            loadThumbnailAndRequestRawUrl();
        }

        getViewModel().getFileDetailModel(repoId, fullPath);
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
                        binding.errorView.setVisibility(VISIBLE);
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
                        binding.errorView.setVisibility(VISIBLE);
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(@NonNull Drawable resource, @NonNull Object model, Target<Drawable> target, @NonNull DataSource dataSource, boolean isFirstResource) {
                        binding.progressBar.setVisibility(View.GONE);
                        // 图片加载成功
//                        SLogs.e("原图：" + dataSource.name() + ": " + isFirstResource + ": " + oriCacheKey + ": " + oriUrl);

                        HashMap<String, String> hashMap = loadExifMeta(oriUrl);
                        addTextView(hashMap);
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
                        binding.errorView.setVisibility(VISIBLE);
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

    private HashMap<String, String> loadExifMeta(String localPath) {
        HashMap<String, String> exifMap = new HashMap<>();

        try {
            ExifInterface exifInterface = new ExifInterface(localPath);

            if (!exifInterface.hasAttribute(ExifInterface.TAG_MODEL)) {
                return exifMap;
            }

            // 1. 读取相机型号
            String cameraModel = exifInterface.getAttribute(ExifInterface.TAG_MODEL);
            SLogs.d("ExifData - 相机型号: " + cameraModel);
            exifMap.put("_model", cameraModel);

            // 2. 读取创建时间
            String dateTime = exifInterface.getAttribute(ExifInterface.TAG_DATETIME);
            if (TextUtils.isEmpty(dateTime)) {
                dateTime = exifInterface.getAttribute(ExifInterface.TAG_DATETIME_ORIGINAL);
            }

            if (!TextUtils.isEmpty(dateTime)) {
                long m = TimeUtils.string2Millis(dateTime, "yyyy:MM:dd HH:mm:ss");
                String d = TimeUtils.millis2String(m, "yyyy-MM-dd HH:mm:ss");
                SLogs.d("ExifData - 创建时间: " + d);
                exifMap.put("_datetime", d);
            }


            // 3. 读取尺寸（需要额外处理）
            int width = exifInterface.getAttributeInt(ExifInterface.TAG_IMAGE_WIDTH, 0);
            int height = exifInterface.getAttributeInt(ExifInterface.TAG_IMAGE_LENGTH, 0);
            String wh = width + "x" + height;
            SLogs.d("ExifData - 尺寸: " + wh);
            exifMap.put("_width_height", wh);

            // 4. 读取色彩空间
            String colorSpace = exifInterface.getAttribute(ExifInterface.TAG_COLOR_SPACE);
            SLogs.d("ExifData - 色彩空间: " + colorSpace);
            exifMap.put("_color_space", colorSpace);


            // 5. 读取焦距
            String focalLength = exifInterface.getAttribute(ExifInterface.TAG_FOCAL_LENGTH);
            SLogs.d("ExifData - 焦距: " + focalLength);
            exifMap.put("_focal_length", focalLength);

            // 6. 读取光圈值
            String apertureValue = exifInterface.getAttribute(ExifInterface.TAG_APERTURE_VALUE);
            SLogs.d("ExifData - 光圈值: " + apertureValue);
            exifMap.put("_aperture_value", apertureValue);

            // 7. 读取光圈数（F-number）
            String fNumber = exifInterface.getAttribute(ExifInterface.TAG_F_NUMBER);
            SLogs.d("ExifData - 光圈数: " + fNumber);
            exifMap.put("_f_nubmer", fNumber);

            // 8. 读取曝光时间
            String exposureTime = exifInterface.getAttribute(ExifInterface.TAG_EXPOSURE_TIME);
            SLogs.d("ExifData - 曝光时间: " + exposureTime);

            // 将曝光时间转换为分数形式
            if (exposureTime != null) {
                double exposureValue = Double.parseDouble(exposureTime);
                String formattedExposureTime = formatExposureTime(exposureValue);
                SLogs.d("ExifData - Formatted Exposure Time: " + formattedExposureTime);
                exifMap.put("_exposure_time", formattedExposureTime);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return exifMap;
    }

    private void addTextView(HashMap<String, String> map) {
        if (map == null || map.isEmpty()) {
            binding.detailsContainer2.setVisibility(View.GONE);
            return;
        }

        String imageCaptureTime = getResources().getString(R.string.image_capture_time);
        String imageDimensions = getResources().getString(R.string.image_dimensions);
        String captureTime = imageCaptureTime + ": " + map.get("_datetime");
        String imageSize = imageDimensions + ": " + map.get("_width_height");

        binding.detailsContainer2.setVisibility(VISIBLE);
        binding.exifModel.setText(map.get("_model"));
        binding.exifDatetime.setText(captureTime);
        binding.exifWh.setText(imageSize);


        String colorSpace = map.get("_color_space");
        if (!TextUtils.isEmpty(colorSpace)) {
            int colorSpaceValue = Integer.parseInt(colorSpace);
            switch (colorSpaceValue) {
                case ExifInterface.COLOR_SPACE_S_RGB:
                    binding.exifColorSpace.setText(R.string.image_color_space_rgb);
                    break;
                case ExifInterface.COLOR_SPACE_UNCALIBRATED:
                    binding.exifColorSpace.setText(R.string.image_color_space_uncalibrated);
                    break;
                default:
                    binding.exifColorSpace.setText(R.string.image_color_space_undefined);
                    break;
            }
        } else {
            binding.exifColorSpace.setText(R.string.image_color_space_undefined);
        }

        String focalLength = map.get("_focal_length");
        if (focalLength != null) {
            if (focalLength.contains("/")) {
                String[] parts = focalLength.split("/");
                if (parts.length == 2) {
                    float numerator = Float.parseFloat(parts[0]);
                    float denominator = Float.parseFloat(parts[1]);
                    float focalLengthValue = numerator / denominator;
                    binding.exifFocalLength.setText(focalLengthValue + " mm");

                } else {
                    SLogs.d("ExifData - 焦距: " + focalLength + " mm");
                    binding.exifFocalLength.setText(focalLength + " mm");
                }
            } else {
                binding.exifFocalLength.setText(focalLength + " mm");
            }
        }

        String apertureValue = map.get("_aperture_value");
        if (apertureValue != null) {
            if (apertureValue.contains("/")) {
                String[] parts = apertureValue.split("/");
                if (parts.length == 2) {
                    float numerator = Float.parseFloat(parts[0]);
                    float denominator = Float.parseFloat(parts[1]);
                    float aperture = numerator / denominator;
                    String r = String.format(Locale.getDefault(), "%.2f", aperture);
                    binding.exifApertureValue.setText(r);

                } else {
                    binding.exifApertureValue.setText(apertureValue);
                }
            } else {
                binding.exifApertureValue.setText(apertureValue);
            }
        }

        //
        String fNumber = map.get("_f_nubmer");
        binding.exifFNumber.setText("f/" + fNumber);

        String formattedExposureTime = map.get("_exposure_time");
        if (!TextUtils.isEmpty(formattedExposureTime)) {
            binding.exifExposureTime.setText(formattedExposureTime + "s");
        }
    }


    private String formatExposureTime(double exposureTime) {
        if (exposureTime < 1.0) {
            double reciprocal = 1.0 / exposureTime;
            long roundedReciprocal = Math.round(reciprocal);
            return "1/" + roundedReciprocal;
        } else {
            return String.format(Locale.getDefault(), "%.2f sec", exposureTime);
        }
    }


}
