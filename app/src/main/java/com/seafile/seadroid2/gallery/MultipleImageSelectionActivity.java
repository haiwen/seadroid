/*
 * Multiple Image Selection Activity, modified from Android's Gallery App.
 * @date 2013-04
 * @see https://android.googlesource.com/platform/packages/apps/Gallery/
 */

package com.seafile.seadroid2.gallery;

import java.util.ArrayList;
import java.util.HashSet;

import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.preference.PreferenceManager;
import android.provider.MediaStore;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.Button;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.seafile.seadroid2.R;

public class MultipleImageSelectionActivity extends NoSearchActivity implements
        GridViewSpecial.Listener, GridViewSpecial.DrawAdapter {
    private static final String STATE_SCROLL_POSITION = "scroll_position";
    private static final String STATE_SELECTED_INDEX = "first_index";

    private static final String TAG = "MultipleImageSelectionActivity";
    private static final float INVALID_POSITION = -1f;
    private ImageManager.ImageListParam mParam;
    private IImageList mAllImages;
    private int mInclusion;
    boolean mSortAscending = false;
    private View mNoImagesView;
    private TextView mSelectionStatus;
    public static final int CROP_MSG = 2;

    private Dialog mMediaScanningDialog;
    // private MenuItem mSlideShowItem;
    private SharedPreferences mPrefs;
    private long mVideoSizeLimit = Long.MAX_VALUE;
    private View mSelectionFooterView;

    private BroadcastReceiver mReceiver = null;

    private final Handler mHandler = new Handler();
    private boolean mLayoutComplete;
    private boolean mPausing = true;
    private ImageLoader mLoader;
    private GridViewSpecial mGvs;

    private Uri mCropResultUri;

    // The index of the first picture in GridViewSpecial.
    private int mSelectedIndex = GridViewSpecial.INDEX_NONE;
    private float mScrollPosition = INVALID_POSITION;
    private boolean mConfigurationChanged = false;

    private HashSet<IImage> mMultiSelected = Sets.newHashSet();

    @Override
    public void onCreate(Bundle icicle) {
        super.onCreate(icicle);

        mPrefs = PreferenceManager.getDefaultSharedPreferences(this);

        // Must be called before setContentView().
        requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);

        setContentView(R.layout.multiple_image_selection);

        getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE,
                R.layout.custom_gallery_title);

        mNoImagesView = findViewById(R.id.no_images);
        mSelectionStatus = (TextView)findViewById(R.id.upload_selection_status);
        mSelectionFooterView = findViewById(R.id.selection_footer);

        mGvs = (GridViewSpecial) findViewById(R.id.image_grid);
        mGvs.setListener(this);

        // mFooterOrganizeView = findViewById(R.id.footer_organize);

        // consume all click events on the footer view
        // mFooterOrganizeView.setOnClickListener(Util.getNullOnClickListener());
        // initializeFooterButtons();

        initializeButtons();

        mVideoSizeLimit = getIntent().getLongExtra(
                MediaStore.EXTRA_SIZE_LIMIT, Long.MAX_VALUE);

        setupTitle();

        mLoader = new ImageLoader(getContentResolver(), mHandler);
    }

    private void initializeButtons() {
        Button cancelButton = (Button) findViewById(R.id.button_cancel_upload);
        cancelButton.setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
                finish();
            }
        });

        Button confirmButton = (Button) findViewById(R.id.button_confirm_upload);
        confirmButton.setOnClickListener(new OnClickListener() {
            public void onClick(View v) {
                setResultAndFinish();
            }
        });
    }

    private void setResultAndFinish() {
        Intent result = new Intent();
        ArrayList<String> selected = Lists.newArrayList();
        for (IImage image : mMultiSelected) {
            selected.add(image.getDataPath());
        }

        result.putExtra("photos", selected);
        setResult(RESULT_OK, result);
        finish();
    }

    // private void initializeFooterButtons() {
    //     Button deleteButton = (Button) findViewById(R.id.button_delete);
    //     deleteButton.setOnClickListener(new OnClickListener() {
    //         public void onClick(View v) {
    //             onDeleteMultipleClicked();
    //         }
    //     });

    //     Button shareButton = (Button) findViewById(R.id.button_share);
    //     shareButton.setOnClickListener(new OnClickListener() {
    //         public void onClick(View v) {
    //             onShareMultipleClicked();
    //         }
    //     });

    //     Button closeButton = (Button) findViewById(R.id.button_close);
    //     closeButton.setOnClickListener(new OnClickListener() {
    //         public void onClick(View v) {
    //             closeMultiSelectMode();
    //         }
    //     });
    // }

    // private MenuItem addSlideShowMenu(Menu menu) {
    //     return menu.add(Menu.NONE, Menu.NONE, MenuHelper.POSITION_SLIDESHOW,
    //             R.string.slide_show)
    //             .setOnMenuItemClickListener(
    //             new MenuItem.OnMenuItemClickListener() {
    //                 public boolean onMenuItemClick(MenuItem item) {
    //                     return onSlideShowClicked();
    //                 }
    //             }).setIcon(android.R.drawable.ic_menu_slideshow);
    // }

    // public boolean onSlideShowClicked() {
    //     if (!canHandleEvent()) {
    //         return false;
    //     }
    //     IImage img = getCurrentImage();
    //     if (img == null) {
    //         img = mAllImages.getImageAt(0);
    //         if (img == null) {
    //             return true;
    //         }
    //     }
    //     Uri targetUri = img.fullSizeImageUri();
    //     Uri thisUri = getIntent().getData();
    //     if (thisUri != null) {
    //         String bucket = thisUri.getQueryParameter("bucketId");
    //         if (bucket != null) {
    //             targetUri = targetUri.buildUpon()
    //                     .appendQueryParameter("bucketId", bucket)
    //                     .build();
    //         }
    //     }
    //     Intent intent = new Intent(Intent.ACTION_VIEW, targetUri);
    //     intent.putExtra("slideshow", true);
    //     startActivity(intent);
    //     return true;
    // }

    // private final Runnable mDeletePhotoRunnable = new Runnable() {
    //     public void run() {
    //         if (!canHandleEvent()) return;

    //         IImage currentImage = getCurrentImage();

    //         // The selection will be cleared when mGvs.stop() is called, so
    //         // we need to call getCurrentImage() before mGvs.stop().
    //         mGvs.stop();

    //         if (currentImage != null) {
    //             mAllImages.removeImage(currentImage);
    //         }
    //         mGvs.setImageList(mAllImages);
    //         mGvs.start();

    //         mNoImagesView.setVisibility(mAllImages.isEmpty()
    //                 ? View.VISIBLE
    //                 : View.GONE);
    //     }
    // };

    // private Uri getCurrentImageUri() {
    //     IImage image = getCurrentImage();
    //     if (image != null) {
    //         return image.fullSizeImageUri();
    //     } else {
    //         return null;
    //     }
    // }

    // private IImage getCurrentImage() {
    //     int currentSelection = mGvs.getCurrentSelection();
    //     if (currentSelection < 0
    //             || currentSelection >= mAllImages.getCount()) {
    //         return null;
    //     } else {
    //         return mAllImages.getImageAt(currentSelection);
    //     }
    // }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        mConfigurationChanged = true;
    }

    boolean canHandleEvent() {
        // Don't process event in pause state.
        return (!mPausing) && (mLayoutComplete);
    }

    // @Override
    // public boolean onKeyDown(int keyCode, KeyEvent event) {
    //     if (!canHandleEvent()) return false;
    //     switch (keyCode) {
    //         case KeyEvent.KEYCODE_DEL:
    //             IImage image = getCurrentImage();
    //             if (image != null) {
    //                 MenuHelper.deleteImage(
    //                         this, mDeletePhotoRunnable, getCurrentImage());
    //             }
    //             return true;
    //     }
    //     return super.onKeyDown(keyCode, event);
    // }

    // private boolean isPickIntent() {
    //     String action = getIntent().getAction();
    //     return (Intent.ACTION_PICK.equals(action)
    //             || Intent.ACTION_GET_CONTENT.equals(action));
    // }

    // private void launchCropperOrFinish(IImage img) {
    //     Bundle myExtras = getIntent().getExtras();

    //     long size = MenuHelper.getImageFileSize(img);
    //     if (size < 0) {
    //         // Return if the image file is not available.
    //         return;
    //     }

    //     if (size > mVideoSizeLimit) {
    //         DialogInterface.OnClickListener buttonListener =
    //                 new DialogInterface.OnClickListener() {
    //             public void onClick(DialogInterface dialog, int which) {
    //                 dialog.dismiss();
    //             }
    //         };
    //         new AlertDialog.Builder(this)
    //                 .setIcon(android.R.drawable.ic_dialog_info)
    //                 .setTitle(R.string.file_info_title)
    //                 .setMessage(R.string.video_exceed_mms_limit)
    //                 .setNeutralButton(R.string.details_ok, buttonListener)
    //                 .show();
    //         return;
    //     }

    //     String cropValue = myExtras != null ? myExtras.getString("crop") : null;
    //     if (cropValue != null) {
    //         Bundle newExtras = new Bundle();
    //         if (cropValue.equals("circle")) {
    //             newExtras.putString("circleCrop", "true");
    //         }

    //         Intent cropIntent = new Intent();
    //         cropIntent.setData(img.fullSizeImageUri());
    //         cropIntent.setClass(this, CropImage.class);
    //         cropIntent.putExtras(newExtras);

    //         /* pass through any extras that were passed in */
    //         cropIntent.putExtras(myExtras);
    //         startActivityForResult(cropIntent, CROP_MSG);
    //     } else {
    //         Intent result = new Intent(null, img.fullSizeImageUri());
    //         if (myExtras != null && myExtras.getBoolean("return-data")) {
    //             // The size of a transaction should be below 100K.
    //             Bitmap bitmap = img.fullSizeBitmap(
    //                     IImage.UNCONSTRAINED, 100 * 1024);
    //             if (bitmap != null) {
    //                 result.putExtra("data", bitmap);
    //             }
    //         }
    //         setResult(RESULT_OK, result);
    //         finish();
    //     }
    // }

    // @Override
    // protected void onActivityResult(int requestCode, int resultCode,
    //         Intent data) {
    //     switch (requestCode) {
    //         case MenuHelper.RESULT_COMMON_MENU_CROP: {
    //             if (resultCode == RESULT_OK) {

    //                 // The CropImage activity passes back the Uri of the cropped
    //                 // image as the Action rather than the Data.
    //                 // We store this URI so we can move the selection box to it
    //                 // later.
    //                 mCropResultUri = Uri.parse(data.getAction());
    //             }
    //             break;
    //         }
    //         case CROP_MSG: {
    //             if (resultCode == RESULT_OK) {
    //                 setResult(resultCode, data);
    //                 finish();
    //             }
    //             break;
    //         }
    //     }
    // }

    @Override
    public void onPause() {
        super.onPause();
        mPausing = true;

        mLoader.stop();

        mGvs.stop();

        if (mReceiver != null) {
            unregisterReceiver(mReceiver);
            mReceiver = null;
        }

        // Now that we've paused the threads that are using the cursor it is
        // safe to close it.
        mAllImages.close();
        mAllImages = null;
    }

    private void rebake(boolean unmounted, boolean scanning) {
        mGvs.stop();
        if (mAllImages != null) {
            mAllImages.close();
            mAllImages = null;
        }

        if (mMediaScanningDialog != null) {
            mMediaScanningDialog.cancel();
            mMediaScanningDialog = null;
        }

        if (scanning) {
            mMediaScanningDialog = ProgressDialog.show(
                    this,
                    null,
                    getResources().getString(R.string.wait),
                    true,
                    true);
        }

        mParam = allImages(!unmounted && !scanning);
        mAllImages = ImageManager.makeImageList(getContentResolver(), mParam);

        mGvs.setImageList(mAllImages);
        mGvs.setDrawAdapter(this);
        mGvs.setLoader(mLoader);
        mGvs.start();

        if (mAllImages.getCount() > 0) {
            mGvs.setVisibility(View.VISIBLE);
            mSelectionFooterView.setVisibility(View.VISIBLE);
            mNoImagesView.setVisibility(View.GONE);
        } else {
            mGvs.setVisibility(View.GONE);
            mSelectionFooterView.setVisibility(View.GONE);
            mNoImagesView.setVisibility(View.VISIBLE);
        }
    }

    @Override
    protected void onSaveInstanceState(Bundle state) {
        super.onSaveInstanceState(state);
        state.putFloat(STATE_SCROLL_POSITION, mScrollPosition);
        state.putInt(STATE_SELECTED_INDEX, mSelectedIndex);
    }

    @Override
    protected void onRestoreInstanceState(Bundle state) {
        super.onRestoreInstanceState(state);
        mScrollPosition = state.getFloat(
                STATE_SCROLL_POSITION, INVALID_POSITION);
        mSelectedIndex = state.getInt(STATE_SELECTED_INDEX, 0);
    }

    @Override
    public void onResume() {
        super.onResume();

        mGvs.setSizeChoice(Integer.parseInt(
                mPrefs.getString("pref_gallery_size_key", "1")));
        mGvs.requestFocus();

        String sortOrder = mPrefs.getString("pref_gallery_sort_key", null);
        if (sortOrder != null) {
            mSortAscending = sortOrder.equals("ascending");
        }

        mPausing = false;

        // install an intent filter to receive SD card related events.
        IntentFilter intentFilter =
                new IntentFilter(Intent.ACTION_MEDIA_MOUNTED);
        intentFilter.addAction(Intent.ACTION_MEDIA_UNMOUNTED);
        intentFilter.addAction(Intent.ACTION_MEDIA_SCANNER_STARTED);
        intentFilter.addAction(Intent.ACTION_MEDIA_SCANNER_FINISHED);
        intentFilter.addAction(Intent.ACTION_MEDIA_EJECT);
        intentFilter.addDataScheme("file");

        mReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                String action = intent.getAction();
                if (action.equals(Intent.ACTION_MEDIA_MOUNTED)) {
                    // SD card available
                    // TODO put up a "please wait" message
                    // TODO also listen for the media scanner finished message
                } else if (action.equals(Intent.ACTION_MEDIA_UNMOUNTED)) {
                    // SD card unavailable
                    rebake(true, false);
                } else if (action.equals(Intent.ACTION_MEDIA_SCANNER_STARTED)) {
                    rebake(false, true);
                } else if (action.equals(
                        Intent.ACTION_MEDIA_SCANNER_FINISHED)) {
                    rebake(false, false);
                } else if (action.equals(Intent.ACTION_MEDIA_EJECT)) {
                    rebake(true, false);
                }
            }
        };
        registerReceiver(mReceiver, intentFilter);
        rebake(false, ImageManager.isMediaScannerScanning(
                getContentResolver()));
    }

    // @Override
    // public boolean onCreateOptionsMenu(Menu menu) {
    //     if (isPickIntent()) {
    //         String type = getIntent().resolveType(this);
    //         if (type != null) {
    //             if (isImageType(type)) {
    //                 MenuHelper.addCapturePictureMenuItems(menu, this);
    //             } else if (isVideoType(type)) {
    //                 MenuHelper.addCaptureVideoMenuItems(menu, this);
    //             }
    //         }
    //     } else {
    //         MenuHelper.addCaptureMenuItems(menu, this);
    //         if ((mInclusion & ImageManager.INCLUDE_IMAGES) != 0) {
    //             mSlideShowItem = addSlideShowMenu(menu);
    //         }

    //         MenuItem item = menu.add(Menu.NONE, Menu.NONE,
    //                 MenuHelper.POSITION_GALLERY_SETTING,
    //                 R.string.camerasettings);
    //         item.setOnMenuItemClickListener(
    //                 new MenuItem.OnMenuItemClickListener() {
    //             public boolean onMenuItemClick(MenuItem item) {
    //                 Intent preferences = new Intent();
    //                 preferences.setClass(MultipleImageSelectionActivity.this,
    //                         GallerySettings.class);
    //                 startActivity(preferences);
    //                 return true;
    //             }
    //         });
    //         item.setAlphabeticShortcut('p');
    //         item.setIcon(android.R.drawable.ic_menu_preferences);

    //         item = menu.add(Menu.NONE, Menu.NONE,
    //                 MenuHelper.POSITION_MULTISELECT,
    //                 R.string.multiselect);
    //         item.setOnMenuItemClickListener(
    //                 new MenuItem.OnMenuItemClickListener() {
    //             public boolean onMenuItemClick(MenuItem item) {
    //                 if (isInMultiSelectMode()) {
    //                     closeMultiSelectMode();
    //                 } else {
    //                     openMultiSelectMode();
    //                 }
    //                 return true;
    //             }
    //         });
    //         item.setIcon(R.drawable.ic_menu_multiselect_gallery);
    //     }
    //     return true;
    // }

    // @Override
    // public boolean onPrepareOptionsMenu(Menu menu) {
    //     if (!canHandleEvent()) return false;
    //     if ((mInclusion & ImageManager.INCLUDE_IMAGES) != 0) {
    //         boolean videoSelected = isVideoSelected();
    //         // TODO: Only enable slide show if there is at least one image in
    //         // the folder.
    //         if (mSlideShowItem != null) {
    //             mSlideShowItem.setEnabled(!videoSelected);
    //         }
    //     }

    //     return true;
    // }

    // private boolean isVideoSelected() {
    //     IImage image = getCurrentImage();
    //     return (image != null) && ImageManager.isVideo(image);
    // }

    // According to the intent, setup what we include (image/video) in the
    // gallery and the title of the gallery.
    private void setupTitle() {
        mInclusion = ImageManager.INCLUDE_IMAGES | ImageManager.INCLUDE_VIDEOS;

        TextView leftText = (TextView) findViewById(R.id.left_text);
        leftText.setText(R.string.pick_photo_video_title);
    }

    // Returns the image list parameter which contains the subset of image/video
    // we want.
    private ImageManager.ImageListParam allImages(boolean storageAvailable) {
        if (!storageAvailable) {
            return ImageManager.getEmptyImageListParam();
        } else {
            return ImageManager.getImageListParam(
                    ImageManager.DataLocation.EXTERNAL,
                    mInclusion,
                    mSortAscending
                    ? ImageManager.SORT_ASCENDING
                    : ImageManager.SORT_DESCENDING,
                    ImageManager.CAMERA_IMAGE_BUCKET_ID);

        }
    }

    private void updateSelectionStatus() {
        int nSelected = mMultiSelected.size();
        String status;
        if (nSelected == 0) {
            status = getResources().getString(R.string.select_upload_items);
        } else {
            status = getResources().getQuantityString(R.plurals.n_upload_items_selected, nSelected, nSelected);
        }
        mSelectionStatus.setText(status);
    }

    private void toggleMultiSelected(IImage image) {
        if (!mMultiSelected.add(image)) {
            mMultiSelected.remove(image);
        }

        Log.d(TAG, "add/remove: " + image.getDataPath());

        updateSelectionStatus();
        mGvs.invalidate();
    }

    public void onImageClicked(int index) {
    }
    // public void onImageClicked(int index) {
    //     if (index < 0 || index >= mAllImages.getCount()) {
    //         return;
    //     }
    //     mSelectedIndex = index;
    //     mGvs.setSelectedIndex(index);

    //     IImage image = mAllImages.getImageAt(index);

    //     if (isInMultiSelectMode()) {
    //         toggleMultiSelected(image);
    //         return;
    //     }

    //     if (isPickIntent()) {
    //         launchCropperOrFinish(image);
    //     } else {
    //         Intent intent;
    //         if (image instanceof VideoObject) {
    //             intent = new Intent(
    //                     Intent.ACTION_VIEW, image.fullSizeImageUri());
    //             intent.putExtra(MediaStore.EXTRA_SCREEN_ORIENTATION,
    //                     ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
    //         } else {
    //             intent = new Intent(this, ViewImage.class);
    //             intent.putExtra(ViewImage.KEY_IMAGE_LIST, mParam);
    //             intent.setData(image.fullSizeImageUri());
    //         }
    //         startActivity(intent);
    //     }
    // }

    public void onImageTapped(int index) {
        // In the multiselect mode, once the finger finishes tapping, we hide
        // the selection box by setting the selected index to none. However, if
        // we use the dpad center key, we will keep the selected index in order
        // to show the the selection box. We do this because we have the
        // multiselect marker on the images to indicate which of them are
        // selected, so we don't need the selection box, but in the dpad case
        // we still need the selection box to show as a "cursor".

        mGvs.setSelectedIndex(GridViewSpecial.INDEX_NONE);
        toggleMultiSelected(mAllImages.getImageAt(index));
    }

    // private class CreateContextMenuListener implements
    //         View.OnCreateContextMenuListener {
    //     public void onCreateContextMenu(ContextMenu menu, View v,
    //             ContextMenu.ContextMenuInfo menuInfo) {
    //         if (!canHandleEvent()) return;

    //         IImage image = getCurrentImage();

    //         if (image == null) {
    //             return;
    //         }

    //         boolean isImage = ImageManager.isImage(image);
    //         if (isImage) {
    //             menu.add(R.string.view)
    //                     .setOnMenuItemClickListener(
    //                     new MenuItem.OnMenuItemClickListener() {
    //                         public boolean onMenuItemClick(MenuItem item) {
    //                             if (!canHandleEvent()) return false;
    //                             onImageClicked(mGvs.getCurrentSelection());
    //                             return true;
    //                         }
    //                     });
    //         }

    //         menu.setHeaderTitle(isImage
    //                 ? R.string.context_menu_header
    //                 : R.string.video_context_menu_header);
    //         if ((mInclusion & (ImageManager.INCLUDE_IMAGES
    //                 | ImageManager.INCLUDE_VIDEOS)) != 0) {
    //             MenuHelper.MenuItemsResult r = MenuHelper.addImageMenuItems(
    //                     menu,
    //                     MenuHelper.INCLUDE_ALL,
    //                     MultipleImageSelectionActivity.this,
    //                     mHandler,
    //                     mDeletePhotoRunnable,
    //                     new MenuHelper.MenuInvoker() {
    //                         public void run(MenuHelper.MenuCallback cb) {
    //                             if (!canHandleEvent()) {
    //                                 return;
    //                             }
    //                             cb.run(getCurrentImageUri(), getCurrentImage());
    //                             mGvs.invalidateImage(mGvs.getCurrentSelection());
    //                         }
    //                     });

    //             if (r != null) {
    //                 r.gettingReadyToOpen(menu, image);
    //             }

    //             if (isImage) {
    //                 MenuHelper.enableShowOnMapMenuItem(
    //                         menu, MenuHelper.hasLatLngData(image));
    //                 addSlideShowMenu(menu);
    //             }
    //         }
    //     }
    // }

    public void onLayoutComplete(boolean changed) {
        mLayoutComplete = true;
        if (mCropResultUri != null) {
            IImage image = mAllImages.getImageForUri(mCropResultUri);
            mCropResultUri = null;
            if (image != null) {
                mSelectedIndex = mAllImages.getImageIndex(image);
            }
        }
        mGvs.setSelectedIndex(mSelectedIndex);
        if (mScrollPosition == INVALID_POSITION) {
            if (mSortAscending) {
                mGvs.scrollTo(0, mGvs.getHeight());
            } else {
                mGvs.scrollToImage(0);
            }
        } else if (mConfigurationChanged) {
            mConfigurationChanged = false;
            mGvs.scrollTo(mScrollPosition);
            if (mGvs.getCurrentSelection() != GridViewSpecial.INDEX_NONE) {
                mGvs.scrollToVisible(mSelectedIndex);
            }
        } else {
            mGvs.scrollTo(mScrollPosition);
        }
    }

    public void onScroll(float scrollPosition) {
        mScrollPosition = scrollPosition;
    }

    private Drawable mVideoOverlay;
    private Drawable mVideoMmsErrorOverlay;
    private Drawable mMultiSelectTrue;
    private Drawable mMultiSelectFalse;

    // mSrcRect and mDstRect are only used in drawImage, but we put them as
    // instance variables to reduce the memory allocation overhead because
    // drawImage() is called a lot.
    private final Rect mSrcRect = new Rect();
    private final Rect mDstRect = new Rect();

    private final Paint mPaint = new Paint(Paint.FILTER_BITMAP_FLAG);

    public void drawImage(Canvas canvas, IImage image,
            Bitmap b, int xPos, int yPos, int w, int h) {
        if (b != null) {
            // if the image is close to the target size then crop,
            // otherwise scale both the bitmap and the view should be
            // square but I suppose that could change in the future.

            int bw = b.getWidth();
            int bh = b.getHeight();

            int deltaW = bw - w;
            int deltaH = bh - h;

            if (deltaW >= 0 && deltaW < 10 &&
                deltaH >= 0 && deltaH < 10) {
                int halfDeltaW = deltaW / 2;
                int halfDeltaH = deltaH / 2;
                mSrcRect.set(0 + halfDeltaW, 0 + halfDeltaH,
                        bw - halfDeltaW, bh - halfDeltaH);
                mDstRect.set(xPos, yPos, xPos + w, yPos + h);
                canvas.drawBitmap(b, mSrcRect, mDstRect, null);
            } else {
                mSrcRect.set(0, 0, bw, bh);
                mDstRect.set(xPos, yPos, xPos + w, yPos + h);
                canvas.drawBitmap(b, mSrcRect, mDstRect, mPaint);
            }
        } else {
            // If the thumbnail cannot be drawn, put up an error icon
            // instead
            Bitmap error = getErrorBitmap(image);
            int width = error.getWidth();
            int height = error.getHeight();
            mSrcRect.set(0, 0, width, height);
            int left = (w - width) / 2 + xPos;
            int top = (w - height) / 2 + yPos;
            mDstRect.set(left, top, left + width, top + height);
            canvas.drawBitmap(error, mSrcRect, mDstRect, null);
        }

        if (ImageManager.isVideo(image)) {
            Drawable overlay = null;
            long size = Util.getImageFileSize(image);
            if (size >= 0 && size <= mVideoSizeLimit) {
                if (mVideoOverlay == null) {
                    mVideoOverlay = getResources().getDrawable(
                            R.drawable.ic_gallery_video_overlay);
                }
                overlay = mVideoOverlay;
            } else {
                if (mVideoMmsErrorOverlay == null) {
                    mVideoMmsErrorOverlay = getResources().getDrawable(
                            R.drawable.ic_error_mms_video_overlay);
                }
                overlay = mVideoMmsErrorOverlay;
                Paint paint = new Paint();
                paint.setARGB(0x80, 0x00, 0x00, 0x00);
                canvas.drawRect(xPos, yPos, xPos + w, yPos + h, paint);
            }
            int width = overlay.getIntrinsicWidth();
            int height = overlay.getIntrinsicHeight();
            int left = (w - width) / 2 + xPos;
            int top = (h - height) / 2 + yPos;
            mSrcRect.set(left, top, left + width, top + height);
            overlay.setBounds(mSrcRect);
            overlay.draw(canvas);
        }
    }

    public void drawDecoration(Canvas canvas, IImage image,
            int xPos, int yPos, int w, int h) {

        initializeMultiSelectDrawables();

        if (mMultiSelected.contains(image)) {
            Drawable checkBox = mMultiSelectTrue;
            int width = checkBox.getIntrinsicWidth();
            int height = checkBox.getIntrinsicHeight();
            int left = 5 + xPos;
            int top = h - height - 5 + yPos;
            mSrcRect.set(left, top, left + width, top + height);
            checkBox.setBounds(mSrcRect);
            checkBox.draw(canvas);
        }
    }

    private void initializeMultiSelectDrawables() {
        if (mMultiSelectTrue == null) {
            mMultiSelectTrue = getResources()
                    .getDrawable(R.drawable.btn_check_buttonless_on);
        }
        if (mMultiSelectFalse == null) {
            mMultiSelectFalse = getResources()
                    .getDrawable(R.drawable.btn_check_buttonless_off);
        }
    }

    private Bitmap mMissingImageThumbnailBitmap;
    private Bitmap mMissingVideoThumbnailBitmap;

    // Create this bitmap lazily, and only once for all the ImageBlocks to
    // use
    public Bitmap getErrorBitmap(IImage image) {
        if (ImageManager.isImage(image)) {
            if (mMissingImageThumbnailBitmap == null) {
                mMissingImageThumbnailBitmap = BitmapFactory.decodeResource(
                        getResources(),
                        R.drawable.ic_missing_thumbnail_picture);
            }
            return mMissingImageThumbnailBitmap;
        } else {
            if (mMissingVideoThumbnailBitmap == null) {
                mMissingVideoThumbnailBitmap = BitmapFactory.decodeResource(
                        getResources(), R.drawable.ic_missing_thumbnail_video);
            }
            return mMissingVideoThumbnailBitmap;
        }
    }

    // private Animation mFooterAppear;
    // private Animation mFooterDisappear;

    // private void showFooter() {
    //     mFooterOrganizeView.setVisibility(View.VISIBLE);
    //     if (mFooterAppear == null) {
    //         mFooterAppear = AnimationUtils.loadAnimation(
    //                 this, R.anim.footer_appear);
    //     }
    //     mFooterOrganizeView.startAnimation(mFooterAppear);
    // }

    // private void hideFooter() {
    //     if (mFooterOrganizeView.getVisibility() != View.GONE) {
    //         mFooterOrganizeView.setVisibility(View.GONE);
    //         if (mFooterDisappear == null) {
    //             mFooterDisappear = AnimationUtils.loadAnimation(
    //                     this, R.anim.footer_disappear);
    //         }
    //         mFooterOrganizeView.startAnimation(mFooterDisappear);
    //     }
    // }

    // private String getShareMultipleMimeType() {
    //     final int FLAG_IMAGE = 1, FLAG_VIDEO = 2;
    //     int flag = 0;
    //     for (IImage image : mMultiSelected) {
    //         flag |= ImageManager.isImage(image) ? FLAG_IMAGE : FLAG_VIDEO;
    //     }
    //     return flag == FLAG_IMAGE
    //             ? "image/*"
    //             : flag == FLAG_VIDEO ? "video/*" : "*/*";
    // }

    // private void onShareMultipleClicked() {
    //     if (mMultiSelected == null) return;
    //     if (mMultiSelected.size() > 1) {
    //         Intent intent = new Intent();
    //         intent.setAction(Intent.ACTION_SEND_MULTIPLE);

    //         String mimeType = getShareMultipleMimeType();
    //         intent.setType(mimeType);
    //         ArrayList<Parcelable> list = Lists.newArrayList();
    //         for (IImage image : mMultiSelected) {
    //             list.add(image.fullSizeImageUri());
    //         }
    //         intent.putParcelableArrayListExtra(Intent.EXTRA_STREAM, list);
    //         try {
    //             startActivity(Intent.createChooser(
    //                     intent, getText(R.string.send_media_files)));
    //         } catch (android.content.ActivityNotFoundException ex) {
    //             Toast.makeText(this, R.string.no_way_to_share,
    //                     Toast.LENGTH_SHORT).show();
    //         }
    //     } else if (mMultiSelected.size() == 1) {
    //         IImage image = mMultiSelected.iterator().next();
    //         Intent intent = new Intent();
    //         intent.setAction(Intent.ACTION_SEND);
    //         String mimeType = image.getMimeType();
    //         intent.setType(mimeType);
    //         intent.putExtra(Intent.EXTRA_STREAM, image.fullSizeImageUri());
    //         boolean isImage = ImageManager.isImage(image);
    //         try {
    //             startActivity(Intent.createChooser(intent, getText(
    //                     isImage ? R.string.sendImage : R.string.sendVideo)));
    //         } catch (android.content.ActivityNotFoundException ex) {
    //             Toast.makeText(this, isImage
    //                     ? R.string.no_way_to_share_image
    //                     : R.string.no_way_to_share_video,
    //                     Toast.LENGTH_SHORT).show();
    //         }
    //     }
    // }

    // private void onDeleteMultipleClicked() {
    //     if (mMultiSelected == null) return;
    //     Runnable action = new Runnable() {
    //         public void run() {
    //             ArrayList<Uri> uriList = Lists.newArrayList();
    //             for (IImage image : mMultiSelected) {
    //                 uriList.add(image.fullSizeImageUri());
    //             }
    //             closeMultiSelectMode();
    //             Intent intent = new Intent(MultipleImageSelectionActivity.this,
    //                     DeleteImage.class);
    //             intent.putExtra("delete-uris", uriList);
    //             try {
    //                 startActivity(intent);
    //             } catch (ActivityNotFoundException ex) {
    //                 Log.e(TAG, "Delete images fail", ex);
    //             }
    //         }
    //     };
    //     MenuHelper.deleteMultiple(this, action);
    // }

    // private boolean isInMultiSelectMode() {
    //     return mMultiSelected != null;
    // }

    // private void closeMultiSelectMode() {
    //     if (mMultiSelected == null) return;
    //     mMultiSelected = null;
    //     mGvs.invalidate();
    //     hideFooter();
    // }
}