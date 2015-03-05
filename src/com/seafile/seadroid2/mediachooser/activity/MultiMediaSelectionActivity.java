package com.seafile.seadroid2.mediachooser.activity;

import android.app.AlertDialog;
import android.content.Intent;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.provider.MediaStore;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v4.view.ViewPager;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.*;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.mediachooser.MediaChooser;
import com.seafile.seadroid2.mediachooser.MediaChooserConstants;
import com.seafile.seadroid2.mediachooser.fragment.MultiImageSelectionFragment;
import com.seafile.seadroid2.mediachooser.fragment.VideoFragment;
import com.viewpagerindicator.TabPageIndicator;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;


public class MultiMediaSelectionActivity extends SherlockFragmentActivity implements MultiImageSelectionFragment.OnImageSelectedListener,
        VideoFragment.OnVideoSelectedListener {

    private ChooserTabsAdapter mAdapter;
    private ViewPager mPager;
    private TabPageIndicator mIndicator;
    private int mCurrentTab = 0;

    private TextView mSelectionStatus;

    private Button mCancelBtn;
    private Button mUploadBtn;

    private static Uri fileUri;

    private final Handler handler = new Handler();

    private int mImageSelectedCounts;
    private int mVideoSelectedCounts;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.media_chooser_activity_home);

        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        mCancelBtn = (Button) findViewById(R.id.button_cancel_upload);
        mUploadBtn = (Button) findViewById(R.id.button_confirm_upload);

        mSelectionStatus = (TextView) findViewById(R.id.upload_selection_status);

        mAdapter = new ChooserTabsAdapter(getSupportFragmentManager());
        mPager = (ViewPager) findViewById(R.id.pager);
        mPager.setAdapter(mAdapter);

        mIndicator = (TabPageIndicator)findViewById(R.id.indicator);
        mIndicator.setViewPager(mPager);
        mIndicator.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageSelected(final int position) {
                mCurrentTab = position;
                supportInvalidateOptionsMenu();
                int total = mImageSelectedCounts + mVideoSelectedCounts;
                if (total == 0)
                    mSelectionStatus.setText(getResources().getString(R.string.select_upload_items));
                else
                    mSelectionStatus.setText(getResources().getQuantityString(R.plurals.n_upload_items_selected, total));

                if (position == 0)
                    ((VideoFragment) mAdapter.getItem(1)).finishActionModeIfOn();
                else
                    ((MultiImageSelectionFragment) mAdapter.getItem(0)).finishActionModeIfOn();
            }

            @Override
            public void onPageScrollStateChanged(int arg0) {}

            @Override
            public void onPageScrolled(int arg0, float arg1, int arg2) {}
        });

        applyCustomFont();

        mCancelBtn.setOnClickListener(clickListener);
        mUploadBtn.setOnClickListener(clickListener);
    }

    private void applyCustomFont() {
        ViewGroup vg = (ViewGroup) mIndicator.getChildAt(0);
        int vgChildCount = vg.getChildCount();
        for (int j = 0; j < vgChildCount; j++) {
            View vgChild = vg.getChildAt(j);
            if (vgChild instanceof TextView) {
                Typeface face = Typeface.createFromAsset(getAssets(), SeadroidApplication.CUSTOM_FONT_ROBOTO_PATH);
                ((TextView) vgChild).setTypeface(face);
            }
        }

    }

    private void updateSelectionStatus(int count) {
        String status;

        String dynamicTitle;
        if (mCurrentTab == 0) {
            if (count == 0) {
                dynamicTitle = getResources().getString(R.string.media_chooser_images_tab);
            } else {
                dynamicTitle = String.format(getResources().getString(R.string.media_chooser_images_selected), count);
            }
            mImageSelectedCounts = count;

            if (count + mVideoSelectedCounts == 0)
                status = getResources().getString(R.string.select_upload_items);
            else
                status = getResources().getQuantityString(R.plurals.n_upload_items_selected, count + mVideoSelectedCounts, count + mVideoSelectedCounts);

            mAdapter.setImageTabTitle(dynamicTitle);
        } else {
            if (count == 0) {
                dynamicTitle = getResources().getString(R.string.media_chooser_videos_tab);
            } else {
                dynamicTitle = String.format(getResources().getString(R.string.media_chooser_videos_selected), count);
            }
            mVideoSelectedCounts = count;

            if (count + mImageSelectedCounts == 0)
                status = getResources().getString(R.string.select_upload_items);
            else
                status = getResources().getQuantityString(R.plurals.n_upload_items_selected, count + mImageSelectedCounts, count + mImageSelectedCounts);

            mAdapter.setVideoTabTitle(dynamicTitle);
        }

        mSelectionStatus.setText(status);
        mIndicator.notifyDataSetChanged();
        applyCustomFont();
    }

    class ChooserTabsAdapter extends FragmentPagerAdapter {

        private MultiImageSelectionFragment imageFragment;
        private VideoFragment videoFragment;

        private String imageTabTitle;
        private String videoTabTitle;

        public void setImageTabTitle(String imageTabTitle) {
            this.imageTabTitle = imageTabTitle;
        }

        public void setVideoTabTitle(String videoTabTitle) {
            this.videoTabTitle = videoTabTitle;
        }

        public ChooserTabsAdapter(FragmentManager fm) {
            super(fm);
            imageTabTitle = getString(R.string.media_chooser_images_tab);
            videoTabTitle = getString(R.string.media_chooser_videos_tab);
        }

        @Override
        public Fragment getItem(int position) {
            switch (position) {
                case 0:

                    if (imageFragment == null) {
                        imageFragment = new MultiImageSelectionFragment();
                    }
                    return imageFragment;
                case 1:
                    if (videoFragment == null) {
                        videoFragment = new VideoFragment();
                    }
                    return videoFragment;
                default:
                    return new Fragment();
            }
        }

        @Override
        public CharSequence getPageTitle(int position) {
            switch (position) {
                case 0:
                    return imageTabTitle;
                case 1:
                    return videoTabTitle;
                default:
                    return null;
            }
        }

        @Override
        public int getCount() {
            return 2;
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.media_capture_menu, menu);
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        MenuItem menuVideo = menu.findItem(R.id.video_capture);
        MenuItem menuPhoto = menu.findItem(R.id.photo_capture);

        if (mCurrentTab == 0) {
            menuPhoto.setVisible(true);
            menuVideo.setVisible(false);
        } else {
            menuPhoto.setVisible(false);
            menuVideo.setVisible(true);
        }
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.video_capture:
                Intent vIntent = new Intent(MediaStore.ACTION_VIDEO_CAPTURE);
                fileUri = getOutputMediaFileUri(MediaChooserConstants.MEDIA_TYPE_VIDEO); // create a file to save the image
                vIntent.putExtra(MediaStore.EXTRA_OUTPUT, fileUri); // set the image file name

                // start the image capture Intent
                startActivityForResult(vIntent, MediaChooserConstants.CAPTURE_VIDEO_ACTIVITY_REQUEST_CODE);
                break;
            case R.id.photo_capture:

                Intent iIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                fileUri = getOutputMediaFileUri(MediaChooserConstants.MEDIA_TYPE_IMAGE); // create a file to save the image
                iIntent.putExtra(MediaStore.EXTRA_OUTPUT, fileUri); // set the image file name

                // start the image capture Intent
                startActivityForResult(iIntent, MediaChooserConstants.CAPTURE_IMAGE_ACTIVITY_REQUEST_CODE);
                break;

        }
        return super.onOptionsItemSelected(item);
    }

    OnClickListener clickListener = new OnClickListener() {

        @Override
        public void onClick(View view) {
            if (view == mUploadBtn) {

                MultiImageSelectionFragment imageFragment = (MultiImageSelectionFragment) mAdapter.getItem(0);
                VideoFragment videoFragment = (VideoFragment) mAdapter.getItem(1);

                if (videoFragment != null || imageFragment != null) {

                    if (videoFragment != null) {
                        if (videoFragment.getSelectedVideoList() != null && videoFragment.getSelectedVideoList().size() > 0) {
                            Intent videoIntent = new Intent();
                            videoIntent.setAction(MediaChooser.VIDEO_SELECTED_ACTION_FROM_MEDIA_CHOOSER);
                            videoIntent.putStringArrayListExtra(MediaChooserConstants.MEDIA_SELECTED_LIST, videoFragment.getSelectedVideoList());
                            LocalBroadcastManager.getInstance(MultiMediaSelectionActivity.this).sendBroadcast(videoIntent);
                        }
                    }

                    if (imageFragment != null) {
                        if (imageFragment.getSelectedImageList() != null && imageFragment.getSelectedImageList().size() > 0) {
                            Intent imageIntent = new Intent();
                            imageIntent.setAction(MediaChooser.IMAGE_SELECTED_ACTION_FROM_MEDIA_CHOOSER);
                            imageIntent.putStringArrayListExtra(MediaChooserConstants.MEDIA_SELECTED_LIST, imageFragment.getSelectedImageList());
                            LocalBroadcastManager.getInstance(MultiMediaSelectionActivity.this).sendBroadcast(imageIntent);
                        }
                    }

                    finish();
                } else {
                    Toast.makeText(MultiMediaSelectionActivity.this, getString(R.string.media_chooser_plaese_select_file), Toast.LENGTH_SHORT).show();
                }

            } else if (view == mCancelBtn) {
                finish();
            }
        }
    };

    /**
     * Create a file Uri for saving an image or video
     */
    private Uri getOutputMediaFileUri(int type) {
        return Uri.fromFile(getOutputMediaFile(type));
    }

    /**
     * Create a File for saving an image or video
     */
    private static File getOutputMediaFile(int type) {

        File mediaStorageDir = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), MediaChooserConstants.folderName);
        if (!mediaStorageDir.exists()) {
            if (!mediaStorageDir.mkdirs()) {
                return null;
            }
        }

        // Create a media file name
        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date());
        File mediaFile;
        if (type == MediaChooserConstants.MEDIA_TYPE_IMAGE) {
            mediaFile = new File(mediaStorageDir.getPath() + File.separator + "IMG_" + timeStamp + ".jpg");
        } else if (type == MediaChooserConstants.MEDIA_TYPE_VIDEO) {
            mediaFile = new File(mediaStorageDir.getPath() + File.separator + "VID_" + timeStamp + ".mp4");
        } else {
            return null;
        }

        return mediaFile;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {

        if (requestCode == MediaChooserConstants.CAPTURE_VIDEO_ACTIVITY_REQUEST_CODE) {
            if (resultCode == RESULT_OK) {

                sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, fileUri));
                final AlertDialog alertDialog = MediaChooserConstants.getDialog(MultiMediaSelectionActivity.this).create();
                alertDialog.show();
                handler.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        //Do something after 5000ms
                        String fileUriString = fileUri.toString().replaceFirst("file:///", "/").trim();
                        VideoFragment videoFragment = (VideoFragment) mAdapter.getItem(1);
                        //
                        if (videoFragment == null) {
                            VideoFragment newVideoFragment = new VideoFragment();
                            newVideoFragment.addItem(fileUriString);

                        } else {
                            videoFragment.addItem(fileUriString);
                        }
                        alertDialog.cancel();
                    }
                }, 5000);


            } else if (resultCode == RESULT_CANCELED) {
                // User cancelled the video capture
            } else {
                // Video capture failed, advise user
            }
        } else if (requestCode == MediaChooserConstants.CAPTURE_IMAGE_ACTIVITY_REQUEST_CODE) {
            if (resultCode == RESULT_OK) {

                sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, fileUri));

                final AlertDialog alertDialog = MediaChooserConstants.getDialog(MultiMediaSelectionActivity.this).create();
                alertDialog.show();
                handler.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        //Do something after 5000ms
                        String fileUriString = fileUri.toString().replaceFirst("file:///", "/").trim();
                        MultiImageSelectionFragment imageFragment = (MultiImageSelectionFragment) mAdapter.getItem(0);
                        if (imageFragment == null) {
                            MultiImageSelectionFragment newImageFragment = new MultiImageSelectionFragment();
                            newImageFragment.addItem(fileUriString);

                        } else {
                            imageFragment.addItem(fileUriString);
                        }
                        alertDialog.cancel();
                    }
                }, 5000);
            }
        }
    }

    @Override
    public void onImageSelected(int count) {
        updateSelectionStatus(count);
    }


    @Override
    public void onVideoSelected(int count) {
        updateSelectionStatus(count);
    }

    /*public int convertDipToPixels(float dips) {
        return (int) (dips * HomeFragmentActivity.this.getResources().getDisplayMetrics().density + 0.5f);
    }*/

}
