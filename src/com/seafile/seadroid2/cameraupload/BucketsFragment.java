package com.seafile.seadroid2.cameraupload;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.TranslateAnimation;
import android.widget.Button;
import android.widget.RadioGroup;
import android.widget.RadioGroup.OnCheckedChangeListener;
import android.widget.RelativeLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;

/**
 * Buckets fragment
 */
public class BucketsFragment extends Fragment {

    private CameraUploadConfigActivity mActivity;
    private FragmentManager fm;
    private BucketsSelectionFragment mSelectionFragment;
    private RadioGroup mRadioGroup;
    private Button mDoneBtn;
    private TranslateAnimation mSlideInAnimation;
    private TranslateAnimation mSlideOutAnimation;
    private RelativeLayout mDirectoriesLayout;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        mActivity = (CameraUploadConfigActivity) getActivity();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.cuc_local_directory_fragment, null);
        mDirectoriesLayout = (RelativeLayout) rootView.findViewById(R.id.cuc_local_directory_list_container);

        mSlideInAnimation = new TranslateAnimation(Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 2.0f,
                Animation.RELATIVE_TO_SELF, 0.0f);

        mSlideInAnimation.setDuration(600);
        mSlideInAnimation.setInterpolator(new AccelerateDecelerateInterpolator());
        mSlideInAnimation.setAnimationListener(slideInListener);

        mSlideOutAnimation = new TranslateAnimation(Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 2.0f);
        mSlideOutAnimation.setDuration(600);
        mSlideOutAnimation.setInterpolator(new AccelerateDecelerateInterpolator());
        mSlideOutAnimation.setAnimationListener(slideOutListener);

        fm = getChildFragmentManager();
        fm.beginTransaction()
                .add(R.id.cuc_local_directory_list_container, getSelectionFragment())
                .commit();

        mRadioGroup = (RadioGroup) rootView.findViewById(R.id.cuc_local_directory_radio_group);
        mDoneBtn = (Button) rootView.findViewById(R.id.cuc_local_directory_btn);
        mDoneBtn.setOnClickListener(onClickListener);
        if (mActivity.isChooseDirPage())
            mDoneBtn.setVisibility(View.VISIBLE);
        // RadioButton mAutoScanRadioBtn = (RadioButton) mRadioGroup.findViewById(R.id.cuc_local_library_auto_scan_rb);
        // RadioButton mCustomPickRadioBtn = (RadioButton) mRadioGroup.findViewById(R.id.cuc_local_library_pick_folders_rb);

        SettingsManager settingsManager = SettingsManager.instance();

        if (settingsManager.getCameraUploadBucketList().isEmpty()) {
            // auto scan
            mDirectoriesLayout.setVisibility(View.INVISIBLE);
            mDirectoriesLayout.setEnabled(false);
            mRadioGroup.check(R.id.cuc_local_directory_auto_scan_rb);
        } else {
            // pick custom folders to scan
            mDirectoriesLayout.setVisibility(View.VISIBLE);
            mDirectoriesLayout.setEnabled(true);
            mRadioGroup.check(R.id.cuc_local_directory_pick_folders_rb);
        }

        mRadioGroup.setOnCheckedChangeListener(onCheckedChangeListener);

        return rootView;
    }

    /**
     * RadioButton selection listener.
     */
    private OnCheckedChangeListener onCheckedChangeListener = new OnCheckedChangeListener() {

        @Override
        public void onCheckedChanged(RadioGroup radioGroup, int radioButtonId) {
            switch (radioButtonId) {
                case R.id.cuc_local_directory_auto_scan_rb:
                    mActivity.saveSettings();
                    mDirectoriesLayout.startAnimation(mSlideOutAnimation);
                    mDirectoriesLayout.setEnabled(false);
                    mActivity.saveSettings();
                    break;
                case R.id.cuc_local_directory_pick_folders_rb:
                    mDirectoriesLayout.startAnimation(mSlideInAnimation);
                    mDirectoriesLayout.setEnabled(true);
                    mActivity.saveSettings();
                    break;
            }

        }

    };

    public boolean isAutoScanSelected() {
        return mRadioGroup.getCheckedRadioButtonId() == R.id.cuc_local_directory_auto_scan_rb;
    }

    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            mActivity.saveSettings();
            mActivity.finish();
        }
    };

    /**
     * Slide out animation listener.
     */
    private AnimationListener slideOutListener = new AnimationListener() {

        @Override
        public void onAnimationEnd(Animation arg0) {
            mDirectoriesLayout.setVisibility(View.INVISIBLE);
        }

        @Override
        public void onAnimationRepeat(Animation arg0) {}

        @Override
        public void onAnimationStart(Animation arg0) {
            mDirectoriesLayout.setVisibility(View.VISIBLE);
        }

    };

    /**
     * Slide in animation listener.
     */
    private AnimationListener slideInListener = new AnimationListener() {

        @Override
        public void onAnimationEnd(Animation arg0) {
            mDirectoriesLayout.setVisibility(View.VISIBLE);
        }

        @Override
        public void onAnimationRepeat(Animation arg0) {}

        @Override
        public void onAnimationStart(Animation arg0) {
            mDirectoriesLayout.setVisibility(View.VISIBLE);
        }

    };

    /**
     * Instantiates a new fragment if mSelectionFragment is null.
     * Returns the current fragment, otherwise.
     */
    public BucketsSelectionFragment getSelectionFragment() {
        if (mSelectionFragment == null) {
            mSelectionFragment = new BucketsSelectionFragment();
        }

        return mSelectionFragment;
    }

}
