package com.seafile.seadroid2.cameraupload;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.RadioGroup.OnCheckedChangeListener;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;

/**
 * What to upload fragment for camera upload configuration helper
 */
public class WhatToUploadFragment extends Fragment {

    private CameraUploadConfigActivity mActivity;
    private RadioGroup mRadioGroup;
    private RadioButton mVideoUploadRadioBtn;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        mActivity = (CameraUploadConfigActivity) getActivity();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.cuc_what_to_upload_fragment, null);
        mRadioGroup = (RadioGroup) rootView.findViewById(R.id.cuc_upload_radio_group);
        mVideoUploadRadioBtn = (RadioButton) rootView.findViewById(R.id.cuc_upload_photos_and_videos_rb);
        if (SettingsManager.instance().isVideosUploadAllowed()) {
            mRadioGroup.check(R.id.cuc_upload_photos_and_videos_rb);
            //mVideoUploadRadioBtn.setChecked(true);
        }

        mRadioGroup.setOnCheckedChangeListener(new OnCheckedChangeListener() {

            @Override
            public void onCheckedChanged(RadioGroup group, int checkedId) {
                switch (checkedId) {
                    case R.id.cuc_upload_photos_rb:
                        // only upload photos
                        mActivity.saveVideosAllowed(false);
                        break;
                    case R.id.cuc_upload_photos_and_videos_rb:
                        // upload photos and videos
                        mActivity.saveVideosAllowed(true);
                        break;
                }

            }

        });

        return rootView;
    }

}

