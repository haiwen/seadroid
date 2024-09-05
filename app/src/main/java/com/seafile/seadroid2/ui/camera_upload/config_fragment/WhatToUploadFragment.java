package com.seafile.seadroid2.ui.camera_upload.config_fragment;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RadioGroup;

import androidx.fragment.app.Fragment;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;

/**
 * What to upload fragment for camera upload configuration helper
 */
public class WhatToUploadFragment extends Fragment {
    private RadioGroup mRadioGroup;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.cuc_what_to_upload_fragment, null);
        mRadioGroup = rootView.findViewById(R.id.cuc_upload_radio_group);
        if (AlbumBackupSharePreferenceHelper.readAllowVideoSwitch()) {
            mRadioGroup.check(R.id.cuc_upload_photos_and_videos_rb);
        }

        return rootView;
    }

    public boolean getWhatToUpload() {
        return mRadioGroup.getCheckedRadioButtonId() == R.id.cuc_upload_photos_and_videos_rb;
    }

}

