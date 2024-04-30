package com.seafile.seadroid2.ui.camera_upload.config_fragment;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;

/**
 * How to upload fragment
 */
public class HowToUploadFragment extends Fragment {

    private RadioButton mDataPlanRadioBtn;
    private RadioGroup mRadioGroup;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View rootView = LayoutInflater.from(getContext()).inflate(R.layout.cuc_how_to_upload_fragment, null);

        mRadioGroup = rootView.findViewById(R.id.cuc_wifi_radio_group);
        mDataPlanRadioBtn = rootView.findViewById(R.id.cuc_wifi_or_data_plan_rb);

        if (AlbumBackupManager.readAllowDataPlanSwitch()) {
            mDataPlanRadioBtn.setChecked(true);
        }

        return rootView;
    }

    public boolean getHowToUpload() {
        return mRadioGroup.getCheckedRadioButtonId() == R.id.cuc_wifi_or_data_plan_rb;
    }

}

