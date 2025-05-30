package com.seafile.seadroid2.ui.camera_upload.config_fragment;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;

/**
 * How to upload fragment
 */
public class HowToUploadFragment extends Fragment {

    private RadioButton mDataPlanRadioBtn;
    private RadioGroup mRadioGroup;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        return inflater.inflate(R.layout.cuc_how_to_upload_fragment, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        mRadioGroup = view.findViewById(R.id.cuc_wifi_radio_group);
        mDataPlanRadioBtn = view.findViewById(R.id.cuc_wifi_or_data_plan_rb);

        if (AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch()) {
            mDataPlanRadioBtn.setChecked(true);
        }
    }

    public boolean getHowToUpload() {
        return mRadioGroup.getCheckedRadioButtonId() == R.id.cuc_wifi_or_data_plan_rb;
    }

}

