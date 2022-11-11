package com.seafile.seadroid2.backupdirectory;

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
 * How to upload fragment
 */
public class DirHowToUploadFragment extends Fragment {

    private RadioButton mDataPlanRadioBtn;
    private RadioGroup mRadioGroup;

    private DirectoryUploadConfigActivity mActivity;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        mActivity = (DirectoryUploadConfigActivity) getActivity();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.dir_how_to_upload_fragment, null);

        mRadioGroup = (RadioGroup) rootView.findViewById(R.id.cuc_wifi_radio_group);
        mDataPlanRadioBtn = (RadioButton) rootView.findViewById(R.id.cuc_wifi_or_data_plan_rb);

        if (SettingsManager.instance().isDataPlanAllowed()) {
            mDataPlanRadioBtn.setChecked(true);
        }

        mRadioGroup.setOnCheckedChangeListener(new OnCheckedChangeListener() {

            @Override
            public void onCheckedChanged(RadioGroup group, int checkedId) {
                switch (checkedId) {
                    case R.id.cuc_wifi_only_rb:
                        // WiFi only
                        mActivity.saveDirDataPlanAllowed(false);
                        break;
                    case R.id.cuc_wifi_or_data_plan_rb:
                        // WiFi and data plan
                        mActivity.saveDirDataPlanAllowed(true);
                        break;
                }

            }

        });

        return rootView;
    }

}

