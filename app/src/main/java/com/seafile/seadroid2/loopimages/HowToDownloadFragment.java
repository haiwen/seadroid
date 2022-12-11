package com.seafile.seadroid2.loopimages;

import android.content.Intent;
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
public class HowToDownloadFragment extends Fragment {

    private RadioButton mDataPlanRadioBtn;
    private RadioGroup mRadioGroup;

    private LoopImagesWidgetConfigureActivity mActivity;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mActivity = (LoopImagesWidgetConfigureActivity) getActivity();

        View rootView = mActivity.getLayoutInflater().inflate(R.layout.loop_images_how_to_download_fragment, null);

        mRadioGroup = (RadioGroup) rootView.findViewById(R.id.loopimages_wifi_radio_group);
        mDataPlanRadioBtn = (RadioButton) rootView.findViewById(R.id.loopimages_wifi_or_data_plan_rb);

        if (mActivity.getDataPlanAllowed(mActivity.getAppWidgetId())) {
            mDataPlanRadioBtn.setChecked(true);
        }

        mRadioGroup.setOnCheckedChangeListener(new OnCheckedChangeListener() {

            @Override
            public void onCheckedChanged(RadioGroup group, int checkedId) {
                switch (checkedId) {
                    case R.id.loopimages_wifi_only_rb:
                        // WiFi only
                        mActivity.saveDataPlanAllowed(false);
                        break;
                    case R.id.loopimages_wifi_or_data_plan_rb:
                        // WiFi and data plan
                        mActivity.saveDataPlanAllowed(true);
                        break;
                }

            }

        });

        return rootView;
    }

}

