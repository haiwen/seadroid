package com.seafile.seadroid2.ui.camera_upload.config_fragment;

import android.os.Bundle;
import androidx.fragment.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import com.seafile.seadroid2.R;

/**
 * Welcome fragment for camera upload configuration helper
 */
public class ConfigWelcomeFragment extends Fragment {

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        return getActivity().getLayoutInflater().inflate(R.layout.cuc_welcome_fragment, null);
    }

}

