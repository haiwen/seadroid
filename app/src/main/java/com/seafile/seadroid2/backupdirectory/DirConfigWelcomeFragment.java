package com.seafile.seadroid2.backupdirectory;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.seafile.seadroid2.R;

/**
 * Welcome fragment for dir upload configuration helper
 */
public class DirConfigWelcomeFragment extends Fragment {

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        View rootView = getActivity().getLayoutInflater().inflate(R.layout.dir_welcome_fragment, null);

        return rootView;
    }

}

