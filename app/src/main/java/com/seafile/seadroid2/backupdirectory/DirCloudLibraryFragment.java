package com.seafile.seadroid2.backupdirectory;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;

import com.seafile.seadroid2.R;

public class DirCloudLibraryFragment extends Fragment {

    private DirectoryUploadConfigActivity mActivity;
    private FragmentManager fm;
    private Button mDoneBtn;
    private DirCloudLibrarySelectionFragment mSelectionFragment;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        mActivity = (DirectoryUploadConfigActivity) getActivity();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.cuc_remote_library_fragment, null);

        fm = getChildFragmentManager();
        fm.beginTransaction()
                .add(R.id.cuc_remote_library_list_container, getAccountOrReposSelectionFragment())
                .commit();

        mDoneBtn = (Button) rootView.findViewById(R.id.cuc_remote_library_btn);
        mDoneBtn.setOnClickListener(onClickListener);

        return rootView;
    }

    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            mActivity.saveSettings();
            mActivity.finish();
        }
    };
    public DirCloudLibrarySelectionFragment getAccountOrReposSelectionFragment() {
        if (mSelectionFragment == null) {
            mSelectionFragment = new DirCloudLibrarySelectionFragment();
        }

        return mSelectionFragment;
    }

}

