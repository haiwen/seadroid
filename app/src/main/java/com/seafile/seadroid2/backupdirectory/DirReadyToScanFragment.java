package com.seafile.seadroid2.backupdirectory;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;

import java.util.List;

public class DirReadyToScanFragment extends Fragment {

    private Button continueBtn;
    private DirectoryUploadConfigActivity mActivity;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        mActivity = (DirectoryUploadConfigActivity) getActivity();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.cuc_ready_to_scan_fragment, null);
        continueBtn = (Button) rootView.findViewById(R.id.cuc_click_to_finish_btn);
        continueBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                List<String> selectFilePath = mActivity.getSelectFilePath();
                if (selectFilePath == null || selectFilePath.size() == 0) {
                    Toast.makeText(getActivity(), getActivity().getString(R.string.select_backup_folder), Toast.LENGTH_SHORT).show();
                    return;
                }
                if (mActivity.getSeafRepo() == null) {
                    Toast.makeText(getActivity(), getActivity().getString(R.string.select_backup_libraries), Toast.LENGTH_SHORT).show();
                    return;
                }
                SettingsManager.instance().saveDirAutomaticUpload(true);
                mActivity.saveSettings();
                mActivity.finish();
            }
        });

        return rootView;
    }

}

