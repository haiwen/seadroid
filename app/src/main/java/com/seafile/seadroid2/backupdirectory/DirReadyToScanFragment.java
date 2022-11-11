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
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.util.Utils;

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
                FileBean fileBean = mActivity.getFileBean();
                if (fileBean == null) {
                    Toast.makeText(getActivity(), getActivity().getString(R.string.select_backup_folder), Toast.LENGTH_SHORT).show();
                    return;
                }
                if (mActivity.getSeafRepo() == null) {
                    Toast.makeText(getActivity(), getActivity().getString(R.string.select_backup_libraries), Toast.LENGTH_SHORT).show();
                    return;
                }
                Account account = mActivity.getAccount();
                UploadDirectoryDBHelper databaseHelper = mActivity.getDatabaseHelper();
                UploadDirConfig dirConfig = databaseHelper.getDirConfig(account, fileBean.getFilePath());
                SeafRepo repo = mActivity.getSeafRepo();
                if (dirConfig != null && dirConfig.filePath.equals(fileBean.getFilePath())) {
                    Toast.makeText(getActivity(), getActivity().getString(R.string.folder_backup_completed), Toast.LENGTH_SHORT).show();
                    if (!mActivity.getSeafRepo().id.equals(dirConfig.repoID)) {
                        //the repo different and the backup path is the same. Updating the database
                        Utils.utilsLogInfo(false, "-------old---"+dirConfig.repoName+"====new===="+repo.name);
                        databaseHelper.updateDirConfig(account, dirConfig.repoID, repo.id, repo.name, fileBean.getFileName(), fileBean.getFilePath());
                    }
                } else {
                    databaseHelper.saveDirUploadConfig(account, repo.id, repo.name, fileBean.getFileName(), fileBean.getFilePath());
                }
                SettingsManager.instance().saveDirectoryFilePath(fileBean.getFilePath());
                SettingsManager.instance().saveDirAutomaticUpload(true);
                mActivity.saveSettings();
                mActivity.finish();
            }
        });

        return rootView;
    }

}

