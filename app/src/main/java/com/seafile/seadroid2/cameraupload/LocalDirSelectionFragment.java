package com.seafile.seadroid2.cameraupload;

import android.annotation.SuppressLint;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import android.widget.AdapterView.OnItemClickListener;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
/**
 * Choose local directories
 */
public class LocalDirSelectionFragment extends Fragment {

    private RelativeLayout mUpLayout;
    private TextView mCurrentDirText;
    private ImageView mRefreshBtn;
    private ListView mListView;
    private List<String> mSelectedDirList;

    private String mRootDir;
    private String mCurrentDir;

    private List<String> mDirectoryNamesList;
    /** only contains partial data, like paths under sdcard0 (or sdcard1) */
    private List<String> mDirectoryPathsList;
    /** only contains partial data specifically for mDirectoryPathsList */
    private List<String> mDirectorySizesList;
    /** contains all selected data, the value of each always be true */
    private HashMap<String, Boolean> mDirectoriesMap;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        View rootView = getActivity().getLayoutInflater().inflate(R.layout.cuc_multi_selection_layout, null);
        mDirectoriesMap = new HashMap<String, Boolean>();
        mRefreshBtn = (ImageView) rootView.findViewById(R.id.cuc_multi_selection_refresh_iv);
        mRefreshBtn.setVisibility(View.INVISIBLE);
        mListView = (ListView) rootView.findViewById(R.id.cuc_multi_selection_lv);
        mListView.setFastScrollEnabled(true);
        mUpLayout = (RelativeLayout) rootView.findViewById(R.id.cuc_multi_selection_up_layout);
        mCurrentDirText = (TextView) rootView.findViewById(R.id.cuc_multi_selection_current_directory_txt);
        mUpLayout.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                try {
                    getDir(new File(mCurrentDir).getParentFile().getCanonicalPath());
                } catch (Exception e) {
                    e.printStackTrace();
                }

            }

        });

        mRootDir = Environment.getExternalStorageDirectory().getAbsolutePath().toString();
        mCurrentDir = mRootDir;

        // Get a list of all the selected directory paths (will be empty if this is the first run).
        mSelectedDirList = CameraUploadDBHelper.getInstance().getCustomDirList();

        // Get a list of all the directory paths that are currently stored in the DB.
        for (int i = 0; i < mSelectedDirList.size(); i++) {

            // Filter out any double slashes.
            String path = mSelectedDirList.get(i);
            if (path.contains("//")) {
                path.replace("//", "/");
            }

            mDirectoriesMap.put(path, true);
        }

        //Get the folder hierarchy of the selected directory.
        getDir(mRootDir);

        mListView.setOnItemClickListener(new OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> arg0, View arg1, int index, long arg3) {
                String newPath = mDirectoryPathsList.get(index);
                getDir(newPath);

            }

        });

        return rootView;
    }

    /**
     * Sets the current directory's text.
     */
    private void setCurrentDirText() {
        mCurrentDirText.setText(mCurrentDir);
    }

    /**
     * Retrieves the folder hierarchy for the specified folder
     * (this method is NOT recursive and doesn't go into the parent
     * folder's subfolders.
     */
    private void getDir(String dirPath) {

        mDirectoryNamesList = Lists.newArrayList();
        mDirectoryPathsList = Lists.newArrayList();
        mDirectorySizesList = Lists.newArrayList();

        File f = new File(dirPath);
        File[] files = f.listFiles();
        Arrays.sort(files);

        if (files != null) {
            for (int i = 0; i < files.length; i++) {
                File file = files[i];
                if (!file.isHidden() && file.canRead()) {
                    if (file.isDirectory()) {
                        /* Starting with Android 4.2, /storage/emulated/legacy/...
                         * is a symlink that points to the actual directory where
                         * the user's files are stored. We need to detect the
                         * actual directory's file path here.
                         **/
                        String filePath;
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1)
                            filePath = getRealFilePath(file.getAbsolutePath());
                        else
                            filePath = file.getAbsolutePath();

                        mDirectoryPathsList.add(filePath);
                        mDirectoryNamesList.add(file.getName());

                        File[] listOfFiles = file.listFiles();

                        if (listOfFiles != null) {
                            mDirectorySizesList.add(getResources().getQuantityString(R.plurals.settings_cuc_local_dir_items,
                                    listOfFiles.length,
                                    listOfFiles.length));
                        }
                    }
                }
            }
        }

        boolean dirChecked = false;
        if (getLocalDirHashMap().get(dirPath) != null)
            dirChecked = getLocalDirHashMap().get(dirPath);

        LocalDirSelectionAdapter adapter = new LocalDirSelectionAdapter(getActivity(),
                this,
                dirChecked);

        mListView.setAdapter(adapter);
        adapter.notifyDataSetChanged();

        mCurrentDir = dirPath;
        setCurrentDirText();

    }

    /**
     * Resolves the /storage/emulated/legacy paths to
     * their true folder path representations. Required
     * for Nexuses and other devices with no SD card.
     */
    @SuppressLint("SdCardPath")
    private String getRealFilePath(String filePath) {

        if (filePath.equals("/storage/emulated/0") ||
                filePath.equals("/storage/emulated/0/") ||
                filePath.equals("/storage/emulated/legacy") ||
                filePath.equals("/storage/emulated/legacy/") ||
                filePath.equals("/storage/sdcard0") ||
                filePath.equals("/storage/sdcard0/") ||
                filePath.equals("/sdcard") ||
                filePath.equals("/sdcard/") ||
                filePath.equals("/mnt/sdcard") ||
                filePath.equals("/mnt/sdcard/")) {

            return Environment.getExternalStorageDirectory().toString();
        }

        return filePath;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        getActivity().finish();
    }

    @Override
    public void onPause() {
        super.onPause();
        getActivity().finish();
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();

        if (isRemoving()) {
            mSelectedDirList = null;
        }

    }

    public HashMap<String, Boolean> getLocalDirHashMap() {
        return mDirectoriesMap;
    }

    public List<String> getDirNamesList() {
        return mDirectoryNamesList;
    }

    public List<String> getDirSizesList() {
        return mDirectorySizesList;
    }

    public List<String> getDirList() {
        return mDirectoryPathsList;
    }

}

