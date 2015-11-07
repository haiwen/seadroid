package com.seafile.seadroid2.cache;

import android.annotation.SuppressLint;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;

import java.io.File;
import java.util.Arrays;
import java.util.List;

/**
 * Fragment for selecting a default cache dir
 */
public class CacheDirSelectionFragment extends Fragment {

    private RelativeLayout mUpLayout;
    private TextView mCurrentDirText;
    private ImageView mRefreshBtn;
    private ListView mListView;
    private String mRootDir;
    private String mCurrentDir;
    private List<String> mDirectoryNamesList;
    private List<String> mDirectoryPathsList;
    private List<String> mDirectorySizesList;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        View rootView = getActivity().getLayoutInflater().inflate(R.layout.cuc_multi_selection_layout, null);
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

        //Get the folder hierarchy of the selected directory.
        getDir(mRootDir);

        mListView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

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

        CacheDirSelectionAdapter adapter = new CacheDirSelectionAdapter(this);

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
    }

    public List<String> getDirNamesList() {
        return mDirectoryNamesList;
    }

    public List<String> getDirSizesList() {
        return mDirectorySizesList;
    }

    public String getmCurrentDir() {
        return mCurrentDir;
    }
}
