package com.seafile.seadroid2.folderbackup.selectfolder;

import android.app.Activity;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.folderbackup.FolderBackupConfigActivity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SelectBackupFolderFragment extends Fragment {

    private RecyclerView mTabbarFileRecyclerView, mFileRecyclerView;
    private SelectOptions mSelectOptions;
    private List<String> allPathsList;
    private List<String> mShowFileTypes;
    private int mSortType;
    private List<FileBean> mFileList;
    private List<TabbarFileBean> mTabbarFileList;
    private String mCurrentPath;
    private FileListAdapter mFileListAdapter;
    private TabbarFileListAdapter mTabbarFileListAdapter;
    private FolderBackupConfigActivity mActivity;
    private boolean chooseDirPage;
    private Button mButton;
    private List<String> selectPaths;
    private String initialPath;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mActivity = (FolderBackupConfigActivity) getActivity();
        View rootView = getActivity().getLayoutInflater().inflate(R.layout.folder_selection_fragment, null);
        mTabbarFileRecyclerView = (RecyclerView) rootView.findViewById(R.id.rcv_tabbar_files_list);
        mFileRecyclerView = (RecyclerView) rootView.findViewById(R.id.rcv_files_list);
        mButton = (Button) rootView.findViewById(R.id.bt_dir_click_to_finish);
        mFileRecyclerView.setLayoutManager(new LinearLayoutManager(getActivity(),
                LinearLayoutManager.VERTICAL, false));
        mFileListAdapter = new FileListAdapter(getActivity(), mFileList);
        mFileRecyclerView.setAdapter(mFileListAdapter);
        mTabbarFileRecyclerView.setLayoutManager(new LinearLayoutManager(getActivity(),
                LinearLayoutManager.HORIZONTAL, false));
        mTabbarFileListAdapter = new TabbarFileListAdapter(getActivity(), mTabbarFileList);
        mTabbarFileRecyclerView.setAdapter(mTabbarFileListAdapter);
        chooseDirPage = mActivity.isChooseDirPage();
        init();
        initData();
        return rootView;
    }

    private void init() {
        if (chooseDirPage) {
            mButton.setVisibility(View.VISIBLE);
        } else {
            mButton.setVisibility(View.GONE);
        }
        selectPaths = mActivity.getSelectFolderPath();
        if (selectPaths == null) {
            selectPaths = new ArrayList<>();
        }

        mButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mActivity.saveFolderConfig();
                mActivity.finish();
            }
        });

        mFileListAdapter.setOnItemClickListener(new OnFileItemClickListener() {
            @Override
            public void itemClick(int position) {
                FileBean item = mFileList.get(position);
                if (item.isFile()) {
                    Toast.makeText(getActivity(), getActivity().getString(R.string.selection_file_type), Toast.LENGTH_SHORT).show();
                } else {
                    mCurrentPath = item.getFilePath();
                    refreshFileAndTabbar(BeanListManager.TypeAddTabbar);
                }
            }

            @Override
            public void checkBoxClick(View view, int position) {
                FileBean item = mFileList.get(position);
                for (FileBean fb : mFileList) {
                    if (item.equals(fb)) {
                        if (fb.isChecked()) {
                            for (int i = 0; i < selectPaths.size(); i++) {
                                if (item.getFilePath().equals(selectPaths.get(i))) {
                                    selectPaths.remove(i);
                                    i--;
                                }
                            }
                            fb.setChecked(false);

                        } else {
                            selectPaths.add(item.getFilePath());
                            fb.setChecked(true);
                        }
                        mActivity.setFolderPathList(selectPaths);
                    }
                }
                view.post(new Runnable() {
                    @Override
                    public void run() {
                        mFileListAdapter.updateListData(mFileList);
                        mFileListAdapter.notifyDataSetChanged();
                    }
                });
            }
        });

        mTabbarFileListAdapter.setOnItemClickListener(new OnFileItemClickListener() {
            @Override
            public void itemClick(int position) {
                TabbarFileBean item = mTabbarFileList.get(position);
                mCurrentPath = item.getFilePath();

                if (mTabbarFileList.size() > 1) {
                    refreshFileAndTabbar(BeanListManager.TypeDelTabbar);
                }
            }

            @Override
            public void checkBoxClick(View view, int position) {

            }
        });
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

    private void initData() {
        mSelectOptions = SelectOptions.getResetInstance(getActivity());
        allPathsList= initRootPath(getActivity());
        mShowFileTypes = Arrays.asList(mSelectOptions.getShowFileTypes());
        mSortType = mSelectOptions.getSortType();
        mFileList = new ArrayList<>();
        mTabbarFileList = new ArrayList<>();
        refreshFileAndTabbar(BeanListManager.TypeInitTabbar);
    }

    private List<String> initRootPath(Activity activity) {
        List<String> allPaths = FileTools.getAllPaths(activity);
        mCurrentPath = mSelectOptions.rootPath;
        if (mCurrentPath == null) {
            if (allPaths.isEmpty()) {
                mCurrentPath = Constants.DEFAULT_ROOTPATH;
            } else {
                mCurrentPath = allPaths.get(0);
            }
        }
        initialPath =mCurrentPath;
        return allPaths;
    }

    private void refreshFileAndTabbar(int tabbarType) {
        BeanListManager.upDataFileBeanListByAsyn(getActivity(), selectPaths, mFileList, mFileListAdapter,
                mCurrentPath, mShowFileTypes, mSortType);
        BeanListManager.upDataTabbarFileBeanList(mTabbarFileList, mTabbarFileListAdapter,
                mCurrentPath, tabbarType, allPathsList);
    }

    public boolean onBackPressed() {
        if (mCurrentPath.equals(initialPath)||allPathsList.contains(mCurrentPath)){
            return false;
        }else {
            mCurrentPath=FileTools.getParentPath(mCurrentPath);
            refreshFileAndTabbar(BeanListManager.TypeDelTabbar);
            return true;
        }
    }

}

