package com.seafile.seadroid2.backupdirectory;

import android.app.Activity;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.Toast;

import com.seafile.seadroid2.R;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DirectorySelectionFragment extends Fragment {

    private RecyclerView mTabbarFileRecyclerView, mFileRecyclerView;
    private ImageButton imbChangeSdCard;
    private SelectOptions mSelectOptions;
    private List<String> mSdCardList;
    private List<String> mShowFileTypes;
    private int mSortType;
    private List<FileBean> mFileList;
    private List<TabbarFileBean> mTabbarFileList;
    private String mCurrentPath;
    private FileListAdapter mFileListAdapter;
    private TabbarFileListAdapter mTabbarFileListAdapter;
    private DirectoryUploadConfigActivity mActivity;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mActivity = (DirectoryUploadConfigActivity) getActivity();
        View rootView = getActivity().getLayoutInflater().inflate(R.layout.folder_selection_fragment, null);

        mTabbarFileRecyclerView = (RecyclerView) rootView.findViewById(R.id.rcv_tabbar_files_list);
        mFileRecyclerView = (RecyclerView) rootView.findViewById(R.id.rcv_files_list);
        imbChangeSdCard = (ImageButton) rootView.findViewById(R.id.imb_select_sdcard);

        mFileRecyclerView.setLayoutManager(new LinearLayoutManager(getActivity(), LinearLayoutManager.VERTICAL, false));
        mFileListAdapter = new FileListAdapter(getActivity(), mFileList);
        mFileRecyclerView.setAdapter(mFileListAdapter);
        mTabbarFileRecyclerView.setLayoutManager(new LinearLayoutManager(getActivity(), LinearLayoutManager.HORIZONTAL, false));
        mTabbarFileListAdapter = new TabbarFileListAdapter(getActivity(), mTabbarFileList);
        mTabbarFileRecyclerView.setAdapter(mTabbarFileListAdapter);

        init();
        initData();
        return rootView;
    }

    private void init() {
        mFileListAdapter.setOnItemClickListener(new OnFileItemClickListener() {
            @Override
            public void click(int position) {
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
                    if (!item.equals(fb)) {
                        fb.setChecked(false);
                    } else {
                        fb.setChecked(true);
                        mActivity.setFileBean(fb);
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
            public void click(int position) {
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
        mSdCardList = initRootPath(getActivity(), imbChangeSdCard);
        mShowFileTypes = Arrays.asList(mSelectOptions.getShowFileTypes());
        mSortType = mSelectOptions.getSortType();
        mFileList = new ArrayList<>();
        mTabbarFileList = new ArrayList<>();
        refreshFileAndTabbar(BeanListManager.TypeInitTabbar);
    }

    private List<String> initRootPath(Activity activity, ImageButton imb) {
        List<String> SdCardList = FileTools.getAllSdPaths(activity);
        mCurrentPath = mSelectOptions.rootPath;
        if (mCurrentPath == null) {
            if (SdCardList.isEmpty()) {
                mCurrentPath = Constants.DEFAULT_ROOTPATH;
            } else {
                mCurrentPath = SdCardList.get(0);
            }
        }
        if (!SdCardList.isEmpty() && SdCardList.size() > 1) {
            imb.setVisibility(View.VISIBLE);
        } else {
            imb.setVisibility(View.INVISIBLE);
        }
        return SdCardList;
    }

    private void refreshFileAndTabbar(int tabbarType) {
        if (PermissionsTools.isAndroid11() && FileTools.isAndroidDataPath(mCurrentPath)) {
            UriTools.upDataFileBeanListByUri(getActivity(), UriTools.file2Uri(mCurrentPath), mFileList, mFileListAdapter, mShowFileTypes, mSortType);
            UriTools.upDataTabbarFileBeanListByUri(mTabbarFileList, mTabbarFileListAdapter, mCurrentPath, tabbarType, mSdCardList);
        } else {
            BeanListManager.upDataFileBeanListByAsyn(mFileList, mFileListAdapter, mCurrentPath, mShowFileTypes, mSortType);
            BeanListManager.upDataTabbarFileBeanList(mTabbarFileList, mTabbarFileListAdapter, mCurrentPath, tabbarType, mSdCardList);
        }
    }


}

