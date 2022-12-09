package com.seafile.seadroid2.backupdirectory;

import android.app.Activity;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.data.CameraSyncEvent;

import org.greenrobot.eventbus.EventBus;

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
    private String directoryFilePath;
    private boolean chooseDirPage;
    private Button mButton;


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mActivity = (DirectoryUploadConfigActivity) getActivity();
        View rootView = getActivity().getLayoutInflater().inflate(R.layout.folder_selection_fragment, null);
        mTabbarFileRecyclerView = (RecyclerView) rootView.findViewById(R.id.rcv_tabbar_files_list);
        mFileRecyclerView = (RecyclerView) rootView.findViewById(R.id.rcv_files_list);
        imbChangeSdCard = (ImageButton) rootView.findViewById(R.id.imb_select_sdcard);
        mButton = (Button) rootView.findViewById(R.id.bt_dir_click_to_finish);
        directoryFilePath = SettingsManager.instance().getDirectoryFilePath();
        mFileRecyclerView.setLayoutManager(new LinearLayoutManager(getActivity(), LinearLayoutManager.VERTICAL, false));
        mFileListAdapter = new FileListAdapter(getActivity(), mFileList,directoryFilePath);
        mFileRecyclerView.setAdapter(mFileListAdapter);
        mTabbarFileRecyclerView.setLayoutManager(new LinearLayoutManager(getActivity(), LinearLayoutManager.HORIZONTAL, false));
        mTabbarFileListAdapter = new TabbarFileListAdapter(getActivity(), mTabbarFileList);
        mTabbarFileRecyclerView.setAdapter(mTabbarFileListAdapter);
        chooseDirPage = mActivity.isChooseDirPage();
        init();
        initData();
        return rootView;
    }
    private List<String> litePalPath;
    private void init() {
        if (chooseDirPage) {
            mButton.setVisibility(View.VISIBLE);
        } else {
            mButton.setVisibility(View.GONE);
        }
        FolderBean mLitePalFolder = mActivity.getLitePalFolder();
        if (mLitePalFolder == null) {
            litePalPath = new ArrayList<>();
        } else {
            litePalPath = mLitePalFolder.getSelectFolder();
            if (litePalPath == null) {
                litePalPath = new ArrayList<>();
            }
        }

        mButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (litePalPath != null) {
                    EventBus.getDefault().post(new CameraSyncEvent("saveSet", litePalPath.size()));
                }
                mActivity.saveUpdateFolder();
                mActivity.finish();
            }
        });
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
                    if (item.equals(fb)) {
                        if (fb.isChecked()) {
                            for (String str : litePalPath) {
                                if (item.getFilePath().equals(str)) {
                                    litePalPath.remove(str);
                                }
                            }
                            fb.setChecked(false);

                        } else {
                            litePalPath.add(item.getFilePath());
                            fb.setChecked(true);
                        }
                        mActivity.setFileList(litePalPath);
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
        BeanListManager.upDataFileBeanListByAsyn(litePalPath, mFileList, mFileListAdapter, mCurrentPath, mShowFileTypes, mSortType);
        BeanListManager.upDataTabbarFileBeanList(mTabbarFileList, mTabbarFileListAdapter, mCurrentPath, tabbarType, mSdCardList);
    }


}

