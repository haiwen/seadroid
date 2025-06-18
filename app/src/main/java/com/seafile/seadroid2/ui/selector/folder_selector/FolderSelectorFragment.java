package com.seafile.seadroid2.ui.selector.folder_selector;

import android.app.Activity;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.databinding.FragmentFolderSelectorBinding;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.FileTools;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupConfigActivity;
import com.seafile.seadroid2.ui.repo.ScrollState;
import com.seafile.seadroid2.view.TipsViews;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class FolderSelectorFragment extends BaseFragmentWithVM<FolderSelectorViewModel> {

    private FragmentFolderSelectorBinding binding;
    private LinearLayoutManager rvManager;
    private LinearLayoutManager rvTabbarManager;

    private List<String> allPathsList;

    private List<TabBarFileBean> mTabbarFileList;
    private String mCurrentPath;
    private FileListAdapter mFileListAdapter;
    private TabBarFileListAdapter mTabBarFileListAdapter;
    private FolderBackupConfigActivity mActivity;
    private String initialPath;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mActivity = (FolderBackupConfigActivity) getActivity();

        binding = FragmentFolderSelectorBinding.inflate(inflater, container, false);

        rvManager = new LinearLayoutManager(getActivity(), LinearLayoutManager.VERTICAL, false);
        binding.rvOfList.setLayoutManager(rvManager);

        rvTabbarManager = new LinearLayoutManager(getActivity(), LinearLayoutManager.HORIZONTAL, false);
        binding.rvOfPath.setLayoutManager(rvTabbarManager);

        mTabBarFileListAdapter = new TabBarFileListAdapter(getActivity(), mTabbarFileList);
        binding.rvOfPath.setAdapter(mTabBarFileListAdapter);

//        chooseDirPage = mActivity.isChooseDirPage();
        return binding.getRoot();
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initView();
        initViewModel();
        initAdapter();
        initData();

        loadData();
    }

    private void initView() {
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);

        mTabBarFileListAdapter.setOnItemClickListener((tabBarFileBean, position) -> {
            TabBarFileBean item = mTabbarFileList.get(position);
            mCurrentPath = item.getFilePath();

            if (mTabbarFileList.size() > 1) {
                refreshFileAndTabBar(TYPE_DEL_TAB_BAR);
            }

            loadData();
        });
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), aBoolean -> {
            binding.swipeRefreshLayout.setRefreshing(aBoolean);
        });

        getViewModel().getDataListLiveData().observe(getViewLifecycleOwner(), fileBeans -> {
            notifyDataChanged(fileBeans);

            restoreScrollPosition();
        });
    }

    private void initAdapter() {
        mFileListAdapter = new FileListAdapter();
        mFileListAdapter.setOnFileItemChangeListener((fileBean, position, isChecked) -> {
            if (!isChecked) {
                getViewModel().removeSpecialPath(fileBean.getFilePath());
            } else {
                getViewModel().addSpecialPath(fileBean.getFilePath());
            }
        });

        mFileListAdapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            saveScrollPosition();

            FileBean item = mFileListAdapter.getItems().get(i);
            if (!item.isDir()) {
                Toasts.show(R.string.selection_file_type);
                return;
            }

            mCurrentPath = item.getFilePath();
            refreshFileAndTabBar(TYPE_ADD_TAB_BAR);

            loadData();
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(mFileListAdapter).build();
        binding.rvOfList.setAdapter(helper.getAdapter());
    }

    private void notifyDataChanged(List<FileBean> list) {
        if (CollectionUtils.isEmpty(list)) {
            showAdapterTip();
        } else {
            mFileListAdapter.submitList(list);
        }
    }

    private void showAdapterTip() {
        mFileListAdapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.dir_empty);
        mFileListAdapter.setStateView(tipView);
        mFileListAdapter.setStateViewEnable(true);
    }

    private void loadData() {
        getViewModel().loadData(mCurrentPath);
    }

    private void initData() {
        List<String> selectPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (!CollectionUtils.isEmpty(selectPaths)) {
            getViewModel().setSelectFilePathList(selectPaths);
        }

        allPathsList = initRootPath(getActivity());
        mTabbarFileList = new ArrayList<>();
        refreshFileAndTabBar(TYPE_INIT_TAB_BAR);
    }

    private List<String> initRootPath(Activity activity) {
        List<String> allPaths = FileTools.getAllPaths(activity);
        if (allPaths.isEmpty()) {
            mCurrentPath = Constants.DEFAULT_ROOTPATH;
        } else {
            mCurrentPath = allPaths.get(0);
        }
        initialPath = mCurrentPath;
        return allPaths;
    }

    private void refreshFileAndTabBar(int tabbarType) {
        updateTabbarFileBeanList(mTabbarFileList, mCurrentPath, tabbarType, allPathsList);
    }

    public boolean onBackPressed() {
        if (mCurrentPath.equals(initialPath) || allPathsList.contains(mCurrentPath)) {
            return false;
        } else {
            mCurrentPath = FileTools.getParentPath(mCurrentPath);
            refreshFileAndTabBar(TYPE_DEL_TAB_BAR);

            loadData();
            return true;
        }
    }


    private final Map<String, ScrollState> scrollPositions = Maps.newHashMap();

    private void saveScrollPosition() {
        View vi = binding.rvOfList.getChildAt(0);
        int top = (vi == null) ? 0 : vi.getTop();
        final int index = rvManager.findFirstVisibleItemPosition();
        final ScrollState state = new ScrollState(index, top);

        removeScrollPosition();

        scrollPositions.put(mCurrentPath, state);
    }

    private void removeScrollPosition() {
        scrollPositions.remove(mCurrentPath);
    }

    private void restoreScrollPosition() {
        ScrollState state = scrollPositions.get(mCurrentPath);

        if (state != null) {
            rvManager.scrollToPositionWithOffset(state.index, state.top);
        } else {
            rvManager.scrollToPosition(0);
        }
    }

    public List<String> getSelectedPath() {
        return getViewModel().getSelectFilePathList();
    }

    public static final int TYPE_ADD_TAB_BAR = 0;
    public static final int TYPE_DEL_TAB_BAR = 1;
    public static final int TYPE_INIT_TAB_BAR = 2;

    public static void getTabbarFileBeanList(List<TabBarFileBean> tabbarList, String path, List<String> allPathsList) {
        if (allPathsList.contains(path)) {
            tabbarList.add(0, new TabBarFileBean(path, SeadroidApplication.getAppContext().getString(R.string.internal_storage)));
        }
    }

    private void updateTabbarFileBeanList(List<TabBarFileBean> tabbarList, String path, int type, List<String> allPathsList) {
        switch (type) {
            case TYPE_ADD_TAB_BAR:
                tabbarList.add(new TabBarFileBean(path));
                break;
            case TYPE_DEL_TAB_BAR:
                for (int i = tabbarList.size() - 1; i >= 0; i--) {
                    if (tabbarList.get(i).getFilePath().length() > path.length()) {
                        tabbarList.remove(i);
                    } else {
                        break;
                    }
                }
                break;
            case TYPE_INIT_TAB_BAR:
                if (tabbarList == null) {
                    tabbarList = new ArrayList<>();
                } else {
                    tabbarList.clear();
                }
                getTabbarFileBeanList(tabbarList, path, allPathsList);
                break;
        }

        if (mTabBarFileListAdapter != null) {
            mTabBarFileListAdapter.updateListData(tabbarList);
            mTabBarFileListAdapter.notifyDataSetChanged();

            if (mTabBarFileListAdapter.getItemCount() > 0) {
                rvTabbarManager.scrollToPosition(mTabBarFileListAdapter.getItemCount() - 1);
            }
        }
    }
}

