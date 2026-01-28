package com.seafile.seadroid2.ui.selector.folder_selector;

import android.content.BroadcastReceiver;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.tabs.TabLayout;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.databinding.FragmentFolderSelectorBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.model.StorageInfo;
import com.seafile.seadroid2.framework.util.FileUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.repo.repo_list.ScrollState;
import com.seafile.seadroid2.view.TipsViews;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Collectors;

public class FolderSelectorFragment extends BaseFragmentWithVM<FolderSelectorViewModel> {

    private final String TAG = "FolderSelectorFragment";

    private BroadcastReceiver mountReceiver;

    private FragmentFolderSelectorBinding binding;
    private LinearLayoutManager rvManager;
    private LinearLayoutManager rvTabbarManager;

    private FileListAdapter mFileListAdapter;
    private NavPathListAdapter mNavPathAdapter;

    private String mCurrentPath;

    private final List<TabVolumeBean> _pathList = new Stack<>();

    public List<TabVolumeBean> getNavPathList() {
        return _pathList;
    }

    private List<StorageInfo> volumeList;

    @Override
    public void onDestroy() {
        super.onDestroy();

        BusHelper.getCommonObserver().removeObserver(busObserver);
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentFolderSelectorBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        BusHelper.getCommonObserver().observe(getViewLifecycleOwner(), busObserver);

        initViewModel();
        initView();

        reload();
    }

    private final Observer<String> busObserver = new Observer<String>() {
        @Override
        public void onChanged(String actionStr) {
            if (TextUtils.isEmpty(actionStr)) {
                return;
            }

            String action;
            String path;
            if (actionStr.contains("-")) {
                String[] s = actionStr.split("-");
                action = s[0];
                path = s[1];
            } else {
                action = actionStr;
                path = "";
            }

            if (Intent.ACTION_MEDIA_MOUNTED.equals(action)) {
                SLogs.d(TAG, "Storage", "设备挂载");
                reload();
            } else if (Intent.ACTION_MEDIA_UNMOUNTED.equals(action)) {
                SLogs.d(TAG, "Storage", "设备卸载");
                reload();
            } else if (Intent.ACTION_MEDIA_REMOVED.equals(action)) {
                SLogs.d(TAG, "Storage", "设备移除");
                reload();
            }
        }
    };

    private void reload() {
        initRootPath();
        initData();

        initNavListAdapter();
        initFileListAdapter();
        initTabLayout();

        loadData();
    }

    private void initView() {
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), aBoolean -> {
            binding.swipeRefreshLayout.setRefreshing(aBoolean);
        });

        getViewModel().getLocalFileListLiveData().observe(getViewLifecycleOwner(), fileBeans -> {
            notifyFileListDataChanged(fileBeans);

            restoreScrollPosition();
        });
    }

    private void initRootPath() {
        volumeList = FileUtils.getAllStorageInfos(requireContext());
        for (StorageInfo info : volumeList) {
            SLogs.d("Storage", "路径: " + info.path + "，类型: " + info.label +
                    "，可移除: " + info.isRemovable + "，是否主存储: " + info.isPrimary);
        }

        if (volumeList.isEmpty()) {
            mCurrentPath = Constants.DEFAULT_ROOTPATH;
        } else {
            mCurrentPath = volumeList.get(0).path;
        }

        // init nav path list data
        if (!volumeList.isEmpty()) {
            getNavPathList().clear();

            StorageInfo storageInfo = volumeList.get(0);
            TabVolumeBean tabVolumeBean = new TabVolumeBean(storageInfo.path, storageInfo.label, ItemPositionEnum.ALL);
            getNavPathList().add(tabVolumeBean);
        }
    }

    private void initData() {
        List<String> selectPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (!CollectionUtils.isEmpty(selectPaths)) {
            getViewModel().setSelectFilePathList(selectPaths);
        }
    }

    private void initNavListAdapter() {
        rvTabbarManager = new LinearLayoutManager(getActivity(), LinearLayoutManager.HORIZONTAL, false);
        binding.rvOfPath.setLayoutManager(rvTabbarManager);

        mNavPathAdapter = new NavPathListAdapter(getActivity());
        mNavPathAdapter.setOnItemClickListener((item, position) -> {
            if (mCurrentPath.equals(item.getFilePath())) {
                return;
            }

            mCurrentPath = item.getFilePath();

            if (mNavPathAdapter.getItemCount() > 1) {
                navSpecialPath(item);
            }

            loadData();
        });
        binding.rvOfPath.setAdapter(mNavPathAdapter);
        mNavPathAdapter.updateListData(getNavPathList());
    }

    private void initFileListAdapter() {
        rvManager = new LinearLayoutManager(getActivity(), LinearLayoutManager.VERTICAL, false);
        binding.rvOfList.setLayoutManager(rvManager);

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

            addNavPathListData(new TabVolumeBean(item.getFilePath(), item.getFileName()));

            loadData();
        });

        binding.rvOfList.setAdapter(mFileListAdapter);
    }

    private void addNavPathListData(TabVolumeBean bean) {
        getNavPathList().add(bean);
        resetNavPathListItemPosition();
        notifyNavPathListDataChanged();
    }


    private void initTabLayout() {
        binding.slidingTabs.removeAllTabs();

        List<StorageInfo> vList = volumeList.stream().filter(StorageInfo::isAvailable).collect(Collectors.toList());

        if (vList.size() == 1) {
            binding.slidingTabs.setVisibility(View.GONE);
            return;
        }


        binding.slidingTabs.setVisibility(View.VISIBLE);
        binding.slidingTabs.setTabIndicatorAnimationMode(TabLayout.INDICATOR_ANIMATION_MODE_ELASTIC);
        binding.slidingTabs.setSelectedTabIndicator(R.drawable.cat_tabs_rounded_line_indicator);
        binding.slidingTabs.setTabIndicatorFullWidth(false);
        binding.slidingTabs.setTabGravity(TabLayout.GRAVITY_CENTER);

        for (StorageInfo storageInfo : vList) {
            TabLayout.Tab tab = binding.slidingTabs.newTab();
            tab.setTag(storageInfo.path);
            tab.setText(storageInfo.label);
            binding.slidingTabs.addTab(tab);
        }

        binding.slidingTabs.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                String tag = (String) tab.getTag();
                if (tag != null) {
                    getViewModel().getSelectFilePathList().clear();

                    //reset nav path list data
                    getNavPathList().clear();
                    for (StorageInfo storageInfo : volumeList) {
                        if (storageInfo.path.equals(tag)) {
                            TabVolumeBean tabVolumeBean = new TabVolumeBean(storageInfo.path, storageInfo.label);
                            getNavPathList().add(tabVolumeBean);
                            break;
                        }
                    }
                    resetNavPathListItemPosition();
                    notifyNavPathListDataChanged();

                    mCurrentPath = tag;
                    loadData();
                }
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {
            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {
            }
        });
    }

    private void notifyFileListDataChanged(List<FileBean> list) {
        if (CollectionUtils.isEmpty(list)) {
            mFileListAdapter.submitList(null);
            TextView tipView = TipsViews.getTipTextView(requireContext());
            tipView.setText(R.string.dir_empty);
            mFileListAdapter.setStateView(tipView);
            mFileListAdapter.setStateViewEnable(true);
        } else {
            mFileListAdapter.submitList(list);
        }
    }

    private void loadData() {
        getViewModel().loadLocalFileData(mCurrentPath);
    }

    public boolean onBackPressed() {
        if (getNavPathList().size() == 1) {
            return false;
        } else {

            getNavPathList().remove(getNavPathList().size() - 1);
            resetNavPathListItemPosition();

            TabVolumeBean bean = getNavPathList().get(getNavPathList().size() - 1);
            mCurrentPath = bean.getFilePath();
            notifyNavPathListDataChanged();

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

    private void navSpecialPath(TabVolumeBean bean) {
        if (getNavPathList().size() == 1) {
            return;
        }

        List<TabVolumeBean> newPathList = new ArrayList<>();
        for (TabVolumeBean tabVolumeBean : getNavPathList()) {
            if (tabVolumeBean.getFilePath().length() <= bean.getFilePath().length()) {
                newPathList.add(tabVolumeBean);
            }
        }

        getNavPathList().clear();
        getNavPathList().addAll(newPathList);

        resetNavPathListItemPosition();
        notifyNavPathListDataChanged();
    }

    private void notifyNavPathListDataChanged() {
        if (mNavPathAdapter != null) {
            mNavPathAdapter.updateListData(getNavPathList());

            if (mNavPathAdapter.getItemCount() > 0) {
                rvTabbarManager.scrollToPosition(mNavPathAdapter.getItemCount() - 1);
            }
        }
    }

    private void resetNavPathListItemPosition() {
        for (TabVolumeBean tabVolumeBean : getNavPathList()) {
            tabVolumeBean.setItemPosition(ItemPositionEnum.NONE);
        }

        if (getNavPathList().size() == 1) {
            getNavPathList().get(0).setItemPosition(ItemPositionEnum.ALL);
        } else {
            getNavPathList().get(0).setItemPosition(ItemPositionEnum.START);
            getNavPathList().get(getNavPathList().size() - 1).setItemPosition(ItemPositionEnum.END);
        }
    }

}

