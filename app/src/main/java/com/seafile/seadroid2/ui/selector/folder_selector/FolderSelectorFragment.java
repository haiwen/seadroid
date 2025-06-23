package com.seafile.seadroid2.ui.selector.folder_selector;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Build;
import android.os.Bundle;
import android.os.storage.StorageManager;
import android.os.storage.StorageVolume;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.tabs.TabLayout;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.FragmentFolderSelectorBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.model.StorageInfo;
import com.seafile.seadroid2.framework.util.FileTools;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.repo.ScrollState;
import com.seafile.seadroid2.view.TipsViews;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

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
    public void onStart() {
        super.onStart();
        startWatching();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        stopWatching();
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentFolderSelectorBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initViewModel();

        initView();

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
        volumeList = FileTools.getAllStorageInfos(requireContext());
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

    /**
     * Start observing mount/unmount events
     */
    public void startWatching() {
        if (mountReceiver != null) return;
        mountReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (Intent.ACTION_MEDIA_MOUNTED.equals(intent.getAction()) ||
                        Intent.ACTION_MEDIA_REMOVED.equals(intent.getAction()) ||
                        Intent.ACTION_MEDIA_UNMOUNTED.equals(intent.getAction())) {
                    notifyMountChanged();
                }
            }
        };
        IntentFilter filter = new IntentFilter();
        filter.addAction(Intent.ACTION_MEDIA_MOUNTED);
        filter.addAction(Intent.ACTION_MEDIA_REMOVED);
        filter.addAction(Intent.ACTION_MEDIA_UNMOUNTED);
        filter.addDataScheme("file");
        requireContext().registerReceiver(mountReceiver, filter);
    }

    private void notifyMountChanged() {
        List<StorageInfo> newVolumeList = FileTools.getAllStorageInfos(requireContext());
        for (StorageInfo info : newVolumeList) {
            SLogs.d("新存储 Storage", "路径: " + info.path + "，类型: " + info.label +
                    "，可移除: " + info.isRemovable + "，是否主存储: " + info.isPrimary);
        }
    }

    /**
     * Stop observing
     */
    public void stopWatching() {
        if (mountReceiver != null) {
            requireContext().unregisterReceiver(mountReceiver);
            mountReceiver = null;
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
        resetNavPathItemPosition();
        notifyNavPathListDataChanged();
    }

    private void resetNavPathItemPosition() {
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

    private void initTabLayout() {
//        if (volumeList == null || volumeList.size() == 1) {
//            binding.slidingTabs.setVisibility(View.GONE);
//            return;
//        }

        binding.slidingTabs.setVisibility(View.VISIBLE);
        binding.slidingTabs.setTabIndicatorAnimationMode(TabLayout.INDICATOR_ANIMATION_MODE_ELASTIC);
        binding.slidingTabs.setSelectedTabIndicator(R.drawable.cat_tabs_rounded_line_indicator);
        binding.slidingTabs.setTabIndicatorFullWidth(false);
        binding.slidingTabs.setTabGravity(TabLayout.GRAVITY_CENTER);

        for (StorageInfo storageInfo : volumeList) {
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
                    resetNavPathItemPosition();
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
        getViewModel().loadData(mCurrentPath);
    }

    public boolean onBackPressed() {
        if (getNavPathList().size() == 1) {
            return false;
        } else {
            getNavPathList().remove(getNavPathList().size() - 1);
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

    public String getPrettyVolumeName(String volumePath) {
        StorageManager storageManager = (StorageManager) requireContext().getSystemService(Context.STORAGE_SERVICE);

        if (storageManager == null) {
            return volumePath;
        }

        List<StorageVolume> volumes = storageManager.getStorageVolumes();
        for (StorageVolume volume : volumes) {
            try {
                File dir;

                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                    dir = volume.getDirectory();
                } else {
                    // Before API 30 way to get a volume path
                    Method getPathMethod = StorageVolume.class.getDeclaredMethod("getPathFile");
                    dir = (File) getPathMethod.invoke(volume);
                }

                if (dir != null && dir.getAbsolutePath().equals(volumePath)) {
                    return volume.getDescription(requireContext()); // e.g. "Internal storage" or "SD card"
                }
            } catch (Exception e) {
                // Will fallback to volumePath
            }
        }

        return volumePath;
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
        resetNavPathItemPosition();
        notifyNavPathListDataChanged();
    }

    private void notifyNavPathListDataChanged() {
        if (mNavPathAdapter != null) {
            mNavPathAdapter.updateListData(getNavPathList());
            mNavPathAdapter.notifyDataSetChanged();

            if (mNavPathAdapter.getItemCount() > 0) {
                rvTabbarManager.scrollToPosition(mNavPathAdapter.getItemCount() - 1);
            }
        }
    }
}

