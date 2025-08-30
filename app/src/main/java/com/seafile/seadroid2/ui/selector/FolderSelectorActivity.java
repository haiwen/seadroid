package com.seafile.seadroid2.ui.selector;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.tabs.TabLayout;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.databinding.ActivityFolderSelectorBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.model.StorageInfo;
import com.seafile.seadroid2.framework.util.FileTools;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetNewLocalFolderDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.repo.ScrollState;
import com.seafile.seadroid2.ui.selector.folder_selector.Constants;
import com.seafile.seadroid2.ui.selector.folder_selector.FileBean;
import com.seafile.seadroid2.ui.selector.folder_selector.FileListAdapter;
import com.seafile.seadroid2.ui.selector.folder_selector.FolderSelectorViewModel;
import com.seafile.seadroid2.ui.selector.folder_selector.NavPathListAdapter;
import com.seafile.seadroid2.ui.selector.folder_selector.TabVolumeBean;
import com.seafile.seadroid2.view.TipsViews;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Collectors;

public class FolderSelectorActivity extends BaseActivityWithVM<FolderSelectorViewModel> {
    public static final String FOLDER_SELECTED_PATHS = "folder_selected_paths";
    public static final String PARAM_SELECTOR_MAX_COUNT = "param_max_select_count";
    public static final String PARAM_FILTER_PATHS = "param_filter_paths";

    private final String TAG = "FolderSelectorActivity";
    /**
     * if maxSelectCount is 1, means single select.
     * if maxSelectCount is 0, means no limit.
     * if maxSelectCount is greater than 1, means multi select.
     * <p>
     * default 0, no limit.;
     */
    private int maxSelectCount = 0;
    private List<String> filterPaths;
    private ActivityFolderSelectorBinding binding;
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
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityFolderSelectorBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());

        initView();


        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (!onBackTo()) {
                    finishPage();
                } else {

                }
            }
        });

        BusHelper.getCommonObserver().observe(this, busObserver);

        if (getIntent().hasExtra(PARAM_SELECTOR_MAX_COUNT)) {
            maxSelectCount = getIntent().getIntExtra(PARAM_SELECTOR_MAX_COUNT, 0);
        }

        if (getIntent().hasExtra(PARAM_FILTER_PATHS)) {
            filterPaths = getIntent().getStringArrayListExtra(PARAM_FILTER_PATHS);
        }

        initViewModel();

        reload();
    }


    private void initView() {
        Toolbar toolbar = getActionBarToolbar();
        if (toolbar != null) {
            toolbar.setNavigationOnClickListener(v -> {
                finish();
            });
        }
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(R.string.settings_select_backup_folder_title);
        }

        binding.confirmButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                finishPage();
            }
        });

        binding.swipeRefreshLayout.setOnRefreshListener(this::loadLocalFileData);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_share_to_seafile, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == R.id.create_new_folder) {
            showNewDirDialog();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void showNewDirDialog() {
        BottomSheetNewLocalFolderDialogFragment sheetDialog = BottomSheetNewLocalFolderDialogFragment.newInstance(mCurrentPath);
        sheetDialog.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    loadLocalFileData();
                }
            }
        });
        sheetDialog.show(getSupportFragmentManager(), "BottomSheetNewLocalFolderDialogFragment");
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, aBoolean -> {
            binding.swipeRefreshLayout.setRefreshing(aBoolean);
        });

        getViewModel().getLocalFileListLiveData().observe(this, fileBeans -> {
            notifyFileListDataChanged(fileBeans);

            restoreScrollPosition();
        });
    }


    private void reload() {
        initRootPath();

        initNavListAdapter();
        initFileListAdapter();
        initTabLayout();

        loadLocalFileData();
    }


    private void initRootPath() {
        volumeList = FileTools.getAllStorageInfos(this);
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

    private void initNavListAdapter() {
        rvTabbarManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
        binding.rvOfPath.setLayoutManager(rvTabbarManager);

        mNavPathAdapter = new NavPathListAdapter(this);
        mNavPathAdapter.setOnItemClickListener((item, position) -> {
            if (mCurrentPath.equals(item.getFilePath())) {
                return;
            }

            mCurrentPath = item.getFilePath();

            if (mNavPathAdapter.getItemCount() > 1) {
                navSpecialPath(item);
            }

            loadLocalFileData();
        });
        binding.rvOfPath.setAdapter(mNavPathAdapter);
        mNavPathAdapter.updateListData(getNavPathList());
    }

    private void initFileListAdapter() {
        rvManager = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        binding.rvOfList.setLayoutManager(rvManager);

        mFileListAdapter = new FileListAdapter();
        mFileListAdapter.setMaxSelectCount(maxSelectCount);
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

            loadLocalFileData();
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
                    loadLocalFileData();
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
            TextView tipView = TipsViews.getTipTextView(this);
            tipView.setText(R.string.dir_empty);
            mFileListAdapter.setStateView(tipView);
            mFileListAdapter.setStateViewEnable(true);
        } else {
            mFileListAdapter.submitList(list);
        }
    }

    private void loadLocalFileData() {
        getViewModel().loadLocalFileData(mCurrentPath, filterPaths);
    }

    private boolean onBackTo() {
        if (getNavPathList().size() == 1) {
            return false;
        } else {
            getNavPathList().remove(getNavPathList().size() - 1);
            resetNavPathListItemPosition();

            TabVolumeBean bean = getNavPathList().get(getNavPathList().size() - 1);
            mCurrentPath = bean.getFilePath();
            notifyNavPathListDataChanged();

            loadLocalFileData();
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


    public void finishPage() {
        Intent intent = new Intent();
        List<String> selectedFolderPaths = getViewModel().getSelectFilePathList();
        if (CollectionUtils.isEmpty(selectedFolderPaths)) {
            intent.putStringArrayListExtra(FOLDER_SELECTED_PATHS, null);
            setResult(RESULT_CANCELED, intent);
        } else {
            intent.putStringArrayListExtra(FOLDER_SELECTED_PATHS, (ArrayList<String>) selectedFolderPaths);
            setResult(RESULT_OK, intent);
        }

        finish();
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
}
