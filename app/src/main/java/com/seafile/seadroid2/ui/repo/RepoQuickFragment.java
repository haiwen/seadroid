package com.seafile.seadroid2.ui.repo;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.OptIn;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.view.ActionMode;
import androidx.appcompat.widget.SearchView;
import androidx.core.view.MenuHost;
import androidx.core.view.MenuProvider;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.media3.common.util.UnstableApi;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.github.panpf.recycler.sticky.StickyItemDecoration;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.context.GlobalNavContext;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.LayoutFastRvBinding;
import com.seafile.seadroid2.enums.ActionModeCallbackType;
import com.seafile.seadroid2.enums.FileReturnActionEnum;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.enums.RefreshStatusEnum;
import com.seafile.seadroid2.enums.SortBy;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.framework.service.PreDownloadHelper;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.TakeCameras;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetMenuAdapter;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetNewDirFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetNewRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetPasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetRenameDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.CopyMoveDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.ui.file.FileActivity;
import com.seafile.seadroid2.ui.main.MainViewModel;
import com.seafile.seadroid2.ui.markdown.MarkdownActivity;
import com.seafile.seadroid2.ui.media.image.CarouselImagePreviewActivity;
import com.seafile.seadroid2.ui.media.player.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.sdoc.SDocWebViewActivity;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;
import com.seafile.seadroid2.view.TipsViews;
import com.seafile.seadroid2.view.ViewSortPopupWindow;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.functions.Consumer;
import kotlin.ranges.IntRange;

public class RepoQuickFragment extends BaseFragmentWithVM<RepoViewModel> {

    private static final String TAG = "RepoQuickFragment";

    private static final String KEY_REPO_SCROLL_POSITION = "repo_scroll_position";
    private final int PADDING_16 = Constants.DP.DP_16;
    private final int PADDING_32 = Constants.DP.DP_32;
    private final int PADDING_128 = Constants.DP.DP_128;

    private LayoutFastRvBinding binding;
    private RepoQuickAdapter adapter;

    private MainViewModel mainViewModel;

    private final Map<String, ScrollState> scrollPositionMap = Maps.newHashMap();
    private final Map<String, Long> pathLoadTimeMap = Maps.newHashMap();
    private AppCompatActivity activity;
    private ActionMode actionMode;

    public static RepoQuickFragment newInstance() {
        Bundle args = new Bundle();
        RepoQuickFragment fragment = new RepoQuickFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        activity = (AppCompatActivity) context;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mainViewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFastRvBinding.inflate(inflater, container, false);

        binding.swipeRefreshLayout.setOnRefreshListener(() -> {
            removeScrolledPosition();
            loadData(RefreshStatusEnum.ONLY_REMOTE, false);
        });

        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        onCreateMenuHost();

        initRv();

        initAdapter();

        initViewModel();

        resetRvPadding();
    }


    @Override
    public void onFirstResume() {
        super.onFirstResume();

        loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE, true);
    }

    @Override
    public void onOtherResume() {
        super.onOtherResume();

        loadData(RefreshStatusEnum.ONLY_LOCAL, false);
    }

    private StickyItemDecoration decoration;

    public StickyItemDecoration getDecoration() {
        if (decoration != null) {
            return decoration;
        }

        decoration = new StickyItemDecoration.Builder()
                .itemType(AbsLayoutItemType.GROUP_ITEM)
                .invisibleOriginItemWhenStickyItemShowing(false)
                .disabledScrollUpStickyItem(false)
                .showInContainer(binding.stickyContainer)
                .build();
        return decoration;
    }

    private void initRv() {
        binding.rv.addItemDecoration(getDecoration());

        binding.rv.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(@NonNull RecyclerView recyclerView, int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                if (RecyclerView.SCROLL_STATE_IDLE == newState) {
                    saveScrollPosition();
                }
            }
        });

        //layout manager
        binding.rv.setLayoutManager(getGridLayoutManager());

        binding.rv.setPadding(0, 0, 0, Constants.DP.DP_32);
        binding.rv.setClipToPadding(false);
    }

    /**
     * to restore the state of Menu
     */
    private final HashMap<String, Boolean> menuIdState = new HashMap<>();

    public void onCreateMenuHost() {
        MenuHost menuHost = requireActivity();
        menuHost.addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menuInflater.inflate(R.menu.fragment_browser_menu, menu);

                //search view
                final SearchView searchView = new SearchView(requireContext());
                searchView.setSubmitButtonEnabled(false);
                if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                    searchView.setQueryHint(getString(R.string.search_in_this_library));
                } else {
                    searchView.setQueryHint(getString(R.string.search_menu_item));
                }

                searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
                    @Override
                    public boolean onQueryTextSubmit(String query) {
                        return false;
                    }

                    @Override
                    public boolean onQueryTextChange(String newText) {
                        searchView.postDelayed(new Runnable() {
                            @Override
                            public void run() {
                                search(newText);
                            }
                        }, 500);
                        return false;
                    }
                });

                //search item
                MenuItem searchMenuItem = menu.findItem(R.id.menu_action_search);
                searchMenuItem.collapseActionView();
                searchMenuItem.setActionView(searchView);
                searchMenuItem.setOnActionExpandListener(new MenuItem.OnActionExpandListener() {
                    @Override
                    public boolean onMenuItemActionExpand(@NonNull MenuItem item) {

                        // Save the state of Menu
                        menuIdState.put("search", menu.findItem(R.id.menu_action_search).isVisible());
                        menuIdState.put("sortGroup", menu.findItem(R.id.menu_action_sort).isVisible());
                        menuIdState.put("createRepo", menu.findItem(R.id.create_repo).isVisible());
                        menuIdState.put("add", menu.findItem(R.id.add).isVisible());
                        menuIdState.put("select", menu.findItem(R.id.select).isVisible());

                        // hide other menu items
                        menu.findItem(R.id.menu_action_search).setVisible(false);
                        menu.findItem(R.id.menu_action_sort).setVisible(false);
                        menu.findItem(R.id.create_repo).setVisible(false);
                        menu.findItem(R.id.add).setVisible(false);
                        menu.findItem(R.id.select).setVisible(false);

                        return true; // Return true to collapse the action view.
                    }

                    @Override
                    public boolean onMenuItemActionCollapse(@NonNull MenuItem item) {

                        menu.findItem(R.id.menu_action_search).setVisible(Boolean.TRUE.equals(menuIdState.get("search")));
                        menu.findItem(R.id.menu_action_sort).setVisible(Boolean.TRUE.equals(menuIdState.get("sortGroup")));
                        menu.findItem(R.id.create_repo).setVisible(Boolean.TRUE.equals(menuIdState.get("createRepo")));
                        menu.findItem(R.id.add).setVisible(Boolean.TRUE.equals(menuIdState.get("add")));
                        menu.findItem(R.id.select).setVisible(Boolean.TRUE.equals(menuIdState.get("select")));

                        menuHost.invalidateMenu();
                        return true; // Return true to expand the action view.
                    }
                });

                //sort pop view
                MenuItem sortMenuItem = menu.findItem(R.id.menu_action_sort);
                sortMenuItem.setActionView(R.layout.menu_view_sort);
                sortMenuItem.getActionView().setOnClickListener(v -> {
                    showCustomMenuView(v);
                });
            }

            @Override
            public void onPrepareMenu(@NonNull Menu menu) {
                MenuProvider.super.onPrepareMenu(menu);

                if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                    menu.findItem(R.id.create_repo).setVisible(false);

                    checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
                        @Override
                        public void accept(Boolean aBoolean) {
                            menu.findItem(R.id.add).setEnabled(aBoolean);
                        }
                    });
                } else {
                    menu.findItem(R.id.create_repo).setVisible(true);
                    menu.findItem(R.id.add).setVisible(false);
                }
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {

                if (menuItem.getItemId() == R.id.menu_action_search) {

                } else if (menuItem.getItemId() == R.id.menu_action_sort) {

                } else if (menuItem.getItemId() == R.id.create_repo) {
                    showNewRepoDialog();
                } else if (menuItem.getItemId() == R.id.add) {
                    showAddFileDialog();
                } else if (menuItem.getItemId() == R.id.select) {
                    startOrUpdateContextualActionBar();
                } else if (menuItem.getItemId() == android.R.id.home) {
                    backTo();
                }
                return true;
            }
        }, getViewLifecycleOwner(), Lifecycle.State.RESUMED);
    }

    private void showCustomMenuView(View anchorView) {
        ViewSortPopupWindow popupWindow = new ViewSortPopupWindow(requireContext(), GlobalNavContext.getCurrentNavContext());
        int x = -popupWindow.getW() / 2;
        popupWindow.showAsDropDown(anchorView, x, Constants.DP.DP_8);
    }

    private int SPAN_COUNT = 1;

    private GridLayoutManager getGridLayoutManager() {
        FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
        if (FileViewType.LIST == fileViewType) {
            SPAN_COUNT = 1;
        } else if (FileViewType.GRID == fileViewType) {
            SPAN_COUNT = 2;
        } else if (FileViewType.GALLERY == fileViewType) {
            SPAN_COUNT = 4;
        }

        GridLayoutManager gridLayoutManager = new GridLayoutManager(requireContext(), SPAN_COUNT);
        gridLayoutManager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(int i) {
                List<BaseModel> list = adapter.getItems();
                if (CollectionUtils.isEmpty(list)) {
                    return SPAN_COUNT;
                }

                if (list.get(i) instanceof GroupItemModel) {
                    return SPAN_COUNT;
                } else if (list.get(i) instanceof Account) {
                    return SPAN_COUNT;
                } else if (list.get(i) instanceof RepoModel) {
                    return SPAN_COUNT;
                }

                return 1;
            }
        });

        return gridLayoutManager;
    }

    private void initAdapter() {
        adapter = new RepoQuickAdapter();

        lastViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
        adapter.setFileViewType(lastViewType);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            if (i >= adapter.getItemCount()) {
                return;
            }

            BaseModel baseModel = adapter.getItems().get(i);
            if (baseModel instanceof GroupItemModel groupItemModel) {
                groupItemModel.is_expanded = !groupItemModel.is_expanded;
                adapter.set(i, groupItemModel);

                expandRepoItem(groupItemModel, i);
                return;
            }


            if (adapter.isOnActionMode()) {
                //toggle
                toggleAdapterItemSelectedState(i);

                //update bar title
                startOrUpdateContextualActionBar();

                List<BaseModel> selectedList = adapter.getSelectedList();

                if (baseModel instanceof RepoModel) {
                    List<RepoModel> selectedModels = selectedList.stream().map(b -> (RepoModel) b).collect(Collectors.toList());
                    getViewModel().inflateRepoMenuWithSelected(requireContext(), selectedModels, getDisableMenuIds(), getWillBeRemovedMenuIds());
                } else if (baseModel instanceof DirentModel) {
                    List<DirentModel> selectedModels = selectedList.stream().map(b -> (DirentModel) b).collect(Collectors.toList());
                    getViewModel().inflateDirentMenuWithSelected(requireContext(), selectedModels, getDisableMenuIds(), getWillBeRemovedMenuIds());
                }

                return;
            }

            navTo(baseModel);
        });

        adapter.setOnItemLongClickListener((baseQuickAdapter, view, i) -> {
            if (i >= adapter.getItemCount()) {
                return true;
            }

            BaseModel baseModel = adapter.getItems().get(i);
            if (baseModel instanceof GroupItemModel) {
                return true;
            } else if (baseModel instanceof SearchModel) {
                return true;
            } else if (baseModel instanceof Account) {
                return true;
            }
            //return
            if (adapter.isOnActionMode()) {
                return true;
            }

            adapter.setOnActionMode(true);

            //toggle this item
            toggleAdapterItemSelectedState(i);

            startOrUpdateContextualActionBar();

            return true;
        });

        binding.rv.setAdapter(adapter);
    }

    private final Map<String, Boolean> _groupExpandMap = new HashMap<>();

    private void expandRepoItem(GroupItemModel groupItemModel, int position) {
        if (groupItemModel.is_expanded) {
            adapter.addAll(position + 1, groupItemModel.getRepoList());
        } else {
            adapter.removeAtRange(new IntRange(position + 1, position + groupItemModel.getRepoList().size()));
        }

        _groupExpandMap.put(groupItemModel.title, groupItemModel.is_expanded);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getShowLoadingDialogLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoadingDialog(aBoolean);
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(getViewLifecycleOwner(), this::showErrorView);

        getViewModel().getStarredLiveData().observe(getViewLifecycleOwner(), aBoolean -> {

            closeActionMode();

            if (aBoolean) {
                loadData(RefreshStatusEnum.ONLY_REMOTE, false);

                // notify starred list need to change
                Bundle bundle = new Bundle();
                bundle.putBoolean(StarredQuickFragment.class.getSimpleName(), true);
                BusHelper.getCustomBundleObserver().post(bundle);
            }

        });

        getViewModel().getShowEmptyViewLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showEmptyView();
                } else {
                    disableErrorView();
                }
            }
        });
        getViewModel().getObjListLiveData().observe(getViewLifecycleOwner(), new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> models) {
                //
                notifyDataChanged(models);

                restoreScrollPosition();
            }
        });

        getViewModel().getMenuItemListLiveData().observe(getViewLifecycleOwner(), new Observer<List<MenuItem>>() {
            @Override
            public void onChanged(List<MenuItem> menuItems) {
                showBottomSheetWindow(menuItems);
            }
        });

        mainViewModel.getOnForceRefreshRepoListLiveData().observe(getViewLifecycleOwner(), aBoolean -> {
            loadData(RefreshStatusEnum.ONLY_REMOTE, false);
        });

        getViewModel().getSearchListLiveData().observe(getViewLifecycleOwner(), new Observer<List<SearchModel>>() {
            @Override
            public void onChanged(List<SearchModel> searchModels) {
                notifySearchData(searchModels);
            }
        });

        Settings.FILE_LIST_VIEW_TYPE.observe(getViewLifecycleOwner(), new Observer<FileViewType>() {
            @Override
            public void onChanged(FileViewType fileViewType) {
                switchRecyclerViewLayout(fileViewType);
            }
        });

        Settings.FILE_LIST_SORT_BY.observe(getViewLifecycleOwner(), new Observer<SortBy>() {
            @Override
            public void onChanged(SortBy sortBy) {
                loadData(RefreshStatusEnum.ONLY_LOCAL, false);
            }
        });

        Settings.FILE_LIST_SORT_ASCENDING.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                loadData(RefreshStatusEnum.ONLY_LOCAL, false);
            }
        });

        Settings.FILE_LIST_SORT_FOLDER_FIRST.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                loadData(RefreshStatusEnum.ONLY_LOCAL, false);
            }
        });

        BusHelper.getTransferProgressObserver().observe(getViewLifecycleOwner(), new Observer<Bundle>() {
            @Override
            public void onChanged(Bundle bundle) {
                doBusWork(bundle);
            }
        });

        BusHelper.getNavContextObserver().observe(getViewLifecycleOwner(), new Observer<NavContext>() {
            @Override
            public void onChanged(NavContext navContext) {
                resetRvPadding();
            }
        });
    }

    private void resetRvPadding() {
        if (GlobalNavContext.getCurrentNavContext().inRepo()) {
            int paddingTop = binding.rv.getPaddingTop();
            int paddingBottom = binding.rv.getPaddingBottom();
            if (paddingTop == 0 || paddingBottom == PADDING_128) {
                binding.rv.setPadding(0, Constants.DP.DP_16, 0, Constants.DP.DP_32);
                binding.rv.setClipToPadding(false);
            }
        } else {
            int p = binding.rv.getPaddingTop();
            if (p != 0) {
                binding.rv.setPadding(0, 0, 0, PADDING_32);
                binding.rv.setClipToPadding(false);
            }
        }
    }

    private void doBusWork(Bundle map) {
        String dataSource = map.getString(TransferWorker.KEY_DATA_SOURCE);
        String statusEvent = map.getString(TransferWorker.KEY_DATA_STATUS);
        String result = map.getString(TransferWorker.KEY_DATA_RESULT);
        String transferId = map.getString(TransferWorker.KEY_TRANSFER_ID);
        int transferCount = map.getInt(TransferWorker.KEY_TRANSFER_COUNT);

        SLogs.d(TAG, "on event: " + statusEvent + ", dataSource: " + dataSource + ", count: " + transferCount);

        if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCANNING)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCAN_FINISH)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_IN_TRANSFER)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_FAILED)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE)) {
            if (transferCount > 0) {
                loadData(RefreshStatusEnum.ONLY_REMOTE, false);
            }
        }
    }

    @Override
    public void onPause() {
        super.onPause();

        //close search view
    }

    private View floatingView;
    private BottomSheetMenuAdapter bottomSheetMenuAdapter;

    private List<Integer> getDisableMenuIds() {
        List<BaseModel> selectedList = adapter.getSelectedList();
        if (selectedList == null || selectedList.isEmpty()) {
            return null;
        }

        if (selectedList.size() == 1) {
            BaseModel baseModel = selectedList.get(0);
            if (baseModel instanceof RepoModel m) {

            } else if (baseModel instanceof DirentModel m) {
                if (m.isDir()) {
                    return CollectionUtils.newArrayList(R.id.upload);
                }
            }

            return null;
        }

        long selectedRepoModelCount = selectedList.stream().filter(f -> f instanceof RepoModel).count();
        long selectedFolderCount = selectedList.stream()
                .filter(f -> f instanceof DirentModel)
                .map(m -> (DirentModel) m)
                .filter(p -> p.isDir())
                .count();

        if (selectedRepoModelCount > 0) {
            return CollectionUtils.newArrayList(R.id.share, R.id.export, R.id.open, R.id.rename, R.id.upload, R.id.delete);
        } else if (selectedFolderCount > 0) {
            return CollectionUtils.newArrayList(R.id.share, R.id.export, R.id.open, R.id.rename, R.id.upload);
        } else {
            return CollectionUtils.newArrayList(R.id.share, R.id.export, R.id.open, R.id.rename);
        }
    }

    private List<Integer> getWillBeRemovedMenuIds() {
        List<BaseModel> selectedList = adapter.getSelectedList();

        if (CollectionUtils.isEmpty(selectedList)) {
            return CollectionUtils.newArrayList(R.id.unstar);
        }

        if (selectedList.size() == 1) {

            BaseModel baseModel = selectedList.get(0);
            if (baseModel instanceof RepoModel m) {
                return CollectionUtils.newArrayList(m.starred ? R.id.star : R.id.unstar);
            } else if (baseModel instanceof DirentModel m) {
                return CollectionUtils.newArrayList(m.starred ? R.id.star : R.id.unstar);
            }

            //remove all starred menu
            return CollectionUtils.newArrayList(R.id.star, R.id.unstar);
        }

        boolean isAllStarred = true;
        for (BaseModel baseModel : selectedList) {
            if (baseModel instanceof RepoModel m) {
                if (m.starred) {
                    continue;
                }
                isAllStarred = false;
                break;
            } else if (baseModel instanceof DirentModel m) {
                if (m.starred) {
                    continue;
                }
                isAllStarred = false;
                break;
            }
        }

        if (isAllStarred) {
            return CollectionUtils.newArrayList(R.id.star);
        } else {
            return CollectionUtils.newArrayList(R.id.unstar);
        }
    }

    private void showBottomSheetWindow(List<MenuItem> localMenuItems) {
        if (CollectionUtils.isEmpty(localMenuItems)) {
            removeFloatingView();
            return;
        }

        if (floatingView != null && floatingView.isAttachedToWindow()) {
            bottomSheetMenuAdapter.submitList(localMenuItems);
            return;
        }


        floatingView = getLayoutInflater().inflate(R.layout.layout_bottom_sheet_menu_view, null, false);

        int columnCount = 5;
        RecyclerView rv = floatingView.findViewById(R.id.rv);
        rv.setLayoutManager(new GridLayoutManager(requireContext(), columnCount));

        bottomSheetMenuAdapter = new BottomSheetMenuAdapter(columnCount);
        bottomSheetMenuAdapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<MenuItem>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<MenuItem, ?> baseQuickAdapter, @NonNull View view, int i) {
                MenuItem item = bottomSheetMenuAdapter.getItem(i);
                if (item == null) {
                    return;
                }
                if (!item.isEnabled()) {
                    return;
                }

                onBottomSheetItemClick(item);
            }
        });

        bottomSheetMenuAdapter.submitList(localMenuItems);
        rv.setAdapter(bottomSheetMenuAdapter);

        FrameLayout.LayoutParams p = new FrameLayout.LayoutParams(-1, -2);
        p.gravity = Gravity.BOTTOM;

        View decorView = requireActivity().getWindow().getDecorView();
        FrameLayout content = decorView.findViewById(android.R.id.content);
        content.addView(floatingView, p);
    }

    private void onBottomSheetItemClick(MenuItem item) {
        if (item == null) {
            return;
        }

        List<BaseModel> selectedList = adapter.getSelectedList();

        if (item.getItemId() == R.id.star) {
            getViewModel().multiStarOrNot(selectedList, true);
        } else if (item.getItemId() == R.id.unstar) {
            getViewModel().multiStarOrNot(selectedList, false);
        } else if (item.getItemId() == R.id.rename) {
            rename(selectedList);
        } else if (item.getItemId() == R.id.move) {
            RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
            String parent_dir = GlobalNavContext.getCurrentNavContext().getNavPath();

            move(repoModel.repo_id, repoModel.repo_name, parent_dir, selectedList);
        } else if (item.getItemId() == R.id.copy) {
            RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
            String parent_dir = GlobalNavContext.getCurrentNavContext().getNavPath();

            copy(repoModel.repo_id, repoModel.repo_name, parent_dir, selectedList);
        } else if (item.getItemId() == R.id.delete) {
            if (!GlobalNavContext.getCurrentNavContext().inRepo()) {
                deleteRepo(selectedList);
            } else {
                deleteDirents(selectedList);
            }
        } else if (item.getItemId() == R.id.upload) {
            addUploadTask(selectedList, true);
        } else if (item.getItemId() == R.id.download) {
            download(selectedList);
        } else if (item.getItemId() == R.id.share) {
            showShareDialog(selectedList);
        } else if (item.getItemId() == R.id.export) {
            exportFile(selectedList);
        } else if (item.getItemId() == R.id.open) {
            openWith(selectedList);
        }
    }

    private void onShowActionMode(ActionModeCallbackType actionModeType) {

        if (actionModeType == ActionModeCallbackType.CREATE) {
            if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                binding.rv.setPadding(0, Constants.DP.DP_16, 0, PADDING_128);
            } else {
                binding.rv.setPadding(0, 0, 0, PADDING_128);
            }
        } else if (actionModeType == ActionModeCallbackType.DESTROY) {
            resetRvPadding();
        }

        if (actionModeType == ActionModeCallbackType.CREATE || actionModeType == ActionModeCallbackType.SELECT_ALL) {
            if (!adapter.isOnActionMode()) {
                adapter.setOnActionMode(true);
            }

            //
            List<BaseModel> selectedList = adapter.getSelectedList();

            if (!GlobalNavContext.getCurrentNavContext().inRepo()) {
                List<RepoModel> selectedModels = selectedList.stream().map(b -> (RepoModel) b).collect(Collectors.toList());
                getViewModel().inflateRepoMenuWithSelected(requireContext(), selectedModels, getDisableMenuIds(), getWillBeRemovedMenuIds());
            } else {
                List<DirentModel> direntModels = selectedList.stream().map(baseModel -> (DirentModel) baseModel).collect(Collectors.toList());
                getViewModel().inflateDirentMenuWithSelected(requireContext(), direntModels, getDisableMenuIds(), getWillBeRemovedMenuIds());
            }

        } else if (actionModeType == ActionModeCallbackType.SELECT_NONE) {

            //
            if (!GlobalNavContext.getCurrentNavContext().inRepo()) {
                getViewModel().inflateRepoMenu(requireContext());
            } else {
                getViewModel().inflateDirentMenu(requireContext());
            }
        } else if (actionModeType == ActionModeCallbackType.DESTROY) {
            removeFloatingView();
            closeActionMode();
        } else {
            removeFloatingView();
            closeActionMode();
        }
    }

    private void removeFloatingView() {
        if (floatingView == null) {
            return;
        }

        if (!floatingView.isAttachedToWindow()) {
            floatingView = null;
            return;
        }

        View decorView = requireActivity().getWindow().getDecorView();
        FrameLayout content = decorView.findViewById(android.R.id.content);
        content.removeView(floatingView);

        floatingView = null;
        bottomSheetMenuAdapter = null;
    }

    private FileViewType lastViewType;

    private void switchRecyclerViewLayout(FileViewType newViewType) {
        int spanCount = 0;
        if (FileViewType.LIST == newViewType) {
            spanCount = 1;
        } else if (FileViewType.GRID == newViewType) {
            spanCount = 2;
        } else if (FileViewType.GALLERY == newViewType) {
            spanCount = 4;
        }

        if (spanCount == SPAN_COUNT) {
            return;
        }

        SPAN_COUNT = spanCount;

        if (FileViewType.GALLERY != lastViewType && newViewType != FileViewType.GALLERY) {
        } else {
            adapter.notifyDataChanged(null);
        }

        GridLayoutManager gridLayoutManager = (GridLayoutManager) binding.rv.getLayoutManager();
        if (gridLayoutManager != null) {
            gridLayoutManager.setSpanCount(SPAN_COUNT);
        }

        adapter.setFileViewType(newViewType);

        if (FileViewType.GALLERY != lastViewType && newViewType != FileViewType.GALLERY) {
            adapter.notifyItemRangeChanged(0, adapter.getItemCount());
        } else {
            loadData(RefreshStatusEnum.ONLY_LOCAL, false);
        }

        //If SPAN_COUNT is updated, then the data in the ScrollPosition is meaningless
        removeScrolledPositionExcludeRoot();

        lastViewType = newViewType;
    }

    public void loadData(RefreshStatusEnum refreshStatus, boolean isBlank) {
        NavContext navContext = GlobalNavContext.getCurrentNavContext();
        if (navContext.inRepo()) {
            RepoModel repoModel = navContext.getRepoModel();
            if (repoModel == null) {
                getViewModel().loadData(navContext, refreshStatus, isBlank);
            } else if (repoModel.encrypted) {
                decryptRepo(repoModel, aBoolean -> {
                    if (aBoolean) {
                        getViewModel().loadData(navContext, refreshStatus, isBlank);
                    }
                });
            } else {
                getViewModel().loadData(navContext, refreshStatus, isBlank);
            }
        } else {
            getViewModel().loadData(navContext, refreshStatus, isBlank);
        }
    }

    private void notifyDataChanged(List<BaseModel> models) {
        if (CollectionUtils.isEmpty(models)) {
            adapter.notifyDataChanged(models);
        } else {
            adapter.notifyDataChanged(checkListByGroup(models));
        }
    }

    private List<BaseModel> checkListByGroup(List<BaseModel> models) {
        List<BaseModel> newList = new ArrayList<>();
        GroupItemModel lastGroup = null;
        for (BaseModel model : models) {
            if (model instanceof GroupItemModel g) {
                if (_groupExpandMap.containsKey(g.title)) {
                    g.is_expanded = Boolean.TRUE.equals(_groupExpandMap.get(g.title));
                }
                lastGroup = g;
                newList.add(g);
            } else if (model instanceof RepoModel r) {
                if (lastGroup != null && lastGroup.is_expanded) {
                    newList.add(r);
                }
            } else {
                newList.add(model);
            }
        }
        return newList;
    }

    private void search(String keyword) {
        if (!TextUtils.isEmpty(keyword)) {
            //hide sticky view
            binding.stickyContainer.setVisibility(View.GONE);

            if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                String repo_id = GlobalNavContext.getCurrentNavContext().getRepoModel().repo_id;
                getViewModel().searchNext(repo_id, keyword, 1, 20);
            } else {
                getViewModel().searchNext(null, keyword, 1, 20);
            }
        } else {
            //show sticky view
            binding.stickyContainer.setVisibility(View.VISIBLE);

            adapter.notifySearchDataChanged(null, false);
        }
    }

    private void notifySearchData(List<SearchModel> searchModels) {
        adapter.notifySearchDataChanged(searchModels, true);
    }

    private void showEmptyView() {
        if (!NetworkUtils.isConnected()) {
            showErrorView(SeafException.NETWORK_UNAVAILABLE);
        } else {
            FileViewType type = Settings.FILE_LIST_VIEW_TYPE.queryValue();
            if (FileViewType.GALLERY == type) {
                showErrorView(R.string.no_album_type_data);
            } else if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                showErrorView(R.string.dir_empty);
            } else {
                showErrorView(R.string.no_repo);
            }
        }
    }

    private void showErrorView(SeafException seafException) {
        int strInt = !GlobalNavContext.getCurrentNavContext().inRepo() ? R.string.error_when_load_repos : R.string.error_when_load_dirents;
        showErrorView(strInt);
    }

    private void showErrorView(int textRes) {
        showErrorView(getString(textRes));
    }

    private void showErrorView(String msg) {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE, false);
            }
        });
        tipView.setText(msg);
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    private void disableErrorView() {
        adapter.setStateViewEnable(false);
        adapter.setStateView(null);
    }

    private RefreshStatusEnum getRefreshStatus() {
        String key;
        RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
        if (repoModel == null) {
            key = "/";
        } else {
            String repoId = repoModel.repo_id;
            String path = GlobalNavContext.getCurrentNavContext().getNavPath();
            key = repoId + path;
        }

        Long d = pathLoadTimeMap.getOrDefault(key, 0L);
        if (d == null || d == 0) {
            pathLoadTimeMap.put(key, System.currentTimeMillis());
            return RefreshStatusEnum.LOCAL_THEN_REMOTE;
        }

        long s = System.currentTimeMillis();
        long diff = s - d;
        if (diff < 10000) {
            return RefreshStatusEnum.ONLY_LOCAL;
        }

        pathLoadTimeMap.put(key, s);
        return RefreshStatusEnum.LOCAL_THEN_REMOTE;
    }

    private void navTo(BaseModel model) {
        //save
        if (model instanceof RepoModel repoModel) {
            if (repoModel.encrypted) {
                decryptRepo(repoModel, new java.util.function.Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) {
                        if (aBoolean) {
                            binding.stickyContainer.setVisibility(View.GONE);
                            GlobalNavContext.push(repoModel);
                            loadData(getRefreshStatus(), true);
                        }
                    }
                });
            } else {
                binding.stickyContainer.setVisibility(View.GONE);
                GlobalNavContext.push(repoModel);
                loadData(getRefreshStatus(), true);
            }

        } else if (model instanceof DirentModel direntModel) {
            if (direntModel.isDir()) {
                GlobalNavContext.push(direntModel);
                loadData(getRefreshStatus(), true);
            } else {
                RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
                open(repoModel, direntModel);
            }
        } else if (model instanceof SearchModel searchModel) {
            navToForSearch(searchModel);
        }
    }

    private void navToForSearch(SearchModel searchModel) {
        RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
        if (searchModel.isDir()) { // it is a dir, switch into it
            switchPath(searchModel.repo_id, searchModel.fullpath, searchModel.isDir());
        } else if (repoModel != null && TextUtils.equals(repoModel.repo_id, searchModel.repo_id)) {
            //it is a file in same repo, open it
            DirentModel direntModel = SearchModel.convert2DirentModel(searchModel);
            open(repoModel, direntModel);
        } else {
            //it is a file in different repo, query repo from db and open it.
            getViewModel().getRepoModelAndPermissionEntity(searchModel.repo_id, new Consumer<Pair<RepoModel, PermissionEntity>>() {
                @Override
                public void accept(Pair<RepoModel, PermissionEntity> pair) {
                    //searchModel is a file
                    DirentModel direntModel = SearchModel.convert2DirentModel(searchModel);
                    Bundle bundle = new Bundle();
                    bundle.putBoolean("load_other_images_in_same_directory", false);
                    open(pair.first, direntModel, bundle);
                }
            });
        }
    }

    /**
     * switch to special path. if isDir is false, switch to the parent directory of the fullPath
     *
     * @param repoId
     * @param fullPath
     * @param isDir
     */
    public void switchPath(String repoId, String fullPath, boolean isDir) {
        RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
        if (repoModel != null && TextUtils.equals(repoModel.repo_id, repoId)) {
            switchToPath(repoModel, fullPath, isDir);
            return;
        }

        //different repo
        getViewModel().getRepoModelEntity(repoId, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel == null) {
                    Toasts.show(R.string.repo_not_found);
                    return;
                }

                if (repoModel.encrypted) {
                    decryptRepo(repoModel, new java.util.function.Consumer<Boolean>() {
                        @Override
                        public void accept(Boolean aBoolean) {
                            if (aBoolean) {
                                switchToPath(repoModel, fullPath, isDir);
                            }
                        }
                    });
                } else {
                    switchToPath(repoModel, fullPath, isDir);
                }
            }
        });
    }

    private void switchToPath(RepoModel repoModel, String fullPath, boolean isDir) {
        if (repoModel == null) {
            return;
        }

        if (isDir && "/".equals(fullPath)) {  // it is a REPO
            GlobalNavContext.switchToPath(repoModel, "/");
        } else if (isDir) { // it is a DIR
            GlobalNavContext.switchToPath(repoModel, fullPath);
        } else {//it is a file, switch to parent path
            String p = Utils.getParentPath(fullPath);
            GlobalNavContext.switchToPath(repoModel, p);
        }
        loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE, true);
    }

    /**
     * true: can continue to back
     */
    public boolean backTo() {
        if (GlobalNavContext.getCurrentNavContext().inRepo()) {
            if (adapter == null) {
                return false;
            }

            if (adapter.isOnActionMode()) {
                adapter.setOnActionMode(false);
            } else {
                binding.swipeRefreshLayout.setRefreshing(false);

                getViewModel().disposeAll();

                removeScrolledPosition();

                GlobalNavContext.pop();

                loadData(RefreshStatusEnum.ONLY_LOCAL, true);
            }

            return true;
        }
        return false;
    }

    private void decryptRepo(RepoModel repoModel, final java.util.function.Consumer<Boolean> checkBack) {
        if (checkBack == null) {
            throw new IllegalArgumentException("checkBack is null");
        }

        if (!repoModel.encrypted) {
            checkBack.accept(true);
            return;
        }

        getViewModel().decryptRepo(repoModel.repo_id, new Consumer<String>() {
            @Override
            public void accept(String i) throws Exception {
                if (TextUtils.equals(i, "need-to-re-enter-password")) {
                    showPasswordDialogCallback(repoModel.repo_id, repoModel.repo_name, new OnResultListener<RepoModel>() {
                        @Override
                        public void onResultData(RepoModel repoModel) {
                            if (repoModel != null) {
                                checkBack.accept(true);
                            }
                        }
                    });
                } else if (TextUtils.equals(i, "done")) {
                    checkBack.accept(true);
                } else {
                    getViewModel().remoteVerify(repoModel.repo_id, i, new Consumer<ResultModel>() {
                        @Override
                        public void accept(ResultModel r) {
                            if (r.success) {
                                checkBack.accept(true);
                                return;
                            }

                            Toasts.show(r.error_msg);

                            showPasswordDialogCallback(repoModel.repo_id, repoModel.repo_name, new OnResultListener<RepoModel>() {
                                @Override
                                public void onResultData(RepoModel repoModel) {
                                    if (repoModel != null) {
                                        checkBack.accept(true);
                                    }
                                }
                            });
                        }
                    });
                }
            }
        });
    }

    private void showPasswordDialogCallback(String repo_id, String repo_name, OnResultListener<RepoModel> resultListener) {
        BottomSheetPasswordDialogFragment passwordDialogFragment = BottomSheetPasswordDialogFragment.newInstance(repo_id, repo_name);
        passwordDialogFragment.setResultListener(resultListener);
        passwordDialogFragment.show(getChildFragmentManager(), BottomSheetPasswordDialogFragment.class.getSimpleName());

//        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance(repo_id, repo_name);
//        dialogFragment.setResultListener(resultListener);
//        dialogFragment.show(getChildFragmentManager(), PasswordDialogFragment.class.getSimpleName());
    }

    private void saveScrollPosition() {
        View vi = binding.rv.getChildAt(0);
        int top = (vi == null) ? 0 : vi.getTop();

        GridLayoutManager gridLayoutManager = (GridLayoutManager) binding.rv.getLayoutManager();
        if (gridLayoutManager == null) {
            return;
        }

        final int index = gridLayoutManager.findFirstVisibleItemPosition();
        final ScrollState state = new ScrollState(index, top);
        SLogs.d(state.toString());

        removeScrolledPosition();

        if (!GlobalNavContext.getCurrentNavContext().inRepo()) {
            scrollPositionMap.put(KEY_REPO_SCROLL_POSITION, state);
        } else {
            String k = GlobalNavContext.getCurrentNavContext().getNavPath();
            scrollPositionMap.put(k, state);
        }
    }

    private void removeScrolledPositionExcludeRoot() {
        if (!scrollPositionMap.isEmpty()) {
            ScrollState rootState = scrollPositionMap.get(KEY_REPO_SCROLL_POSITION);
            scrollPositionMap.clear();
            scrollPositionMap.put(KEY_REPO_SCROLL_POSITION, rootState);
        }
    }

    private void removeScrolledPosition() {
        if (!GlobalNavContext.getCurrentNavContext().inRepo()) {
            scrollPositionMap.remove(KEY_REPO_SCROLL_POSITION);
        } else {
            String k = GlobalNavContext.getCurrentNavContext().getNavPath();
            scrollPositionMap.remove(k);
        }
    }

    private void restoreScrollPosition() {
        ScrollState state;
        if (!GlobalNavContext.getCurrentNavContext().inRepo()) {
            state = scrollPositionMap.get(KEY_REPO_SCROLL_POSITION);
        } else {
            state = scrollPositionMap.get(GlobalNavContext.getCurrentNavContext().getNavPath());
        }

        GridLayoutManager gridLayoutManager = (GridLayoutManager) binding.rv.getLayoutManager();
        if (gridLayoutManager == null) {
            return;
        }

        if (null == state) {
            gridLayoutManager.scrollToPosition(0);
        } else {
            gridLayoutManager.scrollToPositionWithOffset(state.index, state.top);
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();

        removeFloatingView();
        closeActionMode();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    private void toggleAdapterItemSelectedState(int i) {
        BaseModel baseModel = adapter.getItems().get(i);
        if (baseModel instanceof RepoModel repoModel) {
            repoModel.is_checked = !repoModel.is_checked;
            adapter.set(i, repoModel);
        } else if (baseModel instanceof DirentModel direntModel) {
            direntModel.is_checked = !direntModel.is_checked;
            adapter.set(i, direntModel);
        }
    }

    public void closeActionMode() {
        if (adapter.isOnActionMode()) {
            adapter.setOnActionMode(false);
        }

        if (actionMode != null) {
            actionMode.finish();
            actionMode = null;
            customView = null;
        }
    }


    /**
     * start or update state of contextual action bar (CAB)
     */
    public void startOrUpdateContextualActionBar() {
        if (actionMode == null) {
            // there are some selected items, start the actionMode
            actionMode = activity.startSupportActionMode(new ActionModeCallback());
        }

        if (customView != null) {
            int count = adapter.getSelectedList().size();
            TextView textView = customView.findViewById(R.id.title);
            textView.setText(getResources().getQuantityString(R.plurals.transfer_list_items_selected, count, count));
        }
    }

    private View customView;

    /**
     * Represents a contextual mode of the user interface.
     * Action modes can be used to provide alternative interaction modes and replace parts of the normal UI until finished.
     * A Callback configures and handles events raised by a user's interaction with an action mode.
     */
    private final class ActionModeCallback implements ActionMode.Callback {
        private boolean allItemsSelected = false;

        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            LayoutInflater inflater = LayoutInflater.from(requireContext());
            customView = inflater.inflate(R.layout.view_toolbar_action_mode, null);
            mode.setCustomView(customView);

            if (adapter == null) return true;

            LinearLayout checkLayout = customView.findViewById(R.id.check_container);
            checkLayout.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onChecked();
                }
            });

            onShowActionMode(ActionModeCallbackType.CREATE);
            return true;
        }

        private void onChecked() {
            if (customView == null) {
                return;
            }

            ImageView checkImageView = customView.findViewById(R.id.check_box);

            allItemsSelected = !allItemsSelected;

            adapter.setAllItemSelected(allItemsSelected);

            if (!allItemsSelected) {
                checkImageView.setImageResource(R.drawable.ic_checkbox_unchecked);
                onShowActionMode(ActionModeCallbackType.SELECT_NONE);
            } else {
                checkImageView.setImageResource(R.drawable.ic_checkbox_checked);
                onShowActionMode(ActionModeCallbackType.SELECT_ALL);
            }

            startOrUpdateContextualActionBar();
        }

        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            return false;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            return false;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            if (adapter == null) return;

            customView = null;
            onShowActionMode(ActionModeCallbackType.DESTROY);
        }

    }


    /************ Files ************/

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        return DataManager.getLocalFileCachePath(account, repoId, repoName, fullPathInRepo);
    }

    private void open(RepoModel repoModel, DirentModel dirent) {
        open(repoModel, dirent, null);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void open(RepoModel repoModel, DirentModel dirent, Bundle extras) {
        String fileName = dirent.name;
        String filePath = dirent.full_path;

        if (Utils.isViewableImage(fileName) && repoModel != null) {
            boolean load_other_images_in_same_directory = true;
            if (extras != null) {
                load_other_images_in_same_directory = extras.getBoolean("load_other_images_in_same_directory");
            }

            Intent getIntent = CarouselImagePreviewActivity.startThisFromObjs(requireContext(), dirent, load_other_images_in_same_directory);
            imagePreviewActivityLauncher.launch(getIntent);

            return;
        }

        if (fileName.endsWith(Constants.Format.DOT_SDOC)) {
            SDocWebViewActivity.openSdoc(getContext(), repoModel.repo_name, repoModel.repo_id, dirent.parent_dir + dirent.name);

            return;
        }

        if (Utils.isVideoFile(fileName)) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
            builder.setItems(R.array.video_download_array, (dialog, which) -> {
                if (which == 0) {
                    CustomExoVideoPlayerActivity.startThis(getContext(), fileName, repoModel.repo_id, filePath, dirent.id);
                } else if (which == 1) {
                    Intent intent = FileActivity.start(requireContext(), dirent, FileReturnActionEnum.DOWNLOAD_VIDEO);
                    fileActivityLauncher.launch(intent);
                }
            }).show();

            return;
        }

        if (Utils.isTextMimeType(fileName)) {
            File local = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);
            if (TextUtils.equals(dirent.id, dirent.local_file_id) && local.exists()) {
                MarkdownActivity.start(requireContext(), local.getAbsolutePath(), dirent.repo_id, dirent.full_path);
            } else {
                Intent intent = FileActivity.start(requireContext(), dirent, FileReturnActionEnum.OPEN_TEXT_MIME);
                fileActivityLauncher.launch(intent);
            }

            return;
        }

        //Open with another app
        openWith(CollectionUtils.newArrayList(dirent));
    }

    private void openWith(List<BaseModel> direntModels) {
        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        closeActionMode();

        DirentModel dirent = (DirentModel) direntModels.get(0);

        File local = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);
        if (TextUtils.equals(dirent.id, dirent.local_file_id) && local.exists()) {
            WidgetUtils.openWith(requireActivity(), local);
        } else {
            Intent intent = FileActivity.start(requireActivity(), dirent, FileReturnActionEnum.OPEN_WITH);
            fileActivityLauncher.launch(intent);
        }
    }

    public void download(List<BaseModel> direntModels) {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_unavailable);
            return;
        }

        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        closeActionMode();

        List<DirentModel> direntModels1 = direntModels.stream().map(m -> (DirentModel) m).collect(Collectors.toList());
        List<String> uids = direntModels1.stream().map(m -> m.uid).collect(Collectors.toList());
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        PreDownloadHelper.preDownload(requireContext(), account, uids);
    }

    public void rename(List<BaseModel> models) {
        closeActionMode();

        if (CollectionUtils.isEmpty(models)) {
            return;
        }

        BottomSheetRenameDialogFragment dialogFragment;

        BaseModel first = models.get(0);
        if (first instanceof DirentModel dirent) {
            dialogFragment = BottomSheetRenameDialogFragment.newInstance(dirent.name, dirent.full_path, dirent.repo_id, dirent.repo_name, dirent.type);
        } else if (first instanceof RepoModel repo) {
            dialogFragment = BottomSheetRenameDialogFragment.newInstance(repo.repo_name, repo.repo_id, "repo");
        } else {
            return;
        }

        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    Toasts.show(R.string.rename_successful);
                }

                loadData(RefreshStatusEnum.ONLY_REMOTE, false);
            }
        });
        dialogFragment.show(getChildFragmentManager(), BottomSheetRenameDialogFragment.class.getSimpleName());
    }

    public void deleteRepo(List<BaseModel> repoModels) {
        if (CollectionUtils.isEmpty(repoModels)) {
            return;
        }

        List<String> repoIds = repoModels.stream().map(m -> (RepoModel) m).map(m -> m.repo_id).collect(Collectors.toList());
        DeleteRepoDialogFragment dialogFragment = DeleteRepoDialogFragment.newInstance(repoIds);
        dialogFragment.setRefreshListener(isDone -> {
            if (isDone) {
                Toasts.show(R.string.delete_successful);
            }

            closeActionMode();
            loadData(RefreshStatusEnum.ONLY_REMOTE, false);
        });
        dialogFragment.show(getChildFragmentManager(), DeleteRepoDialogFragment.class.getSimpleName());
    }

    public void deleteDirents(List<BaseModel> dirents) {
        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        List<String> direntUids = dirents.stream().map(m -> (DirentModel) m).map(m -> m.uid).collect(Collectors.toList());
        DeleteFileDialogFragment dialogFragment = DeleteFileDialogFragment.newInstance(direntUids);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    Toasts.show(R.string.delete_successful);
                }

                closeActionMode();

                loadData(RefreshStatusEnum.ONLY_REMOTE, false);
            }
        });
        dialogFragment.show(getChildFragmentManager(), DeleteFileDialogFragment.class.getSimpleName());
    }

    /**
     * Share a file. Generating a file share link and send the link or file to someone
     * through some app.
     */
    public void showShareDialog(List<BaseModel> dirents) {
        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        //close action mode firstly
        closeActionMode();

        DirentModel direntModel = (DirentModel) dirents.get(0);

        MaterialAlertDialogBuilder mBuilder = new MaterialAlertDialogBuilder(requireContext());

        boolean inChina = Utils.isInChina();
        String[] strings;

        //if user in China, system add WeChat share
        if (inChina) {
            strings = getResources().getStringArray(R.array.file_action_share_array_zh);
        } else {
            strings = getResources().getStringArray(R.array.file_action_share_array);
        }

        mBuilder.setItems(strings, (dialog, which) -> {
            if (!inChina) {
                which++;
            }

            if (which == 0) {
                shareFile(direntModel);
            } else if (which == 1) {
                Objs.showCreateShareLinkDialog(requireContext(), getChildFragmentManager(), direntModel, false);
            } else if (which == 2) {
                Objs.showCreateShareLinkDialog(requireContext(), getChildFragmentManager(), direntModel, true);
            }
        }).show();
    }

    private void exportFile(List<BaseModel> dirents) {
        closeActionMode();

        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        DirentModel dirent = (DirentModel) dirents.get(0);

        File local = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);
        if (TextUtils.equals(dirent.id, dirent.local_file_id) && local.exists()) {
            WidgetUtils.openWith(requireActivity(), local);
        } else {
            Intent intent = FileActivity.start(requireActivity(), dirent, FileReturnActionEnum.EXPORT);
            fileActivityLauncher.launch(intent);
        }
    }


    /**
     * Copy multiple files
     */
    public void copy(String srcRepoId, String srcRepoName, String srcDir, List<BaseModel> dirents) {
        chooseCopyMoveDestForMultiFiles(srcRepoId, srcRepoName, srcDir, dirents, OpType.COPY);
    }


    /**
     * Move multiple files
     */
    public void move(String srcRepoId, String srcRepoName, String srcDir, List<BaseModel> dirents) {
        chooseCopyMoveDestForMultiFiles(srcRepoId, srcRepoName, srcDir, dirents, OpType.MOVE);
    }

    private CopyMoveContext copyMoveContext = null;

    /**
     * Choose copy/move destination for multiple files
     */
    private void chooseCopyMoveDestForMultiFiles(String repoID, String repoName,
                                                 String dirPath, List<BaseModel> dirents,
                                                 OpType op) {
        closeActionMode();

        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        List<DirentModel> direntModels = dirents.stream().map(m -> (DirentModel) m).collect(Collectors.toList());

        copyMoveContext = new CopyMoveContext(repoID, repoName, dirPath, direntModels, op);

        //launch obj selector activity
        Intent intent = ObjSelectorActivity.getCurrentAccountIntent(requireContext(), ObjSelectType.REPO, ObjSelectType.DIR);
        copyMoveLauncher.launch(intent);
    }

    private final ActivityResultLauncher<Intent> copyMoveLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK || o.getData() == null) {
                return;
            }

            String dstRepoId = o.getData().getStringExtra(ObjKey.REPO_ID);
            String dstDir = o.getData().getStringExtra(ObjKey.DIR);
            String disRepoName = o.getData().getStringExtra(ObjKey.REPO_NAME);

            copyMoveContext.setDest(dstRepoId, dstDir, disRepoName);

            doCopyMove();
        }
    });

    private void doCopyMove() {
        if (copyMoveContext == null) {
            return;
        }

        if (!copyMoveContext.checkCopyMoveToSubfolder()) {
            Toasts.show(copyMoveContext.isCopy()
                    ? R.string.cannot_copy_folder_to_subfolder
                    : R.string.cannot_move_folder_to_subfolder);
            return;
        }

        CopyMoveDialogFragment dialogFragment = CopyMoveDialogFragment.newInstance();
        dialogFragment.initData(copyMoveContext);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    Toasts.show(copyMoveContext.isCopy() ? R.string.copied_successfully : R.string.moved_successfully);
                }

                loadData(RefreshStatusEnum.ONLY_REMOTE, false);
            }
        });
        dialogFragment.show(getChildFragmentManager(), CopyMoveDialogFragment.class.getSimpleName());
    }


    private final ActivityResultLauncher<Intent> imagePreviewActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                return;
            }

            loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE, false);
        }
    });

    private final ActivityResultLauncher<Intent> fileActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                loadData(RefreshStatusEnum.ONLY_REMOTE, false);
                return;
            }

            Intent data = o.getData();
            if (o.getData() == null) {
                return;
            }

            String action = data.getStringExtra("action");
            String repoId = data.getStringExtra("repo_id");
            String targetFile = data.getStringExtra("target_file");
            String localFullPath = data.getStringExtra("destination_path");
            boolean isUpdateWhenFileExists = data.getBooleanExtra("is_update", false);

            if (TextUtils.isEmpty(localFullPath)) {
                return;
            }

            if (isUpdateWhenFileExists) {
                Toasts.show(R.string.download_finished);
            }

            loadData(RefreshStatusEnum.ONLY_REMOTE, false);

            File destinationFile = new File(localFullPath);
            if (TextUtils.equals(FileReturnActionEnum.EXPORT.name(), action)) {

                Objs.exportFile(RepoQuickFragment.this, destinationFile);
            } else if (TextUtils.equals(FileReturnActionEnum.SHARE.name(), action)) {

                Objs.shareFileToWeChat(RepoQuickFragment.this, destinationFile);
            } else if (TextUtils.equals(FileReturnActionEnum.DOWNLOAD_VIDEO.name(), action)) {

            } else if (TextUtils.equals(FileReturnActionEnum.OPEN_WITH.name(), action)) {

                WidgetUtils.openWith(requireContext(), destinationFile);

            } else if (TextUtils.equals(FileReturnActionEnum.OPEN_TEXT_MIME.name(), action)) {

                MarkdownActivity.start(requireContext(), localFullPath, repoId, targetFile);
            }
        }
    });

    private void shareFile(DirentModel dirent) {
        if (dirent.isDir()) {
            Objs.shareDirToWeChat(this, dirent.repo_id, dirent.full_path);
            return;
        }

        File local = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);
        if (TextUtils.equals(dirent.id, dirent.local_file_id) && local.exists()) {
            WidgetUtils.openWith(requireActivity(), local);
        } else {
            Intent intent = FileActivity.start(requireActivity(), dirent, FileReturnActionEnum.SHARE);
            fileActivityLauncher.launch(intent);
        }
    }


    /**
     * re-upload the local downloaded files
     */
    private void addUploadTask(List<BaseModel> dirents, boolean isReplace) {
        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        List<DirentModel> direntModels = dirents.stream().map(m -> (DirentModel) m).collect(Collectors.toList());

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        RepoModel targetRepoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
        String targetDir = GlobalNavContext.getCurrentNavContext().getNavPath();

        List<File> lf = new ArrayList<>();
        for (DirentModel dirent : direntModels) {
            if (dirent.isDir()) {
                continue;
            }

            File localFilePath = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);
            if (localFilePath.exists()) {
                lf.add(localFilePath);
            }
        }

        if (!CollectionUtils.isEmpty(lf)) {
            for (File file : lf) {
                mainViewModel.addUploadTask(requireContext(), account, targetRepoModel, file.getAbsolutePath(), targetDir, isReplace);
            }

            Toasts.show(R.string.added_to_upload_tasks);
            // 启动上传
            TransferService.startManualUploadService(requireContext());
//            Intent uploadIntent = new Intent(requireContext(), FileUploadService.class);
//            ContextCompat.startForegroundService(requireContext(), uploadIntent);
//            BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
        }

        closeActionMode();

    }


    /**
     * create a new repo
     */
    private void showNewRepoDialog() {
        BottomSheetNewRepoDialogFragment bottomSheetNewRepoDialogFragment = new BottomSheetNewRepoDialogFragment();
        bottomSheetNewRepoDialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                }
            }
        });
        bottomSheetNewRepoDialogFragment.show(getChildFragmentManager(), BottomSheetNewRepoDialogFragment.class.getSimpleName());

    }

    /**
     * add new file/files
     */
    private void showAddFileDialog() {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
        builder.setTitle(getString(R.string.add_file));
        builder.setItems(R.array.add_file_options_array, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (which == 0) // create file
                    showNewFileDialog();
                else if (which == 1) // create folder
                    showNewDirDialog();
                else if (which == 2) // upload file
                    pickFile();
                else if (which == 3) // take a photo
                    takePhoto();
            }
        }).show();
    }

    private void checkCurrentPathHasWritePermission(java.util.function.Consumer<Boolean> consumer) {
        BaseModel baseModel = GlobalNavContext.getCurrentNavContext().getTopModel();
        if (null == baseModel) {
            return;
        }

        if (baseModel instanceof RepoModel m) {
            if (!m.isCustomPermission()) {
                consumer.accept(m.hasWritePermission());
            } else {
                mainViewModel.getPermissionFromLocal(m.repo_id, m.getCustomPermissionNum(), entity -> {
                    if (entity == null) {
                        consumer.accept(false);
                        return;
                    }
                    consumer.accept(entity.create);
                });
            }
        } else if (baseModel instanceof DirentModel m) {
            if (!m.isCustomPermission()) {
                consumer.accept(m.hasWritePermission());
            } else {
                mainViewModel.getPermissionFromLocal(m.repo_id, m.getCustomPermissionNum(), entity -> {
                    if (entity == null) {
                        consumer.accept(false);
                        return;
                    }
                    consumer.accept(entity.create);
                });
            }
        }
    }


    //
    private void showNewDirDialog() {
        checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (!aBoolean) {
                    Toasts.show(R.string.library_read_only);
                    return;
                }

                String rid = GlobalNavContext.getCurrentNavContext().getRepoModel().repo_id;
                String parentPath = GlobalNavContext.getCurrentNavContext().getNavPath();

                BottomSheetNewDirFileDialogFragment sheetDialog = BottomSheetNewDirFileDialogFragment.newInstance(rid, parentPath, true);
                sheetDialog.setRefreshListener(new OnRefreshDataListener() {
                    @Override
                    public void onActionStatus(boolean isDone) {
                        if (isDone) {
                            mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                        }
                    }
                });
                sheetDialog.show(getChildFragmentManager(), BottomSheetNewDirFileDialogFragment.class.getSimpleName());
            }
        });
    }

    private void showNewFileDialog() {
        checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (!aBoolean) {
                    Toasts.show(R.string.library_read_only);
                    return;
                }


                String rid = GlobalNavContext.getCurrentNavContext().getRepoModel().repo_id;
                String parentPath = GlobalNavContext.getCurrentNavContext().getNavPath();

                BottomSheetNewDirFileDialogFragment sheetDialog = BottomSheetNewDirFileDialogFragment.newInstance(rid, parentPath, false);
                sheetDialog.setRefreshListener(new OnRefreshDataListener() {
                    @Override
                    public void onActionStatus(boolean isDone) {
                        if (isDone) {
                            mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                        }
                    }
                });
                sheetDialog.show(getChildFragmentManager(), BottomSheetNewDirFileDialogFragment.class.getSimpleName());
            }
        });
    }

    private void pickFile() {
        checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (!aBoolean) {
                    Toasts.show(R.string.library_read_only);
                    return;
                }

                takeFile(false);
            }
        });

    }

    //0 camera
    //1 video
    private int permission_media_select_type = -1;

    private void takePhoto() {
        checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (!aBoolean) {
                    Toasts.show(R.string.library_read_only);
                    return;
                }

                permission_media_select_type = 0;
                cameraPermissionLauncher.launch(Manifest.permission.CAMERA);
            }
        });

    }

    private void takeVideo() {
        checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (!aBoolean) {
                    Toasts.show(R.string.library_read_only);
                    return;
                }

                permission_media_select_type = 1;
                cameraPermissionLauncher.launch(Manifest.permission.CAMERA);
            }
        });

    }


    private void takeFile(boolean isSingleSelect) {
        String[] mimeTypes = new String[]{"*/*"};
        if (isSingleSelect) {
            singleFileAndImageChooseLauncher.launch(mimeTypes);
        } else {
            multiFileAndImageChooserLauncher.launch(mimeTypes);
        }
    }

    private final ActivityResultLauncher<String> cameraPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean result) {
            if (Boolean.FALSE.equals(result)) {
                Toasts.show(R.string.permission_camera);
                return;
            }

            if (permission_media_select_type == 0) {
                uriPair = TakeCameras.buildPhotoUri(requireContext());
                takePhotoLauncher.launch(uriPair.getFirst());
            } else if (permission_media_select_type == 1) {
                uriPair = TakeCameras.buildVideoUri(requireContext());
                takePhotoLauncher.launch(uriPair.getFirst());
            }
        }
    });

    private final ActivityResultLauncher<String[]> singleFileAndImageChooseLauncher = registerForActivityResult(new ActivityResultContracts.OpenDocument(), new ActivityResultCallback<Uri>() {
        @Override
        public void onActivityResult(Uri o) {
            if (null == o) {
                return;
            }

            doSelectSingleFile(o);
        }
    });

    private final ActivityResultLauncher<String[]> multiFileAndImageChooserLauncher = registerForActivityResult(new ActivityResultContracts.OpenMultipleDocuments(), new ActivityResultCallback<List<Uri>>() {
        @Override
        public void onActivityResult(List<Uri> o) {
            if (CollectionUtils.isEmpty(o)) {
                return;
            }

            for (Uri uri : o) {
                int takeFlags = Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION;
                requireContext().getContentResolver().takePersistableUriPermission(uri, takeFlags);
            }

            if (o.size() == 1) {
                doSelectSingleFile(o.get(0));
            } else {
                doSelectedMultiFile(o);
            }
        }
    });


    private kotlin.Pair<Uri, File> uriPair;
    private final ActivityResultLauncher<Uri> takePhotoLauncher = registerForActivityResult(new ActivityResultContracts.TakePicture(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean result) {
            if (Boolean.FALSE.equals(result)) {
                return;
            }

            SLogs.d("take photo");

            if (uriPair == null) {
                return;
            }

            Uri uri = uriPair.getFirst();
            File file = uriPair.getSecond();

            RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();

            addUploadTask(repoModel, GlobalNavContext.getCurrentNavContext().getNavPath(), file.getAbsolutePath());
        }
    });

    private final ActivityResultLauncher<Uri> takeVideoLauncher = registerForActivityResult(new ActivityResultContracts.CaptureVideo(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean o) {
            if (!o) {
                return;
            }

            SLogs.d("take video");
        }
    });

    private void doSelectedMultiFile(List<Uri> uriList) {
        showLoadingDialog();
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
        String parent_dir = GlobalNavContext.getCurrentNavContext().getNavPath();
        mainViewModel.multipleCheckRemoteDirent(requireContext(), account, repoModel.repo_id, repoModel.repo_name, parent_dir, uriList, new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                dismissLoadingDialog();

                if (aBoolean) {
                    Toasts.show(R.string.added_to_upload_tasks);

                    //start worker
//                    BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
                    TransferService.startManualUploadService(requireContext());
//                    Intent uploadIntent = new Intent(requireContext(), FileUploadService.class);
//                    ContextCompat.startForegroundService(requireContext(), uploadIntent);
                }
            }
        });
    }

    private void doSelectSingleFile(Uri uri) {
        if (uri == null) {
            return;
        }

        String appCacheUriPrefix = "content://" + requireContext().getPackageName() + ".documents";
        if (uri.toString().startsWith(appCacheUriPrefix)) {
            return;
        }

        showLoadingDialog();

        String fileName = Utils.getFilenameFromUri(requireContext(), uri);
        String parent_dir = GlobalNavContext.getCurrentNavContext().getNavPath();
        String destinationPath = Utils.pathJoin(parent_dir, fileName);

        RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
        mainViewModel.checkRemoteDirent(repoModel.repo_id, destinationPath, new java.util.function.Consumer<DirentFileModel>() {
            @Override
            public void accept(DirentFileModel direntFileModel) {
                if (direntFileModel != null) {
                    showFileExistDialog(uri, fileName);
                } else {
                    addUploadTask(repoModel, GlobalNavContext.getCurrentNavContext().getNavPath(), uri, fileName, false);
                }

                dismissLoadingDialog();
            }
        });
    }

    private void showFileExistDialog(final Uri uri, String fileName) {

        RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();

        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
        builder.setTitle(getString(R.string.upload_file_exist));
        builder.setMessage(String.format(getString(R.string.upload_duplicate_found), fileName));

        builder.setPositiveButton(getString(R.string.upload_replace), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                addUploadTask(repoModel, GlobalNavContext.getCurrentNavContext().getNavPath(), uri, fileName, true);
            }
        });

        builder.setNeutralButton(getString(R.string.cancel), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                dialog.dismiss();
            }
        });

        builder.setNegativeButton(getString(R.string.upload_keep_both), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                addUploadTask(repoModel, GlobalNavContext.getCurrentNavContext().getNavPath(), uri, fileName, false);
            }
        });

        builder.show();
    }

    // task
    private void addUploadTask(RepoModel repoModel, String targetDir, String localFile) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        mainViewModel.addUploadTask(requireContext(), account, repoModel, localFile, targetDir, false);

        Toasts.show(R.string.added_to_upload_tasks);
//        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
//        Intent uploadIntent = new Intent(requireContext(), FileUploadService.class);
//        ContextCompat.startForegroundService(requireContext(), uploadIntent);
        TransferService.startManualUploadService(requireContext());
    }

    private void addUploadTask(RepoModel repoModel, String targetDir, Uri sourceUri, String fileName, boolean isReplace) {
        //
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        mainViewModel.addUploadTask(requireContext(), account, repoModel, sourceUri, targetDir, fileName, isReplace);

        Toasts.show(R.string.added_to_upload_tasks);
//        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
//        Intent uploadIntent = new Intent(requireContext(), FileUploadService.class);
//        ContextCompat.startForegroundService(requireContext(), uploadIntent);
        TransferService.startManualUploadService(requireContext());
    }
}
