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
import com.blankj.utilcode.util.ToastUtils;
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
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.context.GlobalNavContext;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.LayoutFastRvBinding;
import com.seafile.seadroid2.enums.ActionModeCallbackType;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.enums.RefreshStatusEnum;
import com.seafile.seadroid2.enums.SortBy;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.TakeCameras;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetMenuAdapter;
import com.seafile.seadroid2.ui.dialog_fragment.CopyMoveDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.NewDirFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.NewRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.RenameDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.ui.file.FileActivity;
import com.seafile.seadroid2.ui.main.MainViewModel;
import com.seafile.seadroid2.ui.markdown.MarkdownActivity;
import com.seafile.seadroid2.ui.media.image_preview2.CarouselImagePreviewActivity;
import com.seafile.seadroid2.ui.media.player.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.sdoc.SDocWebViewActivity;
import com.seafile.seadroid2.ui.search.SearchViewModel;
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
    private static final String KEY_REPO_SCROLL_POSITION = "repo_scroll_position";
    private static final String KEY_REPO_LIST = "key_repo_list";

    private LayoutFastRvBinding binding;
    private RepoQuickAdapter adapter;

    private MainViewModel mainViewModel;
    private SearchViewModel searchViewModel;

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
        searchViewModel = new ViewModelProvider(this).get(SearchViewModel.class);
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFastRvBinding.inflate(inflater, container, false);

        binding.swipeRefreshLayout.setOnRefreshListener(() -> {
            removeScrolledPosition();
            loadData(RefreshStatusEnum.ONLY_REMOTE);
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
    }


    @Override
    public void onFirstResume() {
        super.onFirstResume();

        loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
    }

    @Override
    public void onOtherResume() {
        super.onOtherResume();

        loadData(RefreshStatusEnum.ONLY_LOCAL);
    }

    private void initRv() {
        StickyItemDecoration decoration = new StickyItemDecoration.Builder()
                .itemType(AbsLayoutItemType.GROUP_ITEM)
                .invisibleOriginItemWhenStickyItemShowing(false)
                .disabledScrollUpStickyItem(false)
                .showInContainer(binding.stickyContainer)
                .build();

        binding.rv.addItemDecoration(decoration);
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

                //search
                MenuItem searchMenuItem = menu.findItem(R.id.menu_action_search);
                SearchView searchView = new SearchView(requireContext());
                searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
                    @Override
                    public boolean onQueryTextSubmit(String query) {
                        return false;
                    }

                    @Override
                    public boolean onQueryTextChange(String newText) {
                        mainViewModel.getOnSearchLiveData().setValue(newText);
                        return false;
                    }
                });
                searchMenuItem.collapseActionView();
                searchMenuItem.setActionView(searchView);
                searchMenuItem.setOnActionExpandListener(new MenuItem.OnActionExpandListener() {
                    @Override
                    public boolean onMenuItemActionExpand(@NonNull MenuItem item) {
                        menuIdState.put("sortGroup", menu.findItem(R.id.menu_action_sort).isVisible());
                        menuIdState.put("createRepo", menu.findItem(R.id.create_repo).isVisible());
                        menuIdState.put("add", menu.findItem(R.id.add).isVisible());
                        menuIdState.put("select", menu.findItem(R.id.select).isVisible());

                        menu.findItem(R.id.menu_action_sort).setVisible(false);
                        menu.findItem(R.id.create_repo).setVisible(false);
                        menu.findItem(R.id.add).setVisible(false);
                        menu.findItem(R.id.select).setVisible(false);

                        return true;
                    }

                    @Override
                    public boolean onMenuItemActionCollapse(@NonNull MenuItem item) {

                        menu.findItem(R.id.menu_action_sort).setVisible(Boolean.TRUE.equals(menuIdState.get("sortGroup")));
                        menu.findItem(R.id.create_repo).setVisible(Boolean.TRUE.equals(menuIdState.get("createRepo")));
                        menu.findItem(R.id.add).setVisible(Boolean.TRUE.equals(menuIdState.get("add")));
                        menu.findItem(R.id.select).setVisible(Boolean.TRUE.equals(menuIdState.get("select")));

                        menuHost.invalidateMenu();
                        return true;
                    }
                });

                //sort
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
                }
                return true;
            }
        }, getViewLifecycleOwner(), Lifecycle.State.RESUMED);
    }

    private void showCustomMenuView(View anchorView) {
        ViewSortPopupWindow popupWindow = new ViewSortPopupWindow(requireContext());
        int x = -popupWindow.getW() / 3 * 2;
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
            if (aBoolean) {
                loadData(RefreshStatusEnum.ONLY_REMOTE);
            }

            closeActionMode();

            // notify starred list need to change
            Bundle bundle = new Bundle();
            bundle.putBoolean(StarredQuickFragment.class.getSimpleName(), true);
            BusHelper.getCustomBundleObserver().post(bundle);
        });

        getViewModel().getObjListLiveData().observe(getViewLifecycleOwner(), models -> {
            notifyDataChanged(models);

            restoreScrollPosition();
        });

        getViewModel().getMenuItemListLiveData().observe(getViewLifecycleOwner(), new Observer<List<MenuItem>>() {
            @Override
            public void onChanged(List<MenuItem> menuItems) {
                showBottomSheetWindow(menuItems);
            }
        });

        mainViewModel.getOnShowRefreshLoadingInRepoLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        mainViewModel.getOnForceRefreshRepoListLiveData().observe(getViewLifecycleOwner(), aBoolean -> {
            loadData(RefreshStatusEnum.ONLY_REMOTE);
        });


        mainViewModel.getOnSearchLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                search(s);
            }
        });

        searchViewModel.getSearchListLiveData().observe(getViewLifecycleOwner(), new Observer<List<SearchModel>>() {
            @Override
            public void onChanged(List<SearchModel> searchModels) {
                deduplicateSearchData(searchModels);
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
                loadData(RefreshStatusEnum.ONLY_LOCAL);
            }
        });

        Settings.FILE_LIST_SORT_ASCENDING.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                loadData(RefreshStatusEnum.ONLY_LOCAL);
            }
        });

        Settings.FILE_LIST_SORT_FOLDER_FIRST.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                loadData(RefreshStatusEnum.ONLY_LOCAL);
            }
        });

        BusHelper.getTransferProgressObserver().observe(getViewLifecycleOwner(), new Observer<Bundle>() {
            @Override
            public void onChanged(Bundle bundle) {
                doBusWork(bundle);
            }
        });
    }

    private void doBusWork(Bundle map) {
        String dataSource = map.getString(TransferWorker.KEY_DATA_SOURCE);
        String statusEvent = map.getString(TransferWorker.KEY_DATA_STATUS);
        String result = map.getString(TransferWorker.KEY_DATA_RESULT);
        String transferId = map.getString(TransferWorker.KEY_TRANSFER_ID);
        int transferCount = map.getInt(TransferWorker.KEY_TRANSFER_COUNT);

        SLogs.d("repo fragment, event: " + statusEvent + ", dataSource: " + dataSource + ", count: " + transferCount);

        if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCANNING)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCAN_FINISH)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_IN_TRANSFER)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_FAILED)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_TRANSFER_FINISH)) {
            if (transferCount > 0) {
                loadData(RefreshStatusEnum.ONLY_REMOTE);
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
            int p = Constants.DP.DP_32 * 4;
            binding.rv.setPadding(0, 0, 0, p);
        } else if (actionModeType == ActionModeCallbackType.DESTROY) {
            int p = 0;
            binding.rv.setPadding(0, 0, 0, p);
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
            loadData(RefreshStatusEnum.ONLY_LOCAL);
        }

        //If SPAN_COUNT is updated, then the data in the ScrollPosition is meaningless
        removeScrolledPositionExcludeRoot();

        lastViewType = newViewType;
    }

    public void loadData(RefreshStatusEnum refreshStatus) {
        NavContext navContext = GlobalNavContext.getCurrentNavContext();
        if (navContext.inRepo()) {
            RepoModel repoModel = navContext.getRepoModel();
            if (repoModel == null) {
                getViewModel().loadData(navContext, refreshStatus);
            } else if (repoModel.encrypted) {
                decryptRepo(repoModel, aBoolean -> {
                    if (aBoolean) {
                        getViewModel().loadData(navContext, refreshStatus);
                    }
                });
            } else {
                getViewModel().loadData(navContext, refreshStatus);
            }
        } else {
            getViewModel().loadData(navContext, refreshStatus);
        }
    }

    private void notifyDataChanged(List<BaseModel> models) {
        if (CollectionUtils.isEmpty(models)) {
            showEmptyTip();
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
        adapter.filterListBySearchKeyword(keyword);

        //
        searchViewModel.searchNext(keyword, 1, 20);
    }

    private void deduplicateSearchData(List<SearchModel> searchModels) {
        //todo deduplicate
        adapter.addAll(searchModels);
    }

    private void showEmptyTip() {
        FileViewType type = Settings.FILE_LIST_VIEW_TYPE.queryValue();
        if (FileViewType.GALLERY == type) {
            showErrorView(R.string.no_album_type_data);
        } else if (GlobalNavContext.getCurrentNavContext().inRepo()) {
            showErrorView(R.string.dir_empty);
        } else {
            showErrorView(R.string.no_repo);
        }
    }

    private void showErrorView(SeafException seafException) {

        String errorMsg = seafException.getMessage();
        ToastUtils.showLong(errorMsg);

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
                loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
            }
        });
        tipView.setText(msg);
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
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

        long diff = System.currentTimeMillis() - d;
        if (diff < 5000) {
            return RefreshStatusEnum.ONLY_LOCAL;
        }

        pathLoadTimeMap.put(key, System.currentTimeMillis());
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
                            GlobalNavContext.push(repoModel);
                            loadData(getRefreshStatus());
                        }
                    }
                });
            } else {
                GlobalNavContext.push(repoModel);
                loadData(getRefreshStatus());
            }

        } else if (model instanceof DirentModel direntModel) {
            if (direntModel.isDir()) {
                GlobalNavContext.push(direntModel);
                loadData(getRefreshStatus());
            } else {
                RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
                open(repoModel, direntModel);
            }
        } else if (model instanceof SearchModel searchModel) {
            navToForSearch(searchModel);
        }
    }

    private void navToForSearch(SearchModel searchModel) {
        //searchModel is a dir and repo
        if (searchModel.isDir() && "/".equals(searchModel.fullpath)) {
            getViewModel().getRepoModelEntity(searchModel.repo_id, new Consumer<RepoModel>() {
                @Override
                public void accept(RepoModel repoModel) throws Exception {
                    if (repoModel == null) {
                        ToastUtils.showLong(R.string.repo_not_found);
                        return;
                    }

                    GlobalNavContext.getCurrentNavContext().push(repoModel);
                    loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
                }
            });
        } else {
            //searchModel is a dir, and child path
            RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
            if (repoModel != null && TextUtils.equals(repoModel.repo_id, searchModel.repo_id)) {
                if (searchModel.isDir()) {
                    //TODO sss
                    GlobalNavContext.switchToPath(GlobalNavContext.getCurrentNavContext().getRepoModel(), searchModel.fullpath);
                    loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
                } else {
                    //searchModel is a file
                    DirentModel direntModel = SearchModel.convert2DirentModel(searchModel);
                    open(repoModel, direntModel);
                }
            } else {
                getViewModel().getRepoModelAndPermissionEntity(searchModel.repo_id, new Consumer<Pair<RepoModel, PermissionEntity>>() {
                    @Override
                    public void accept(Pair<RepoModel, PermissionEntity> pair) {
                        if (searchModel.isDir()) {
                            GlobalNavContext.switchToPath(pair.first, searchModel.fullpath);
                            loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
                        } else {
                            //searchModel is a file
                            DirentModel direntModel = SearchModel.convert2DirentModel(searchModel);
                            Bundle bundle = new Bundle();
                            bundle.putBoolean("load_other_images_in_same_directory", false);
                            open(pair.first, direntModel, bundle);
                        }
                    }
                });
            }
        }
    }

    /**
     * true: can continue to back
     */
    public boolean backTo() {
        if (GlobalNavContext.getCurrentNavContext().inRepo()) {
            if (adapter.isOnActionMode()) {
                adapter.setOnActionMode(false);
            } else {

                getViewModel().disposeAll();

                removeScrolledPosition();

                GlobalNavContext.pop();

                loadData(RefreshStatusEnum.ONLY_LOCAL);
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

                            ToastUtils.showLong(r.error_msg);

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
        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance(repo_id, repo_name);
        dialogFragment.setResultListener(resultListener);
        dialogFragment.show(getChildFragmentManager(), PasswordDialogFragment.class.getSimpleName());
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
                    CustomExoVideoPlayerActivity.startThis(getContext(), fileName, repoModel.repo_id, filePath);
                } else if (which == 1) {
                    Intent intent = FileActivity.start(requireContext(), dirent, "video_download");
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
                Intent intent = FileActivity.start(requireContext(), dirent, "open_text_mime");
                fileActivityLauncher.launch(intent);
            }

            return;
        }

        //Open with another app
        openWith(CollectionUtils.newArrayList(dirent));
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        return DataManager.getLocalRepoFile(account, repoId, repoName, fullPathInRepo);
    }

    private void openWith(List<BaseModel> direntModels) {
        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        closeActionMode();

        DirentModel direntModel = (DirentModel) direntModels.get(0);

        File local = getLocalDestinationFile(direntModel.repo_id, direntModel.repo_name, direntModel.full_path);
        if (local.exists()) {
            WidgetUtils.openWith(requireActivity(), local);
        } else {
            Intent intent = FileActivity.start(requireActivity(), direntModel, "open_with");
            fileActivityLauncher.launch(intent);
        }
    }

    public void download(List<BaseModel> direntModels) {
        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        List<DirentModel> direntModels1 = direntModels.stream().map(m -> (DirentModel) m).collect(Collectors.toList());
        List<String> uids = direntModels1.stream().map(m -> m.uid).collect(Collectors.toList());
        BackgroundJobManagerImpl.getInstance().startDownloadChain(uids);

        closeActionMode();
    }

    public void rename(List<BaseModel> models) {
        closeActionMode();

        if (CollectionUtils.isEmpty(models)) {
            return;
        }

        RenameDialogFragment dialogFragment;

        BaseModel first = models.get(0);
        if (first instanceof DirentModel dirent) {
            dialogFragment = RenameDialogFragment.newInstance(dirent.name, dirent.full_path, dirent.repo_id, dirent.repo_name, dirent.type);
        } else if (first instanceof RepoModel repo) {
            dialogFragment = RenameDialogFragment.newInstance(repo.repo_name, repo.repo_id, "repo");
        } else {
            return;
        }

        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    ToastUtils.showLong(R.string.rename_successful);
                }

                loadData(RefreshStatusEnum.ONLY_REMOTE);
            }
        });
        dialogFragment.show(getChildFragmentManager(), RenameDialogFragment.class.getSimpleName());
    }

    public void deleteRepo(List<BaseModel> repoModels) {
        if (CollectionUtils.isEmpty(repoModels)) {
            return;
        }

        List<String> repoIds = repoModels.stream().map(m -> (RepoModel) m).map(m -> m.repo_id).collect(Collectors.toList());
        DeleteRepoDialogFragment dialogFragment = DeleteRepoDialogFragment.newInstance(repoIds);
        dialogFragment.setRefreshListener(isDone -> {
            if (isDone) {
                ToastUtils.showLong(R.string.delete_successful);
            }

            closeActionMode();
            loadData(RefreshStatusEnum.ONLY_REMOTE);
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
                    ToastUtils.showLong(R.string.delete_successful);
                }

                closeActionMode();

                loadData(RefreshStatusEnum.ONLY_REMOTE);
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
        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        DirentModel direntModel = (DirentModel) dirents.get(0);

        File destinationFile = getLocalDestinationFile(direntModel.repo_id, direntModel.repo_name, direntModel.full_path);

        if (!destinationFile.exists()) {
            Intent intent = FileActivity.start(requireContext(), direntModel, "export");
            fileActivityLauncher.launch(intent);
        } else {
            Objs.exportFile(this, destinationFile);
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

        Intent intent = new Intent(requireContext(), ObjSelectorActivity.class);
        intent.putExtra(ObjSelectorActivity.DATA_ACCOUNT, SupportAccountManager.getInstance().getCurrentAccount());
        copyMoveLauncher.launch(intent);
    }

    private final ActivityResultLauncher<Intent> copyMoveLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK || o.getData() == null) {
                return;
            }

            String dstRepoId = o.getData().getStringExtra(ObjSelectorActivity.DATA_REPO_ID);
            String dstDir = o.getData().getStringExtra(ObjSelectorActivity.DATA_DIR);
            String disRepoName = o.getData().getStringExtra(ObjSelectorActivity.DATA_REPO_NAME);

            copyMoveContext.setDest(dstRepoId, dstDir, disRepoName);

            doCopyMove();
        }
    });

    private void doCopyMove() {
        if (copyMoveContext == null) {
            return;
        }

        if (!copyMoveContext.checkCopyMoveToSubfolder()) {
            ToastUtils.showLong(copyMoveContext.isCopy()
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
                    ToastUtils.showLong(copyMoveContext.isCopy() ? R.string.copied_successfully : R.string.moved_successfully);
                }

                loadData(RefreshStatusEnum.ONLY_REMOTE);
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

//            loadData(RefreshStatusEnum.REMOTE);
        }
    });

    private final ActivityResultLauncher<Intent> fileActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                loadData(RefreshStatusEnum.ONLY_REMOTE);
                return;
            }

            String action = o.getData().getStringExtra("action");
            String repoId = o.getData().getStringExtra("repo_id");
            String targetFile = o.getData().getStringExtra("target_file");
            String localFullPath = o.getData().getStringExtra("destination_path");
            boolean isUpdateWhenFileExists = o.getData().getBooleanExtra("is_update", false);

            if (TextUtils.isEmpty(localFullPath)) {
                return;
            }

            if (isUpdateWhenFileExists) {
                ToastUtils.showLong(R.string.download_finished);
            }

            loadData(RefreshStatusEnum.ONLY_REMOTE);

            File destinationFile = new File(localFullPath);
            if ("export".equals(action)) {

                Objs.exportFile(RepoQuickFragment.this, destinationFile);
            } else if ("share".equals(action)) {

                Objs.shareFileToWeChat(RepoQuickFragment.this, destinationFile);
            } else if ("video_download".equals(action)) {

            } else if ("open_with".equals(action)) {

                WidgetUtils.openWith(requireActivity(), destinationFile);

            } else if ("open_text_mime".equals(action)) {

                MarkdownActivity.start(requireActivity(), localFullPath, repoId, targetFile);
            }
        }
    });

    private void shareFile(DirentModel dirent) {
        if (dirent.isDir()) {
            Objs.shareDirToWeChat(this, dirent.repo_id, dirent.full_path);
            return;
        }

        File destinationFile = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);
        if (destinationFile.exists()) {
            Objs.shareFileToWeChat(this, destinationFile);
        } else {
            Intent intent = FileActivity.start(requireContext(), dirent, "share");
            fileActivityLauncher.launch(intent);
        }
    }


    ////////////////add task/////////////
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
            File localFilePath = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);
            if (localFilePath.exists()) {
                lf.add(localFilePath);
            }
        }

        if (!CollectionUtils.isEmpty(lf)) {
            ToastUtils.showLong(R.string.added_to_upload_tasks);

            for (File file : lf) {
                mainViewModel.addUploadTask(requireContext(), account, targetRepoModel, file.getAbsolutePath(), targetDir, isReplace);
            }

            BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
        }

        closeActionMode();

    }


    /**
     * create a new repo
     */
    private void showNewRepoDialog() {
        NewRepoDialogFragment dialogFragment = new NewRepoDialogFragment();
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
//                    mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), NewRepoDialogFragment.class.getSimpleName());
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
                    ToastUtils.showLong(R.string.library_read_only);
                    return;
                }

                String rid = GlobalNavContext.getCurrentNavContext().getRepoModel().repo_id;
                String parentPath = GlobalNavContext.getCurrentNavContext().getNavPath();
                NewDirFileDialogFragment dialogFragment = NewDirFileDialogFragment.newInstance(rid, parentPath, true);
                dialogFragment.setRefreshListener(new OnRefreshDataListener() {
                    @Override
                    public void onActionStatus(boolean isDone) {
                        if (isDone) {
                            mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                        }
                    }
                });
                dialogFragment.show(getChildFragmentManager(), NewDirFileDialogFragment.class.getSimpleName());
            }
        });
    }

    private void showNewFileDialog() {
        checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (!aBoolean) {
                    ToastUtils.showLong(R.string.library_read_only);
                    return;
                }


                String rid = GlobalNavContext.getCurrentNavContext().getRepoModel().repo_id;
                String parentPath = GlobalNavContext.getCurrentNavContext().getNavPath();
                NewDirFileDialogFragment dialogFragment = NewDirFileDialogFragment.newInstance(rid, parentPath, false);
                dialogFragment.setRefreshListener(new OnRefreshDataListener() {
                    @Override
                    public void onActionStatus(boolean isDone) {
                        if (isDone) {
                            mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                        }
                    }
                });
                dialogFragment.show(getChildFragmentManager(), NewDirFileDialogFragment.class.getSimpleName());
            }
        });
    }

    private void pickFile() {
        checkCurrentPathHasWritePermission(new java.util.function.Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (!aBoolean) {
                    ToastUtils.showLong(R.string.library_read_only);
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
                    ToastUtils.showLong(R.string.library_read_only);
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
                    ToastUtils.showLong(R.string.library_read_only);
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
                ToastUtils.showLong(R.string.permission_camera);
                return;
            }

            if (permission_media_select_type == 0) {
                uriPair = TakeCameras.buildTakePhotoUri(requireContext());
                takePhotoLauncher.launch(uriPair.getFirst());
            } else if (permission_media_select_type == 1) {
                uriPair = TakeCameras.buildTakeVideoUri(requireContext());
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
                    ToastUtils.showLong(R.string.added_to_upload_tasks);

                    //start worker
                    BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
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

        ToastUtils.showLong(R.string.added_to_upload_tasks);
        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
    }

    private void addUploadTask(RepoModel repoModel, String targetDir, Uri sourceUri, String fileName, boolean isReplace) {
        //
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        mainViewModel.addUploadTask(requireContext(), account, repoModel, sourceUri, targetDir, fileName, isReplace);

        ToastUtils.showLong(R.string.added_to_upload_tasks);
        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();

    }
}
