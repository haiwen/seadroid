package com.seafile.seadroid2.ui.repo;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
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
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.media3.common.util.UnstableApi;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.work.Data;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.github.panpf.recycler.sticky.StickyItemDecoration;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.ActionModeCallbackType;
import com.seafile.seadroid2.framework.data.model.repo.RepoPermissionWrapper;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetMenuAdapter;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.LayoutFastRvBinding;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.enums.SortBy;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.GroupItemModel;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadFileManuallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadMediaFileAutomaticallyWorker;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.CopyMoveDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.RenameDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.ui.file.FileActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.main.MainViewModel;
import com.seafile.seadroid2.ui.markdown.MarkdownActivity;
import com.seafile.seadroid2.ui.media.image_preview2.CarouselImagePreviewActivity;
import com.seafile.seadroid2.ui.media.player.exoplayer.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.sdoc.SDocWebViewActivity;
import com.seafile.seadroid2.ui.search.SearchViewModel;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.view.TipsViews;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.functions.Consumer;

public class RepoQuickFragment extends BaseFragmentWithVM<RepoViewModel> {
    private static final String KEY_REPO_SCROLL_POSITION = "repo_scroll_position";
    private static final String KEY_REPO_LIST = "key_in_repo_list";

    private final HashMap<String, Long> mRefreshStatusExpireTimeMap = new HashMap<>();
    private final HashMap<String, Long> mPermissionRefreshStatusExpireTimeMap = new HashMap<>();

    private LayoutFastRvBinding binding;
    private RepoQuickAdapter adapter;

    private MainViewModel mainViewModel;
    private SearchViewModel searchViewModel;

    private final Map<String, ScrollState> scrollPositions = Maps.newHashMap();
    private AppCompatActivity activity;
    private ActionMode actionMode;

    public static RepoQuickFragment newInstance() {
        Bundle args = new Bundle();
        RepoQuickFragment fragment = new RepoQuickFragment();
        fragment.setArguments(args);
        return fragment;
    }

    private NavContext getNavContext() {
        return mainViewModel.getNavContext();
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
            loadData(RefreshStatusEnum.LOCAL_BEFORE_REMOTE);
        });

        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initAdapter();

        initRv();

        initViewModel();

        initWorkerListener();
    }


    @Override
    public void onFirstResume() {
        super.onFirstResume();

        restoreNavContext();

        loadData(isForce(true));
    }

    private void restoreNavContext() {
        NavContext navContext = getNavContext();
        navContext.restoreNavContextFromSp();
        mainViewModel.getOnNavContextChangeListenerLiveData().setValue(true);
    }


    @Override
    public void onOtherResume() {
        super.onOtherResume();

        loadData(isForce(false));

    }

    private void initRv() {
        StickyItemDecoration decoration = new StickyItemDecoration.Builder()
                .itemType(AbsLayoutItemType.GROUP_ITEM)
//                .invisibleOriginItemWhenStickyItemShowing(false)
//                .disabledScrollUpStickyItem(false)
//                .showInContainer(binding.stickyContainer)
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
            if (adapter.isOnActionMode()) {
                //toggle
                toggleAdapterItemSelectedState(i);

                //update bar title
                startOrUpdateContextualActionBar();

                BaseModel baseModel = adapter.getItems().get(i);
                List<BaseModel> selected = adapter.getSelectedList();
                if (baseModel instanceof RepoModel m) {
                    if (CollectionUtils.isEmpty(selected)) {
                        getViewModel().inflateRepoMenu(requireContext());
                    } else {
                        getViewModel().inflateRepoMenuWithParams(requireContext(), m, getDisableMenuIds(), getWillBeRemovedMenuIds(), isPermissionForce(m.repo_id));
                    }
                } else if (baseModel instanceof DirentModel m) {
                    if (CollectionUtils.isEmpty(selected)) {
                        getViewModel().inflateDirentMenu(requireContext());
                    } else {
                        getViewModel().inflateDirentMenuWithParams(requireContext(), CollectionUtils.newArrayList(m), m.is_checked, getDisableMenuIds(), getWillBeRemovedMenuIds(), isPermissionForce(m.repo_id));
                    }
                }

                return;
            }

            BaseModel baseModel = adapter.getItems().get(i);
            if (baseModel instanceof RepoModel repoModel) {
                if (repoModel.encrypted) {
                    decrypt(repoModel);
                } else {
                    navTo(baseModel);
                }
            } else if (baseModel instanceof DirentModel) {
                navTo(baseModel);
            } else if (baseModel instanceof SearchModel) {
                navTo(baseModel);
            }
        });

        adapter.setOnItemLongClickListener((baseQuickAdapter, view, i) -> {
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

                MainActivity mainActivity = (MainActivity) getActivity();
                if (mainActivity != null) {
                    mainActivity.showProgressDialog(aBoolean);
                }
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(getViewLifecycleOwner(), this::showErrorView);

        getViewModel().getStarredLiveData().observe(getViewLifecycleOwner(), aBoolean -> {
            if (aBoolean) {
                loadData(RefreshStatusEnum.REMOTE);
            }

            closeActionMode();

            SettingsManager.setForceRefreshStarredListState(true);
        });

        getViewModel().getObjListLiveData().observe(getViewLifecycleOwner(), repoModels -> {

            //notify navContext changed
            mainViewModel.getOnNavContextChangeListenerLiveData().setValue(true);

            notifyDataChanged(repoModels);

            restoreScrollPosition();
        });

        getViewModel().getMenuItemListLiveData().observe(getViewLifecycleOwner(), new Observer<List<MenuItem>>() {
            @Override
            public void onChanged(List<MenuItem> menuItems) {
                showBottomSheetWindow(menuItems);
            }
        });


        mainViewModel.getOnResortListLiveData().observe(getViewLifecycleOwner(), a -> {
            loadDataAsFirst();
        });

        mainViewModel.getOnForceRefreshRepoListLiveData().observe(getViewLifecycleOwner(), aBoolean -> {
            loadData(RefreshStatusEnum.REMOTE);
        });

        mainViewModel.getOnSearchLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                search(s);
            }
        });

        mainViewModel.getOnActionModeLiveData().observe(getViewLifecycleOwner(), new Observer<ActionModeCallbackType>() {
            @Override
            public void onChanged(ActionModeCallbackType callbackType) {
                onShowActionMode(callbackType);
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
            RepoModel repoModel = getNavContext().getRepoModel();
            String parent_dir = getNavContext().getNavPath();

            move(repoModel.repo_id, repoModel.repo_name, parent_dir, selectedList);
        } else if (item.getItemId() == R.id.copy) {
            RepoModel repoModel = getNavContext().getRepoModel();
            String parent_dir = getNavContext().getNavPath();

            copy(repoModel.repo_id, repoModel.repo_name, parent_dir, selectedList);
        } else if (item.getItemId() == R.id.delete) {
            if (!getNavContext().inRepo()) {
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

    private void onShowActionMode(ActionModeCallbackType callbackType) {

        if (callbackType == ActionModeCallbackType.CREATE) {
            int p = Constants.DP.DP_32 * 4;
            binding.rv.setPadding(0, 0, 0, p);
        } else if (callbackType == ActionModeCallbackType.DESTORY) {
            int p = 0;
            binding.rv.setPadding(0, 0, 0, p);
        }

        if (callbackType == ActionModeCallbackType.CREATE) {
            if (!adapter.isOnActionMode()) {
                adapter.setOnActionMode(true);
            }

            //select repo list
            List<BaseModel> models = adapter.getSelectedList();
            if (!getNavContext().inRepo()) {
                if (CollectionUtils.isEmpty(models)) {
                    //click the select item of MenuItem
                    getViewModel().inflateRepoMenu(requireContext());
                } else {
                    //When press and hold to select some list item, only one can be selected
                    RepoModel repoModel = (RepoModel) models.get(0);
                    getViewModel().inflateRepoMenuWithParams(requireContext(), repoModel, getDisableMenuIds(), getWillBeRemovedMenuIds(), isPermissionForce(repoModel.repo_id));
                }
            } else {
                if (CollectionUtils.isEmpty(models)) {
                    getViewModel().inflateDirentMenu(requireContext());
                } else {
                    DirentModel direntModel = (DirentModel) models.get(0);
                    getViewModel().inflateDirentMenuWithParams(requireContext(), CollectionUtils.newArrayList(direntModel), true, getDisableMenuIds(), getWillBeRemovedMenuIds(), isPermissionForce(direntModel.repo_id));
                }
            }
        } else if (callbackType == ActionModeCallbackType.SELECT_ALL) {
            //
            List<BaseModel> baseModels = adapter.getSelectedList();

            if (!getNavContext().inRepo()) {
                List<RepoModel> repoModels = baseModels.stream().map(baseModel -> (RepoModel) baseModel).collect(Collectors.toList());
                getViewModel().inflateRepoMenuWithParams(requireContext(), repoModels, true, getDisableMenuIds(), getWillBeRemovedMenuIds(), false);
            } else {
                List<DirentModel> direntModels = baseModels.stream().map(baseModel -> (DirentModel) baseModel).collect(Collectors.toList());
                getViewModel().inflateDirentMenuWithParams(requireContext(), direntModels, true, getDisableMenuIds(), getWillBeRemovedMenuIds(), false);
            }


        } else if (callbackType == ActionModeCallbackType.SELECT_NONE) {
            //clear menu permission list
            getViewModel().clearCachePermissionMap();

            //
            if (!getNavContext().inRepo()) {
                getViewModel().inflateRepoMenu(requireContext());
            } else {
                getViewModel().inflateDirentMenu(requireContext());
            }
        } else if (callbackType == ActionModeCallbackType.DESTORY) {
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

        //clear permission list cache
        getViewModel().clearCachePermissionMap();


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
            loadDataAsFirst();
        }

        //If SPAN_COUNT is updated, then the data in the ScrollPosition is meaningless
        removeScrolledPositionExcludeRoot();

        lastViewType = newViewType;
    }

    private void initWorkerListener() {

        //UploadFileManuallyWorker
        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadFileManuallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkWorkInfo(workInfo);
                    }
                });

        //UploadFolderFileAutomaticallyWorker
        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadFolderFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkWorkInfo(workInfo);
                    }
                });

        //UploadMediaFileAutomaticallyWorker
        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadMediaFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkWorkInfo(workInfo);
                    }
                });

        //DownloadWorker
        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(DownloadWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkWorkInfo(workInfo);
                    }
                });
    }

    private void checkWorkInfo(WorkInfo workInfo) {
        if (!isResumed()) {
            return;
        }

        if (workInfo != null && workInfo.getState().isFinished()) {
            Data data = workInfo.getOutputData();
            String transferState = data.getString(TransferWorker.KEY_DATA_EVENT);
//            int pendingTransferCount = data.getInt(TransferWorker.KEY_DATA_PARAM, -1);
            String dataType = data.getString(TransferWorker.KEY_DATA_TYPE);
            SLogs.e(dataType + " -> " + transferState);
            if (TransferEvent.EVENT_FINISH.equals(transferState)) {
                loadData(isForce());
            }
        }
    }

    public void clearExpireRefreshMap() {
        mRefreshStatusExpireTimeMap.clear();
    }


    private final long THROTTLE_FORCE_ALL_MS = 1000 * 60 * 5; // 5秒
    private final long THROTTLE_FORCE_LOCAL_MS = 5000; // 5秒

    private RefreshStatusEnum isForce() {
        return isForce(false);
    }

    /**
     * It will not be refreshed within 2 seconds,
     * only local data will be refreshed within 5 seconds,
     * and be forced to refresh in other cases
     */
    private RefreshStatusEnum isForce(boolean isFirst) {
        if (isFirst) {
            return RefreshStatusEnum.LOCAL_BEFORE_REMOTE;
        }

        long now = TimeUtils.getNowMills();
        Long lastMills;
        if (!getNavContext().inRepo()) {
            lastMills = mRefreshStatusExpireTimeMap.get(KEY_REPO_LIST);
        } else {
            String k = getNavContext().getRepoModel().repo_id + getNavContext().getNavPath();
            lastMills = mRefreshStatusExpireTimeMap.get(k);
        }

        if (lastMills == null) {
            return RefreshStatusEnum.REMOTE;
        }

        if (now - lastMills > THROTTLE_FORCE_ALL_MS) {
            return RefreshStatusEnum.REMOTE;
        }

        if (now - lastMills > THROTTLE_FORCE_LOCAL_MS) {
            return RefreshStatusEnum.REMOTE;
        }

        return RefreshStatusEnum.NO;
    }

    private boolean isPermissionForce(String repoId) {
        long now = TimeUtils.getNowMills();
        Long lastMills = mPermissionRefreshStatusExpireTimeMap.get(repoId);

        if (lastMills == null) {
            mPermissionRefreshStatusExpireTimeMap.put(repoId, now);
            return true;
        }

        if (now - lastMills > THROTTLE_FORCE_ALL_MS) {
            mPermissionRefreshStatusExpireTimeMap.put(repoId, now);
            return true;
        }

        if (now - lastMills > THROTTLE_FORCE_LOCAL_MS) {
            return false;
        }
        return false;
    }

    public void loadDataAsFirst() {
        loadData(RefreshStatusEnum.LOCAL_BEFORE_REMOTE);
    }

    public void loadData(RefreshStatusEnum refreshStatus) {
        SLogs.e("loadData -> " + refreshStatus);
        if (refreshStatus.ordinal() >= RefreshStatusEnum.LOCAL_BEFORE_REMOTE.ordinal()) {
            long now = TimeUtils.getNowMills();
            String key;
            if (getNavContext().inRepo()) {
                key = getNavContext().getRepoModel().repo_id + getNavContext().getNavPath();
            } else {
                key = KEY_REPO_LIST;
            }
            mRefreshStatusExpireTimeMap.put(key, now);

            //force refresh permission list
            mPermissionRefreshStatusExpireTimeMap.clear();
        }

        getViewModel().loadData(getNavContext(), refreshStatus);
    }

    private void notifyDataChanged(List<BaseModel> repoModels) {
        if (CollectionUtils.isEmpty(repoModels)) {
            showEmptyTip();
        } else {
            adapter.notifyDataChanged(repoModels);
        }
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
        } else if (getNavContext().inRepo()) {
            showErrorView(R.string.dir_empty);
        } else {
            showErrorView(R.string.no_repo);
        }
    }

    private void showErrorView(SeafException seafException) {

        String errorMsg = seafException.getMessage();
        ToastUtils.showLong(errorMsg);

        int strInt = !getNavContext().inRepo() ? R.string.error_when_load_repos : R.string.error_when_load_dirents;
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
                loadData(RefreshStatusEnum.LOCAL_BEFORE_REMOTE);
            }
        });
        tipView.setText(msg);
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    /**
     * TODO improve: go into the folder first, and then load the data
     */
    private void navTo(BaseModel model) {
        //save
        if (model instanceof RepoModel model1) {
            getNavContext().push(model1);

            loadDataAsFirst();
        } else if (model instanceof DirentModel direntModel) {
            if (direntModel.isDir()) {
                getNavContext().push(direntModel);
                loadDataAsFirst();
            } else {
                open(direntModel);
            }
        } else if (model instanceof SearchModel searchModel) {

            DirentModel direntModel = SearchModel.convert2DirentModel(searchModel);
            if (direntModel.isDir()) {
                String repoId = getNavContext().getRepoModel().repo_id;

                //switch to special path in special repo
                if (TextUtils.equals(repoId, direntModel.repo_id)) {
                    getNavContext().switchToPath(getNavContext().getRepoModel(), searchModel.fullpath);
                    loadDataAsFirst();
                } else {
                    getViewModel().getRepoModelAndPermissionEntity(searchModel.repo_id, isPermissionForce(searchModel.repo_id), new Consumer<RepoPermissionWrapper>() {
                        @Override
                        public void accept(RepoPermissionWrapper wrapper) throws Exception {
                            getNavContext().switchToPath(wrapper.repoModel, searchModel.fullpath);
                            loadDataAsFirst();
                        }
                    });
                }
            } else {
                open(direntModel);
            }
        }
    }

    public boolean backTo() {
        if (getNavContext().inRepo()) {
            if (adapter.isOnActionMode()) {
                adapter.setOnActionMode(false);
            } else {
                //
                removeScrolledPosition();

                //
                getNavContext().pop();

                //there may be some issues: no data in some cases.
                loadData(RefreshStatusEnum.ONLY_LOCAL);

                //notify navContext changed
                mainViewModel.getOnNavContextChangeListenerLiveData().setValue(true);
            }
            return true;
        }
        return false;
    }

    private void decrypt(RepoModel repoModel) {
        getViewModel().getEncCacheDB(repoModel.repo_id, new Consumer<EncKeyCacheEntity>() {
            @Override
            public void accept(EncKeyCacheEntity encKeyCacheEntity) throws Exception {
                if (encKeyCacheEntity == null) {
                    showPasswordDialog(repoModel);
                    return;
                }

                long now = TimeUtils.getNowMills();
                if (encKeyCacheEntity.expire_time_long == 0) {
                    showPasswordDialog(repoModel);
                } else if (now < encKeyCacheEntity.expire_time_long) {
                    navTo(repoModel);
                } else {
                    showPasswordDialog(repoModel);
                }
            }
        });
    }

    private void showPasswordDialog(RepoModel repoModel) {
        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance();
        dialogFragment.initData(repoModel.repo_id, repoModel.repo_name);
        dialogFragment.setResultListener(new OnResultListener<RepoModel>() {
            @Override
            public void onResultData(RepoModel uRepoModel) {
                navTo(uRepoModel);
            }
        });

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

        if (!getNavContext().inRepo()) {
            scrollPositions.put(KEY_REPO_SCROLL_POSITION, state);
        } else {
            String k = getNavContext().getNavPath();
            scrollPositions.put(k, state);
        }
    }

    private void removeScrolledPositionExcludeRoot() {
        if (!scrollPositions.isEmpty()) {
            ScrollState rootState = scrollPositions.get(KEY_REPO_SCROLL_POSITION);
            scrollPositions.clear();
            scrollPositions.put(KEY_REPO_SCROLL_POSITION, rootState);
        }
    }

    private void removeScrolledPosition() {
        if (!getNavContext().inRepo()) {
            scrollPositions.remove(KEY_REPO_SCROLL_POSITION);
        } else {
            String k = getNavContext().getNavPath();
            scrollPositions.remove(k);
        }
    }

    private void restoreScrollPosition() {
        ScrollState state;
        if (!getNavContext().inRepo()) {
            state = scrollPositions.get(KEY_REPO_SCROLL_POSITION);
        } else {
            state = scrollPositions.get(getNavContext().getNavPath());
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

        int count = adapter.getSelectedList().size();
        actionMode.setTitle(getResources().getQuantityString(R.plurals.transfer_list_items_selected, count, count));
    }

    /**
     * Represents a contextual mode of the user interface.
     * Action modes can be used to provide alternative interaction modes and replace parts of the normal UI until finished.
     * A Callback configures and handles events raised by a user's interaction with an action mode.
     */
    private final class ActionModeCallback implements ActionMode.Callback {
        private boolean allItemsSelected;

        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            // Inflate the menu for the contextual action bar (CAB)
            MenuInflater inflater = mode.getMenuInflater();
            inflater.inflate(R.menu.repos_fragment_menu, menu);
            if (adapter == null) return true;

            mainViewModel.getOnActionModeLiveData().setValue(ActionModeCallbackType.CREATE);

//            // to hidden  "r" permissions  files or folder
//            if (!getNavContext().isParentHasWritePermission()) {
//                menu.findItem(R.id.action_mode_delete).setVisible(false);
//                menu.findItem(R.id.action_mode_move).setVisible(false);
//            }
            return true;
        }

        @SuppressLint("NewApi")
        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            /*
             * The ActionBarPolicy determines how many action button to place in the ActionBar
             * and the default amount is 2.
             */
//            menu.findItem(R.id.action_mode_delete).setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
//            menu.findItem(R.id.action_mode_copy).setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
            menu.findItem(R.id.action_mode_select_all).setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

            // Here you can perform updates to the contextual action bar (CAB) due to
            // an invalidate() request
            return true;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            //check data
            List<BaseModel> selectedDirents = adapter.getSelectedList();
            if (CollectionUtils.isEmpty(selectedDirents) || !getNavContext().inRepo()) {
                if (item.getItemId() != R.id.action_mode_select_all) {
                    ToastUtils.showLong(R.string.action_mode_no_items_selected);
                    return true;
                }
            }

            int itemId = item.getItemId();
            if (itemId == R.id.action_mode_select_all) {
                adapter.setItemSelected(!allItemsSelected);

                startOrUpdateContextualActionBar();

                if (!allItemsSelected) {
                    mainViewModel.getOnActionModeLiveData().setValue(ActionModeCallbackType.SELECT_ALL);
                } else {
                    mainViewModel.getOnActionModeLiveData().setValue(ActionModeCallbackType.SELECT_NONE);
                }

                allItemsSelected = !allItemsSelected;
            }


            return true;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            if (adapter == null) return;

            mainViewModel.getOnActionModeLiveData().setValue(ActionModeCallbackType.DESTORY);
        }

    }


    /************ Files ************/

    @OptIn(markerClass = UnstableApi.class)
    private void open(DirentModel dirent) {
        String fileName = dirent.name;
        String filePath = dirent.full_path;

        RepoModel repoModel = getNavContext().getRepoModel();

        // Encrypted repo does not support gallery,
        // because pic thumbnail under encrypted repo was not supported at the server side
        if (Utils.isViewableImage(fileName) && !repoModel.encrypted) {

            Intent getIntent = CarouselImagePreviewActivity.startThisFromObjs(requireContext(), dirent);
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

            //check need to update
            if (local.exists() && local.length() == dirent.size) {
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
        if (local.exists() && local.length() == direntModel.size) {
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
        String[] sArray = uids.toArray(new String[0]);
        BackgroundJobManagerImpl.getInstance().startDownloadChainWorker(sArray);

        closeActionMode();
    }

    public void rename(List<BaseModel> models) {
        if (CollectionUtils.isEmpty(models)) {
            return;
        }

        RenameDialogFragment dialogFragment;

        BaseModel first = models.get(0);
        if (first instanceof DirentModel dirent) {
            dialogFragment = RenameDialogFragment.newInstance(dirent.name, dirent.full_path, dirent.repo_id, dirent.type);
        } else if (first instanceof RepoModel repo) {
            dialogFragment = RenameDialogFragment.newInstance(repo.repo_name, "/", repo.repo_id, "repo");
        } else {
            return;
        }

        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    ToastUtils.showLong(R.string.rename_successful);
                }

                loadData(RefreshStatusEnum.REMOTE);
                closeActionMode();
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
            loadData(RefreshStatusEnum.REMOTE);
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

                loadData(RefreshStatusEnum.REMOTE);
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

                    closeActionMode();

                    loadData(RefreshStatusEnum.REMOTE);
                }
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

            loadData(RefreshStatusEnum.REMOTE);
        }
    });

    private final ActivityResultLauncher<Intent> fileActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                loadData(RefreshStatusEnum.REMOTE);
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

            loadData(RefreshStatusEnum.REMOTE);

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
    private void addUploadTask(List<BaseModel> dirents, boolean isUpdate) {
        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        List<DirentModel> direntModels = dirents.stream().map(m -> (DirentModel) m).collect(Collectors.toList());

        DirentModel dirent = direntModels.get(0);

        RepoModel targetedModel = getNavContext().getRepoModel();
        String targetDir = getNavContext().getNavPath();
        File localFilePath = getLocalDestinationFile(dirent.repo_id, dirent.repo_name, dirent.full_path);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        mainViewModel.addUploadTask(account, targetedModel, targetDir, localFilePath.getAbsolutePath(), isUpdate, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity transferEntity) throws Exception {
                ToastUtils.showLong(R.string.added_to_upload_tasks);
            }
        });
    }
}
