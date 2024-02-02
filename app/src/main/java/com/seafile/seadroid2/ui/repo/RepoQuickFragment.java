package com.seafile.seadroid2.ui.repo;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.view.ActionMode;
import androidx.core.content.FileProvider;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.account.SupportDataManager;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.bottomsheetmenu.OnMenuClickListener;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.play.exoplayer.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.dialog.AppChoiceDialog;
import com.seafile.seadroid2.ui.dialog.FetchFileDialog;
import com.seafile.seadroid2.ui.dialog_fragment.CopyMoveDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.DeleteRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.RenameDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.main.MainViewModel;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.view.TipsViews;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import kotlin.Pair;

public class RepoQuickFragment extends BaseFragmentWithVM<RepoViewModel> {
    private static final String KEY_REPO_SCROLL_POSITION = "repo_scroll_position";
    private static final String KEY_REPO_LIST = "key_in_repo_list";

    private final HashMap<String, Long> mRefreshStatusExpireTimeMap = new HashMap<String, Long>();
    private boolean isFirstLoadData = true;
    private LayoutFrameSwipeRvBinding binding;
    private RepoQuickAdapter adapter;
    private LinearLayoutManager rvManager;

    private MainViewModel mainViewModel;
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
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.swipeRefreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                loadData(true);
            }
        });
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        init();

        initAdapter();

        initViewModel();
    }

    @Override
    public void onResume() {
        super.onResume();
        d("load data：onResume");
        if (isFirstLoadData) {
            isFirstLoadData = false;
            d("load data：isFirstLoadData");
            loadData(true);
        } else {
            if (isForce()) {
                loadData(true);
            }
        }
    }

    private void init() {
        rvManager = (LinearLayoutManager) binding.rv.getLayoutManager();
    }

    private void initAdapter() {
        adapter = new RepoQuickAdapter();

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            if (adapter.getActionMode()) {
                //toggle
                toggleAdapterItemSelectedOnLongClick(i);

                //update bar title
                updateContextualActionBar();
            } else {
                BaseModel baseModel = adapter.getItems().get(i);
                if (baseModel instanceof RepoModel) {
                    RepoModel repoModel = (RepoModel) baseModel;
                    if (repoModel.encrypted) {
                        doEncrypt(repoModel);
                    } else {
                        navTo(baseModel);
                    }
                } else {
                    navTo(baseModel);
                }
            }
        });

        adapter.setOnItemLongClickListener((baseQuickAdapter, view, i) -> {
            if (getNavContext().isInRepo()) {

                //return
                if (adapter.getActionMode()) {
                    return true;
                }

                //toggle
                toggleAdapterItemSelectedOnLongClick(i);

                //start action mode
                startContextualActionMode();

                //It's actually updating the title of the ActionBar
                updateContextualActionBar();

                return true;
            } else {
                return false;
            }
        });

        adapter.addOnItemChildClickListener(R.id.expandable_toggle_button, (baseQuickAdapter, view, i) -> {

            //when ActionMode is On, return
            if (adapter.getActionMode()) {
                return;
            }

            //open bottom sheet dialog
            if (getNavContext().isInRepo()) {
                DirentModel direntModel = (DirentModel) adapter.getItems().get(i);
                if (direntModel.isDir()) {
                    showDirNewBottomSheet(direntModel);
                } else {
                    showFileBottomSheet(direntModel);
                }
            } else {
                showRepoBottomSheet((RepoModel) adapter.getItems().get(i));
            }
        });

        binding.rv.setAdapter(createMuiltAdapterHelper(adapter).getAdapter());
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), aBoolean -> binding.swipeRefreshLayout.setRefreshing(aBoolean));

        getViewModel().getExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<Pair<Integer, SeafException>>() {
            @Override
            public void onChanged(Pair<Integer, SeafException> exceptionPair) {
                showErrorTip();
            }
        });

        getViewModel().getStarLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    loadData(true);
                }
            }
        });

        getViewModel().getObjsListLiveData().observe(getViewLifecycleOwner(), new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> repoModels) {

                notifyDataChanged(repoModels);

                restoreScrollPosition();
            }
        });

        mainViewModel.getOnResortListLiveData().observe(getViewLifecycleOwner(), new Observer<Integer>() {
            @Override
            public void onChanged(Integer a) {
                SLogs.d("resort: " + a);
                loadData();
            }
        });

        mainViewModel.getOnForceRefreshRepoListLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                loadData(true);
            }
        });

    }

    public void clearExpireRefreshMap() {
        mRefreshStatusExpireTimeMap.clear();
    }

    private boolean isForce() {
        long now = TimeUtils.getNowMills();
        Long expire;
        if (getNavContext().isInRepoList()) {
            expire = mRefreshStatusExpireTimeMap.get(KEY_REPO_LIST);
        } else {
            String k = getNavContext().getRepoModel().repo_id + getNavContext().getNavPath();
            expire = mRefreshStatusExpireTimeMap.get(k);
        }
        return expire == null || now > expire;
    }

    public void loadData() {
        loadData(false);
    }

    public void loadData(boolean forceRefresh) {

        if (forceRefresh) {
            long now = TimeUtils.getNowMills();
            now += 1000 * 60 * 10;//10min
            if (getNavContext().isInRepoList()) {
                mRefreshStatusExpireTimeMap.put(KEY_REPO_LIST, now);
            } else {
                String k = getNavContext().getRepoModel().repo_id + getNavContext().getNavPath();
                mRefreshStatusExpireTimeMap.put(k, now);
            }
        }

        getViewModel().loadData(getNavContext(), forceRefresh);
    }


    private void notifyDataChanged(List<BaseModel> repoModels) {
        if (CollectionUtils.isEmpty(repoModels)) {
            showEmptyTip();
        } else {
            adapter.notifyDataChanged(repoModels);
        }
    }

    private void showEmptyTip() {
        showAdapterTip(R.string.no_repo);
    }

    private void showErrorTip() {
        int strInt = !getNavContext().isInRepo() ? R.string.error_when_load_repos : R.string.error_when_load_dirents;
        showAdapterTip(strInt);
    }

    private void showAdapterTip(int textRes) {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(textRes);
        tipView.setOnClickListener(v -> loadData(true));
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    private void navTo(BaseModel model) {
        //save
        saveScrollPosition();

        if (getNavContext().isInRepoList()) {
            getNavContext().push(model);
            loadData(isForce());
        } else if (model instanceof DirentModel) {
            DirentModel direntModel = (DirentModel) model;
            if (direntModel.isDir()) {
                getNavContext().push(direntModel);
                loadData(isForce());
            } else {
                openDirent(direntModel, true);
            }
        }

        //notify navContext changed
        mainViewModel.getOnNavContextChangeListenerLiveData().setValue(true);
    }

    public boolean backTo() {
        if (getNavContext().isInRepo()) {
            if (adapter.getActionMode()) {
                adapter.setActionModeOn(false);
            } else {
                //
                removeScrollPosition();
                //
                getNavContext().pop();
                getViewModel().loadData(getNavContext(), false);

                //notify navContext changed
                mainViewModel.getOnNavContextChangeListenerLiveData().setValue(true);
            }
            return true;
        }
        return false;
    }

    private void doEncrypt(RepoModel repoModel) {
        getViewModel().getObjFromDB(repoModel.repo_id, objsModel -> {
            if (objsModel == null) {
                showPasswordDialog(repoModel);
            } else {
                long now = TimeUtils.getNowMills();
                if (objsModel.decrypt_expire_time_long == 0) {
                    showPasswordDialog(repoModel);
                } else if (now < objsModel.decrypt_expire_time_long) {
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
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    navTo(repoModel);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), PasswordDialogFragment.class.getSimpleName());
    }


    private void saveScrollPosition() {
        View vi = binding.rv.getChildAt(0);
        int top = (vi == null) ? 0 : vi.getTop();
        final int index = rvManager.findFirstVisibleItemPosition();
        final ScrollState state = new ScrollState(index, top);

        removeScrollPosition();

        if (!getNavContext().isInRepo()) {
            scrollPositions.put(KEY_REPO_SCROLL_POSITION, state);
        } else {
            String k = getNavContext().getNavPath();
            scrollPositions.put(k, state);
        }
    }

    private void removeScrollPosition() {
        if (!getNavContext().isInRepo()) {
            scrollPositions.remove(KEY_REPO_SCROLL_POSITION);
        } else {
            String k = getNavContext().getNavPath();
            scrollPositions.remove(k);
        }
    }

    private void restoreScrollPosition() {
        ScrollState state;
        if (!getNavContext().isInRepo()) {
            state = scrollPositions.get(KEY_REPO_SCROLL_POSITION);
        } else {
            state = scrollPositions.get(getNavContext().getNavPath());
        }

        if (state != null) {
            rvManager.scrollToPositionWithOffset(state.index, state.top);
        } else {
            rvManager.scrollToPosition(0);
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();

        if (actionMode != null) {
            actionMode.finish();
        }
    }

    private void toggleAdapterItemSelectedOnLongClick(int i) {
        //action mode on
        if (!adapter.getActionMode()) {
            adapter.setActionModeOn(true);
        }

        DirentModel direntModel = (DirentModel) adapter.getItems().get(i);
        direntModel.is_selected = !direntModel.is_selected;
        adapter.getItems().set(i, direntModel);
        adapter.notifyItemChanged(i);
    }

    public void closeActionMode(){
        if (adapter.getActionMode()) {
            adapter.setActionModeOn(false);

            actionMode.finish();
            actionMode = null;
        }
    }

    public void startContextualActionMode() {
        if (!getNavContext().isInRepo()) return;

        if (actionMode == null) {
            // start the actionMode
            actionMode = activity.startSupportActionMode(new ActionModeCallback());
        }
    }

    /**
     * update state of contextual action bar (CAB)
     */
    public void updateContextualActionBar() {
        if (!getNavContext().isInRepo()) return;

        if (actionMode == null) {
            // there are some selected items, start the actionMode
            actionMode = activity.startSupportActionMode(new ActionModeCallback());
        } else {
            int count = adapter.getSelectedList().size();
            actionMode.setTitle(getResources().getQuantityString(R.plurals.transfer_list_items_selected, count, count));
        }

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

            // to hidden  "r" permissions  files or folder
            if (!getNavContext().hasWritePermissionWithRepo()) {
                menu.findItem(R.id.action_mode_delete).setVisible(false);
                menu.findItem(R.id.action_mode_move).setVisible(false);
            }
            return true;
        }

        @SuppressLint("NewApi")
        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            /*
             * The ActionBarPolicy determines how many action button to place in the ActionBar
             * and the default amount is 2.
             */
            menu.findItem(R.id.action_mode_delete).setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
            menu.findItem(R.id.action_mode_copy).setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
            menu.findItem(R.id.action_mode_select_all).setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

            // Here you can perform updates to the contextual action bar (CAB) due to
            // an invalidate() request
            return true;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            //check data
            List<DirentModel> selectedDirents = adapter.getSelectedList();
            if (CollectionUtils.isEmpty(selectedDirents) || !getNavContext().isInRepo()) {
                if (item.getItemId() != R.id.action_mode_select_all) {
                    ToastUtils.showLong(R.string.action_mode_no_items_selected);
                    return true;
                }
            }
            int itemId = item.getItemId();
            if (itemId == R.id.action_mode_select_all) {
                adapter.setItemSelected(!allItemsSelected);
                updateContextualActionBar();

                allItemsSelected = !allItemsSelected;
            } else if (itemId == R.id.action_mode_delete) {
                deleteDirens(selectedDirents);
            } else if (itemId == R.id.action_mode_copy) {
                DirentModel dirent = selectedDirents.get(0);
                copyFiles(dirent.repo_id, dirent.repo_name, dirent.parent_dir, selectedDirents);
            } else if (itemId == R.id.action_mode_move) {
                DirentModel dirent = selectedDirents.get(0);
                moveFiles(dirent.repo_id, dirent.repo_name, dirent.parent_dir, selectedDirents);
            } else if (itemId == R.id.action_mode_download) {
                ToastUtils.showLong("action: download");
                // mActivity.downloadFiles(repoID, repoName, dirPath, selectedDirents);
            } else {
                return false;
            }

            return true;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            if (adapter == null) return;

            adapter.setActionModeOn(false);

            // Here you can make any necessary updates to the activity when
            // the contextual action bar (CAB) is removed. By default, selected items are deselected/unchecked.
            if (actionMode != null) {
                actionMode = null;
            }
        }

    }

    public void showRepoBottomSheet(RepoModel model) {
        int rid = model.starred ? R.menu.bottom_sheet_op_repo_with_unstar : R.menu.bottom_sheet_op_repo;
        BottomSheetHelper.showSheet(getActivity(), rid, menuItem -> {
            if (menuItem.getItemId() == R.id.star_repo) {
                starOrNot(model);
            } else if (menuItem.getItemId() == R.id.rename_repo) {
                rename(model.repo_id, null, model.repo_name, "repo");
            } else if (menuItem.getItemId() == R.id.delete_repo) {
                deleteRepo(model.repo_id);
            }
        });
    }

    public void showFileBottomSheet(DirentModel dirent) {
        BottomSheetHelper.showSheet(getActivity(), R.menu.bottom_sheet_op_file, new OnMenuClickListener() {
            @Override
            public void onMenuClick(MenuItem menuItem) {
                int itemId = menuItem.getItemId();

                if (itemId == R.id.share) {
                    showShareDialog(dirent);
                } else if (itemId == R.id.open) {
                    openDirent(dirent, true);
                } else if (itemId == R.id.delete) {
                    deleteDirent(dirent);
                } else if (itemId == R.id.copy) {
                    copyFiles(dirent.repo_id, dirent.repo_name, dirent.parent_dir, CollectionUtils.newArrayList(dirent));
                } else if (itemId == R.id.move) {
                    moveFiles(dirent.repo_id, dirent.repo_name, dirent.parent_dir, CollectionUtils.newArrayList(dirent));
                } else if (itemId == R.id.rename) {
                    rename(dirent.repo_id, dirent.full_path, dirent.name, "file");
                } else if (itemId == R.id.update) {
                    // mActivity.addUpdateTask(repoID, repoName, dir, localPath);
                    //TODO: 文件上传
                    ToastUtils.showLong("TODO: 文件上传");
                } else if (itemId == R.id.download) {
                    // mActivity.downloadFile(dir, dirent.name);
                    //TODO: 文件下载
                    ToastUtils.showLong("TODO: 文件下载");
                } else if (itemId == R.id.export) {
                    chooseExportApp(dirent);
                } else if (itemId == R.id.star) {
                    starOrNot(dirent);
                }
            }
        });
    }

    public void showDirNewBottomSheet(DirentModel dirent) {
        int rid = dirent.starred ? R.menu.bottom_sheet_op_dir_with_unstar : R.menu.bottom_sheet_op_dir;
        BottomSheetMenuFragment.Builder builder1 = BottomSheetHelper.buildSheet(getActivity(), rid, new OnMenuClickListener() {
            @Override
            public void onMenuClick(MenuItem menuItem) {
                int itemId = menuItem.getItemId();

                if (itemId == R.id.star) {
                    starOrNot(dirent);
                } else if (itemId == R.id.share) {
                    showShareDialog(dirent);
                } else if (itemId == R.id.delete) {
                    deleteDirent(dirent);
                } else if (itemId == R.id.copy) {
                    copyFiles(dirent.repo_id, dirent.repo_name, dirent.parent_dir, CollectionUtils.newArrayList(dirent));
                } else if (itemId == R.id.move) {
                    moveFiles(dirent.repo_id, dirent.repo_name, dirent.parent_dir, CollectionUtils.newArrayList(dirent));
                } else if (itemId == R.id.rename) {
                    rename(dirent.repo_id, dirent.full_path, dirent.name, "dir");
                } else if (itemId == R.id.download) {
                    // mActivity.downloadDir(dir, dirent.name, true);
                    //TODO: 文件夹下载
                    ToastUtils.showLong("TODO: 文件夹下载");
                }
            }
        });

        if (!dirent.hasWritePermission()) {
            builder1.removeMenu(R.id.rename);
            builder1.removeMenu(R.id.delete);
            builder1.removeMenu(R.id.move);
        }

//        SeafRepo repo = getDataManager().getCachedRepoByID(repoID);
//        if (repo != null && repo.encrypted) {
//            builder1.removeMenu(R.id.share);
//        }
        builder1.show(getChildFragmentManager());
    }


    /************  Files ************/

    /**
     * Export a file.
     * 1. first ask the user to choose an app
     * 2. then download the latest version of the file
     * 3. start the choosen app
     */
    private void chooseExportApp(DirentModel direntModel) {

        final File file = SupportDataManager
                .getInstance()
                .getDataManager()
                .getLocalRepoFile(direntModel.repo_name, direntModel.repo_id, direntModel.full_path);
        Uri uri = FileProvider.getUriForFile(requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = Utils.getAppsByIntent(sendIntent);

        if (infos.isEmpty()) {
            ToastUtils.showLong(R.string.no_app_available);
            return;
        }

        AppChoiceDialog dialog = new AppChoiceDialog();
        dialog.init(getString(R.string.export_file), infos, new AppChoiceDialog.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(AppChoiceDialog.CustomAction action) {
            }

            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                sendIntent.setClassName(packageName, className);

                if (!Utils.isNetworkOn() && file.exists()) {
                    startActivity(sendIntent);
                    return;
                }
                fetchFileAndExport(sendIntent, direntModel.repo_name, direntModel.repo_id, direntModel.full_path, direntModel.size);
            }

        });
        dialog.show(getChildFragmentManager(), AppChoiceDialog.class.getSimpleName());
    }

    public void fetchFileAndExport(Intent intent, String repoName, String repoID, String path, long fileSize) {

        FetchFileDialog fetchFileDialog = new FetchFileDialog();
        fetchFileDialog.init(repoName, repoID, path, fileSize, new FetchFileDialog.FetchFileListener() {
            @Override
            public void onSuccess() {
                startActivity(intent);
            }

            @Override
            public void onDismiss() {
            }

            @Override
            public void onFailure(SeafException err) {
            }
        });
        fetchFileDialog.show(getChildFragmentManager(), FetchFileDialog.class.getSimpleName());
    }

    public void openDirent(DirentModel dirent, boolean isOpenWith) {
        String fileName = dirent.name;
        String filePath = Utils.pathJoin(getNavContext().getNavPath(), fileName);

        RepoModel repoModel = getNavContext().getRepoModel();

        // Encrypted repo does not support gallery,
        // because pic thumbnail under encrypted repo was not supported at the server side
        if (Utils.isViewableImage(fileName) && !repoModel.encrypted) {
            WidgetUtils.startGalleryActivity(getActivity(),
                    repoModel.repo_name,
                    repoModel.repo_id,
                    getNavContext().getNavPath(),
                    fileName,
                    SupportAccountManager.getInstance().getCurrentAccount());
        } else if (fileName.endsWith(Constants.Format.DOT_SDOC)) {
            SeaWebViewActivity.openSdoc(getContext(), repoModel.repo_name, repoModel.repo_id, dirent.parent_dir + dirent.name);
        } else if (Utils.isVideoFile(fileName)) {
            AlertDialog.Builder builder = new AlertDialog.Builder(requireContext());
            builder.setItems(R.array.video_download_array, (dialog, which) -> {
                if (which == 0) {
                    CustomExoVideoPlayerActivity.startThis(getContext(), fileName, repoModel.repo_id, filePath, SupportAccountManager.getInstance().getCurrentAccount());
                } else if (which == 1) {
                    startFileActivity(repoModel.repo_name, repoModel.repo_id, filePath, dirent.size, isOpenWith);
                }
            }).show();
        } else {
            File localFile = SupportDataManager.getInstance().getDataManager()
                    .getLocalCachedFile(repoModel.repo_name, repoModel.repo_id, filePath, dirent.id);
            if (localFile != null) {
                WidgetUtils.showFile(getActivity(), localFile, isOpenWith);
            } else {
                startFileActivity(repoModel.repo_name, repoModel.repo_id, filePath, dirent.size, isOpenWith);
            }
        }
    }

    private void startFileActivity(String repoName, String repoID, String path, long size, boolean isOpenWith) {
        ToastUtils.showLong("启动文件下载页: 暂未完成");

//        int taskID = txService.addDownloadTask(account, repoName, repoID, path, size);
//        Intent intent = new Intent(this, FileActivity.class);
//        intent.putExtra("repoName", repoName);
//        intent.putExtra("repoID", repoID);
//        intent.putExtra("filePath", path);
//        intent.putExtra("account", account);
//        intent.putExtra("taskID", taskID);
//        intent.putExtra("is_open_with", isOpenWith);
//        startActivityForResult(intent, DOWNLOAD_FILE_REQUEST);
    }

    public void rename(String repoID, String curPath, String curName, String type) {
        RenameDialogFragment dialogFragment = RenameDialogFragment.newInstance();
        dialogFragment.initData(curName, curPath, repoID, type);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), RenameDialogFragment.class.getSimpleName());
    }

    public void deleteRepo(String repoID) {
        DeleteRepoDialogFragment dialogFragment = DeleteRepoDialogFragment.newInstance(repoID);
        dialogFragment.setRefreshListener(isDone -> {
            if (isDone) {
                ToastUtils.showLong(R.string.delete_successful);

                loadData(true);
            }
        });
        dialogFragment.show(getChildFragmentManager(), DeleteRepoDialogFragment.class.getSimpleName());
    }

    public void deleteDirent(DirentModel dirent) {
        deleteDirens(CollectionUtils.newArrayList(dirent));
    }

    public void deleteDirens(List<DirentModel> dirents) {
        DeleteFileDialogFragment dialogFragment = DeleteFileDialogFragment.newInstance();
        dialogFragment.initData(dirents);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    ToastUtils.showLong(R.string.delete_successful);

                    closeActionMode();

                    loadData(true);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), DeleteFileDialogFragment.class.getSimpleName());
    }

    /**
     * Share a file. Generating a file share link and send the link or file to someone
     * through some app.
     */
    public void showShareDialog(DirentModel direntModel) {
        MaterialAlertDialogBuilder mBuilder = new MaterialAlertDialogBuilder(requireContext());

        boolean inChina = Utils.isInChina();
        String[] strings;
        //if user  in China ，system  add  WeChat  share
        if (inChina) {
            strings = getResources().getStringArray(R.array.file_action_share_array_zh);
        } else {
            strings = getResources().getStringArray(R.array.file_action_share_array);
        }

        mBuilder.setItems(strings, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (!inChina) {
                    which++;
                }
                switch (which) {
                    case 0: {
                        ToastUtils.showLong("TODO: 分享到微信");
                        // WidgetUtils.ShareWeChat(requireContext(), account, repoID, path, fileName, fileSize, isDir);
                    }
                    break;
                    case 1: {
                        ToastUtils.showLong("TODO: 分享链接");
                        // need input password
//                        WidgetUtils.chooseShareApp(BrowserActivity.this, repoID, path, isDir, account, null, null);
                    }
                    break;
                    case 2: {
                        ToastUtils.showLong("TODO: 分享高级链接");
//                        WidgetUtils.inputSharePassword(BrowserActivity.this, repoID, path, isDir, account);
                    }
                    break;
                }
            }
        }).show();
    }

    /**
     * Copy multiple files
     */
    public void copyFiles(String srcRepoId, String srcRepoName, String srcDir, List<DirentModel> dirents) {
        chooseCopyMoveDestForMultiFiles(srcRepoId, srcRepoName, srcDir, dirents, CopyMoveContext.OP.COPY);
    }


    /**
     * Move multiple files
     */
    public void moveFiles(String srcRepoId, String srcRepoName, String srcDir, List<DirentModel> dirents) {
        chooseCopyMoveDestForMultiFiles(srcRepoId, srcRepoName, srcDir, dirents, CopyMoveContext.OP.MOVE);
    }


    private CopyMoveContext copyMoveContext = null;

    /**
     * Choose copy/move destination for multiple files
     */
    private void chooseCopyMoveDestForMultiFiles(String repoID, String repoName, String dirPath, List<DirentModel> dirents, CopyMoveContext.OP op) {
        copyMoveContext = new CopyMoveContext(repoID, repoName, dirPath, dirents, op);

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

                    loadData(true);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), CopyMoveDialogFragment.class.getSimpleName());
    }

    private void starOrNot(BaseModel model) {
        if (model instanceof RepoModel) {
            RepoModel repoModel = (RepoModel) model;
            if (repoModel.starred) {
                getViewModel().unStar(repoModel.repo_id, "/");
            } else {
                getViewModel().star(repoModel.repo_id, "/");
            }
        } else if (model instanceof DirentModel) {
            DirentModel direntModel = (DirentModel) model;
            String path = Utils.pathJoin(getNavContext().getNavPath(), direntModel.name);
            if (direntModel.starred) {
                getViewModel().unStar(direntModel.repo_id, path);
            } else {
                getViewModel().star(direntModel.repo_id, path);
            }
        }
    }
}
