package com.seafile.seadroid2.ui.transfer_list;

import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.view.ActionMode;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.base.adapter.LoadMoreAdapter;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import io.reactivex.functions.Consumer;

public abstract class TransferListFragment extends BaseFragment {
    private LayoutFrameSwipeRvBinding binding;
    protected TransferListAdapter adapter;
    private ActionMode actionMode;
    protected TransferActivity activity = null;
    private LinearLayoutManager layoutManager;
    private TransferListViewModel viewModel;

    /**
     * key is uid, value is position
     */
    private final ConcurrentHashMap<String, Integer> positionMap = new ConcurrentHashMap<>();

    private int pageIndex = 0;
    private final int pageSize = 100;
    private QuickAdapterHelper helper;

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        activity = (TransferActivity) context;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        viewModel = new ViewModelProvider(this).get(TransferListViewModel.class);
    }

    public TransferListViewModel getViewModel() {
        return viewModel;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.swipeRefreshLayout.setOnRefreshListener(this::refreshData);
        layoutManager = (LinearLayoutManager) binding.rv.getLayoutManager();
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initAdapter();

        initViewModel();
    }

    @Override
    public void onFirstResume() {
        super.onFirstResume();
        loadNext(true);
    }

    private void initAdapter() {
        adapter = new TransferListAdapter();
        TextView tipView = TipsViews.getTipTextView(requireContext());
        if (getTransferAction() == TransferAction.DOWNLOAD) {
            tipView.setText(R.string.no_download_tasks);
        } else {
            tipView.setText(R.string.no_upload_tasks);
        }
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(false);

        adapter.setTransferAction(getTransferAction());
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<FileTransferEntity>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<FileTransferEntity, ?> baseQuickAdapter, @NonNull View view, int i) {
                if (adapter.getActionMode()) {
                    toggleAdapterItemSelectedOnLongClick(i);

                    //update bar title
                    updateContextualActionBar();
                }
            }
        });

        adapter.setOnItemLongClickListener(new BaseQuickAdapter.OnItemLongClickListener<FileTransferEntity>() {
            @Override
            public boolean onLongClick(@NonNull BaseQuickAdapter<FileTransferEntity, ?> baseQuickAdapter, @NonNull View view, int i) {
                //return
                if (adapter.getActionMode()) {
                    return true;
                }

                toggleAdapterItemSelectedOnLongClick(i);

                startContextualActionMode();

                updateContextualActionBar();

                return true;
            }
        });

        adapter.addOnItemChildClickListener(R.id.expandable_toggle_button, (baseQuickAdapter, view, i) -> {

            //when ActionMode is On, return
            if (adapter.getActionMode()) {
                return;
            }

            showBottomSheet(adapter.getItems().get(i));
        });

        LoadMoreAdapter loadMoreAdapter = new LoadMoreAdapter();
        loadMoreAdapter.setOnLoadMoreListener(new TrailingLoadStateAdapter.OnTrailingListener() {
            @Override
            public void onLoad() {
                loadNext(false);
            }

            @Override
            public void onFailRetry() {
            }

            @Override
            public boolean isAllowLoading() {
                return !binding.swipeRefreshLayout.isRefreshing();
            }
        });

        helper = new QuickAdapterHelper.Builder(adapter)
                .setTrailingLoadStateAdapter(loadMoreAdapter)
                .build();
        binding.rv.setAdapter(helper.getAdapter());
    }


    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), aBoolean -> binding.swipeRefreshLayout.setRefreshing(aBoolean));

        getViewModel().getShowLoadingDialogLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoadingDialog(aBoolean);
            }
        });

        getViewModel().getTransferListLiveData().observe(getViewLifecycleOwner(), new Observer<List<FileTransferEntity>>() {
            @Override
            public void onChanged(List<FileTransferEntity> list) {
                submitData(list);
            }
        });
    }

    private void submitData(List<FileTransferEntity> list) {
        if (CollectionUtils.isEmpty(list)) {
            if (pageIndex <= 1) {
                adapter.setStateViewEnable(true);
                adapter.submitList(null);
            }
            return;
        }

        if (pageIndex == 1) {
            for (int i = 0; i < list.size(); i++) {
                positionMap.put(list.get(i).uid, i);
            }
            adapter.submitList(list);

        } else {
            int start = adapter.getItemCount();
            int end = start + list.size();

            for (int i = start; i < end; i++) {
                int index = i - start;
                positionMap.put(list.get(index).uid, i);
            }
            adapter.addAll(list);
        }

        if (list.size() < pageSize) {
            helper.setTrailingLoadState(new LoadState.NotLoading(true));
            if (helper.getTrailingLoadStateAdapter() != null) {
                helper.getTrailingLoadStateAdapter().checkDisableLoadMoreIfNotFullPage();
            }
        } else {
            helper.setTrailingLoadState(new LoadState.NotLoading(false));
        }
    }

    public void showBottomSheet(FileTransferEntity entity) {
        int rid = R.menu.bottom_sheet_op_transfer_list;
        BottomSheetMenuFragment.Builder builder = BottomSheetHelper.buildSheet(getActivity(), rid, menuItem -> {
            int itemId = menuItem.getItemId();

            if (itemId == R.id.delete) {
                onBottomSheetFileDelete(entity);
            } else if (itemId == R.id.upload) {
                BackgroundJobManagerImpl.getInstance().startDownloadChainWorker(entity.uid);
            } else if (itemId == R.id.download) {
                BackgroundJobManagerImpl.getInstance().startDownloadChainWorker(entity.uid);
            }
        });

        builder.removeMenu(R.id.upload);
        builder.removeMenu(R.id.download);

        builder.show(getChildFragmentManager());
    }

    protected void onBottomSheetFileDelete(FileTransferEntity entity) {
        if (TransferAction.DOWNLOAD == getTransferAction()) {
            showDeleteDownloadConfirmDialog(entity);
        } else {
            showDeleteUploadConfirmDialog(entity);
        }
    }

    private void showDeleteDownloadConfirmDialog(FileTransferEntity entity) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
        builder.setTitle(R.string.delete_records);

        String deleteFile = getString(R.string.delete_local_file_sametime);
        CharSequence[] sequences = new CharSequence[1];
        sequences[0] = deleteFile;
        boolean[] booleans = new boolean[1];
        booleans[0] = true;
        builder.setMultiChoiceItems(sequences, booleans, new DialogInterface.OnMultiChoiceClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which, boolean isChecked) {
                booleans[which] = isChecked;
            }
        });

        builder.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                getViewModel().getShowLoadingDialogLiveData().setValue(true);

                getViewModel().deleteTransferData(entity, booleans[0], getTransferAction(), new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        ToastUtils.showLong(R.string.deleted);

                        removeSpecialEntity(entity.uid);

                        getViewModel().getShowLoadingDialogLiveData().setValue(false);
                    }
                });
                dialog.dismiss();
            }
        });

        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

    private void showDeleteUploadConfirmDialog(FileTransferEntity entity) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
        builder.setTitle(R.string.delete_records);

        builder.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                getViewModel().getShowLoadingDialogLiveData().setValue(true);

                getViewModel().deleteTransferData(entity, false, getTransferAction(), new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        ToastUtils.showLong(R.string.deleted);

                        removeSpecialEntity(entity.uid);

                        getViewModel().getShowLoadingDialogLiveData().setValue(false);
                    }
                });
                dialog.dismiss();
            }
        });

        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

    public void restartAllSpecialStatusTasks(TransferAction transferAction, TransferStatus transferStatus) {
        getViewModel().restartSpecialStatusTask(transferAction, transferStatus, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                if (aBoolean) {
                    if (TransferAction.DOWNLOAD == transferAction) {
                        //
                        BackgroundJobManagerImpl.getInstance().cancelDownloadWorker();

                        //
                        BackgroundJobManagerImpl.getInstance().startDownloadChainWorker();
                    } else {

                        //
                        BackgroundJobManagerImpl.getInstance().cancelMediaWorker();
                        BackgroundJobManagerImpl.getInstance().cancelFolderAutoUploadWorker();
                        BackgroundJobManagerImpl.getInstance().cancelFileManualUploadWorker();

                        //
                        BackgroundJobManagerImpl.getInstance().startMediaWorkerChain(false);
                        BackgroundJobManagerImpl.getInstance().startFolderAutoBackupWorkerChain(false);
                        BackgroundJobManagerImpl.getInstance().startFileManualUploadWorker();
                    }
                } else {
                    ToastUtils.showLong(R.string.done);
                }
            }
        });
    }

    private void toggleAdapterItemSelectedOnLongClick(int i) {
        //action mode on
        if (!adapter.getActionMode()) {
            adapter.setActionModeOn(true);
        }

        FileTransferEntity entity = adapter.getItems().get(i);
        entity.is_checked = !entity.is_checked;
        adapter.getItems().set(i, entity);
        adapter.notifyItemChanged(i);
    }

    public abstract TransferAction getTransferAction();

    public abstract void deleteSelectedItems(List<FileTransferEntity> list);

    public abstract void restartSelectedItems(List<FileTransferEntity> list);

    public ConcurrentHashMap<String, Integer> getPositionMap() {
        return positionMap;
    }

    protected void removeSpecialEntity(String uid) {
        Integer integer = positionMap.get(uid);
        if (integer == null) {
            return;
        }

        adapter.notifyItemRemoved(integer);
        adapter.getItems().remove(integer.intValue());

        resort();
    }

    protected void resort() {
        //
        positionMap.clear();


        List<FileTransferEntity> list = adapter.getItems();
        for (int i = 0; i < list.size(); i++) {
            positionMap.put(list.get(i).uid, i);
        }
    }

    protected void refreshData() {
        pageIndex = 0;
        loadNext(true);
    }

    private void loadNext(boolean isShowRefresh) {
        pageIndex++;

        if (pageIndex <= 1) {
            positionMap.clear();
        }

        getViewModel().loadData(getTransferAction(), pageIndex, pageSize, isShowRefresh);
    }

    public void notifyProgressById(String transferId, long transferredSize, int percent, String event) {

        if (TextUtils.isEmpty(transferId)) {
            return;
        }

        if (!positionMap.containsKey(transferId)) {
            return;
        }

        Integer position = positionMap.get(transferId);
        if (position == null || position == -1) {
            return;
        }

        if (TransferEvent.EVENT_TRANSFER_FAILED.equals(event)) {
            adapter.getItems().get(position).transferred_size = transferredSize;
            adapter.getItems().get(position).transfer_status = TransferStatus.FAILED;
        } else if (TransferEvent.EVENT_TRANSFER_SUCCESS.equals(event)) {
            adapter.getItems().get(position).transferred_size = transferredSize;
            adapter.getItems().get(position).transfer_status = TransferStatus.SUCCEEDED;
        } else if (TransferEvent.EVENT_TRANSFERRING.equals(event)) {
            adapter.getItems().get(position).transferred_size = transferredSize;
            adapter.getItems().get(position).transfer_status = TransferStatus.IN_PROGRESS;
        }

//        if (isItemVisible(position)) {
        Bundle bundle = new Bundle();
        bundle.putInt(TransferWorker.KEY_DATA_PROGRESS, percent);
        bundle.putLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, transferredSize);
        adapter.notifyItemChanged(position, bundle);
//        }
    }


    public boolean isItemVisible(int position) {

        if (layoutManager == null) {
            return false;
        }

        int firstVisibleItemPosition = layoutManager.findFirstVisibleItemPosition();
        int lastVisibleItemPosition = layoutManager.findLastVisibleItemPosition();

        return position >= firstVisibleItemPosition && position <= lastVisibleItemPosition;
    }

    public void cancelSelectItems() {
        if (adapter == null) return;

        adapter.setActionModeOn(false);

        if (actionMode != null) {
            actionMode.finish();
            actionMode = null;
        }
    }


    public void startContextualActionMode() {
        if (actionMode == null) {
            // start the actionMode
            actionMode = activity.startSupportActionMode(new ActionModeCallback());
        }
    }

    public ActionMode getActionMode() {
        return actionMode;
    }

    /**
     * update state of contextual action bar
     */
    public void updateContextualActionBar() {
        int c = (int) adapter.getSelectedCountByMode();
        boolean itemsChecked = c > 0;

        if (itemsChecked && actionMode == null) {
            // there are some selected items, start the actionMode
            actionMode = activity.startSupportActionMode(new ActionModeCallback());
            adapter.setActionModeOn(true);
        }

        if (actionMode != null) {
            // Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            actionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    c,
                    c));
        }

    }

    /**
     * Represents a contextual mode of the user interface.
     * Action modes can be used to provide alternative interaction modes and replace parts of the normal UI until finished.
     * A Callback configures and handles events raised by a user's interaction with an action mode.
     */
    class ActionModeCallback implements ActionMode.Callback {
        private boolean allItemsSelected;

        public ActionModeCallback() {
        }

        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            // Inflate the menu for the contextual action bar (CAB)
            MenuInflater inflater = mode.getMenuInflater();
            inflater.inflate(R.menu.transfer_list_multi_choice_menu, menu);
            if (adapter == null) return true;

            adapter.setActionModeOn(true);
            return true;
        }

        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            // Here you can perform updates to the contextual action bar (CAB) due to
            // an invalidate() request
            return false;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            // Respond to clicks on the actions in the contextual action bar (CAB)
            final List<FileTransferEntity> selectedList = adapter.getSelectedList();

            if (item.getItemId() != R.id.action_mode_select_all && CollectionUtils.isEmpty(selectedList)) {
                ToastUtils.showLong(R.string.action_mode_no_items_selected);
                return true;
            }

            if (item.getItemId() == R.id.action_mode_delete) {
                deleteSelectedItems(selectedList);
                cancelSelectItems();
            } else if (item.getItemId() == R.id.action_mode_restart) {
                restartSelectedItems(selectedList);
                cancelSelectItems();
            } else if (item.getItemId() == R.id.action_mode_select_all) {
                if (!allItemsSelected) {
                    adapter.setItemSelected(true);
                } else {
                    adapter.setItemSelected(false);
                }
                updateContextualActionBar();

                allItemsSelected = !allItemsSelected;
            }
            return true;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            if (adapter == null) return;

            adapter.setActionModeOn(false);

            // Here you can make any necessary updates to the activity when
            // the contextual action bar (CAB) is removed. By default, selected items are deselected/unchecked.
            actionMode = null;
        }

    }
}