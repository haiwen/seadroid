package com.seafile.seadroid2.ui.transfer_list;

import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
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
import androidx.work.Data;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.ui.transfer.TransferActivity;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.view.TipsViews;
import com.seafile.seadroid2.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.worker.TransferWorker;

import java.util.List;

import io.reactivex.functions.Consumer;

public abstract class TransferListFragment extends BaseFragment {
    private LayoutFrameSwipeRvBinding binding;
    private TransferListAdapter adapter;
    private ActionMode actionMode;
    protected TransferActivity activity = null;
    private TransferListViewModel viewModel;

    public TransferListViewModel getViewModel() {
        return viewModel;
    }

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

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initAdapter();

        initViewModel();

        loadData();
    }

    private void initAdapter() {
        adapter = new TransferListAdapter();
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


        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());
    }

    private void initViewModel() {
        String tag;
        if (getTransferAction() == TransferAction.DOWNLOAD) {
            tag = BackgroundJobManagerImpl.TAG_DOWNLOAD_WORKER_FILES_DOWNLOAD;
        } else {
            tag = BackgroundJobManagerImpl.TAG_UPLOAD_WORKER_FILES_UPLOAD;
        }

        BackgroundJobManagerImpl.getInstance()
                .getWorkInfosByTagLiveData(tag)
                .observe(getViewLifecycleOwner(), new Observer<List<WorkInfo>>() {
                    @Override
                    public void onChanged(List<WorkInfo> workInfos) {
                        doWorkInfoLiveData(workInfos);
                    }
                });

        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), aBoolean -> binding.swipeRefreshLayout.setRefreshing(aBoolean));

        getViewModel().getFileTransferEntitiesLiveData().observe(getViewLifecycleOwner(), this::notifyDataChanged);
    }

    private void doWorkInfoLiveData(List<WorkInfo> workInfos) {
        if (CollectionUtils.isEmpty(workInfos)) {
            return;
        }
        WorkInfo workInfo = workInfos.get(0);

        //
        if (workInfo.getState().isFinished()) {
            loadData();
        } else if (workInfo.getState() == WorkInfo.State.RUNNING) {
            Data data = workInfo.getProgress();
            String transferId = data.getString(TransferWorker.DATA_TRANSFER_KEY);
            String fileName = data.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);
            int percent = data.getInt(TransferWorker.DATA_PROGRESS_KEY, 0);
            long transferredSize = data.getLong(TransferWorker.DATA_TRANSFERRED_SIZE_KEY, 0);
            long totalSize = data.getLong(TransferWorker.DATA_TOTAL_SIZE_KEY, 0);

            if (getTransferAction() == TransferAction.DOWNLOAD) {
                SLogs.e("传输列表页：" + fileName + ", 下载进度：" + percent + ", 已传输：" + transferredSize + ", 总大小：" + totalSize);
            } else {
                SLogs.e("传输列表页：" + fileName + ", 上传进度：" + percent + ", 已传输：" + transferredSize + ", 总大小：" + totalSize);
            }
            adapter.notifyProgressById(transferId, transferredSize, percent);
        }
    }

    public void showBottomSheet(FileTransferEntity entity) {
        int rid = R.menu.bottom_sheet_op_transfer_list;
        BottomSheetMenuFragment.Builder builder = BottomSheetHelper.buildSheet(getActivity(), rid, menuItem -> {
            int itemId = menuItem.getItemId();

            if (itemId == R.id.delete) {
                onBottomSheetFileDelete(entity);
            } else if (itemId == R.id.upload) {
                BackgroundJobManagerImpl.getInstance().scheduleOneTimeFilesDownloadSyncJob(entity.uid);
            } else if (itemId == R.id.download) {
                BackgroundJobManagerImpl.getInstance().scheduleOneTimeFilesDownloadSyncJob(entity.uid);
            }
        });

        builder.removeMenu(R.id.upload);
        builder.removeMenu(R.id.download);

        //TODO
//        if (getTransferAction() == TransferAction.DOWNLOAD) {
//            builder.removeMenu(R.id.upload);
//        } else if (getTransferAction() == TransferAction.UPLOAD) {
//            builder.removeMenu(R.id.download);
//        }
        builder.show(getChildFragmentManager());
    }

    protected void onBottomSheetFileDelete(FileTransferEntity entity) {
        showDeleteConfirmDialog(entity);
    }

    private void showDeleteConfirmDialog(FileTransferEntity entity) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(getContext());
        builder.setTitle(R.string.delete);

        if (getTransferAction() == TransferAction.DOWNLOAD) {
            builder.setMessage(R.string.delete_records_and_file);
        } else {//do not delete file when upload
            builder.setMessage(R.string.delete_records);
        }

        builder.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                getViewModel().deleteTransferData(entity, getTransferAction(), new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        ToastUtils.showLong(R.string.deleted);
                        dialog.dismiss();

                        loadData();
                    }
                });
            }
        });

        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

    private void toggleAdapterItemSelectedOnLongClick(int i) {
        //action mode on
        if (!adapter.getActionMode()) {
            adapter.setActionModeOn(true);
        }

        FileTransferEntity entity = adapter.getItems().get(i);
        entity.is_selected = !entity.is_selected;
        adapter.getItems().set(i, entity);
        adapter.notifyItemChanged(i);
    }

    public void startContextualActionMode() {
        if (actionMode == null) {
            // start the actionMode
            actionMode = activity.startSupportActionMode(new ActionModeCallback());
        }
    }

    public abstract TransferAction getTransferAction();

    public abstract void deleteSelectedItems(List<FileTransferEntity> list);

    public abstract void restartSelectedItems(List<FileTransferEntity> list);

    protected void loadData() {
        getViewModel().loadData(getTransferAction());
    }

    private void notifyDataChanged(List<FileTransferEntity> list) {
        if (CollectionUtils.isEmpty(list)) {
            showEmptyTip();
        } else {
            adapter.submitList(list);
        }
    }

    private void showEmptyTip() {
        if (getTransferAction() == TransferAction.DOWNLOAD) {
            showAdapterTip(R.string.no_download_tasks);
        } else {
            showAdapterTip(R.string.no_upload_tasks);
        }
    }

    private void showAdapterTip(int textRes) {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(textRes);
        tipView.setOnClickListener(v -> loadData());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    public void cancelSelectItems() {
        if (adapter == null) return;

        adapter.setActionModeOn(false);

        if (actionMode != null) {
            actionMode.finish();
            actionMode = null;
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

            switch (item.getItemId()) {
                case R.id.action_mode_delete: {
                    deleteSelectedItems(selectedList);
                    cancelSelectItems();
                }
                break;
                case R.id.action_mode_restart: {
                    restartSelectedItems(selectedList);
                    cancelSelectItems();
                }
                break;
                case R.id.action_mode_select_all: {
                    if (!allItemsSelected) {
                        adapter.setItemSelected(true);
                    } else {
                        adapter.setItemSelected(false);
                    }
                    updateContextualActionBar();

                    allItemsSelected = !allItemsSelected;
                }
                break;
                default:
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
            actionMode = null;
        }

    }
}