package com.seafile.seadroid2.ui.transfer_list;

import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.view.ActionMode;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;

public abstract class TransferListFragment extends BaseFragment {
    private LayoutFrameSwipeRvBinding binding;
    protected TransferListAdapter adapter;
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
        binding.swipeRefreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                loadData();
            }
        });
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
        loadData();
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

        adapter.addOnItemChildClickListener(R.id.expandable_toggle_button, (baseQuickAdapter, view, i) -> {
            showBottomSheet(adapter.getItems().get(i));
        });

        helper = new QuickAdapterHelper.Builder(adapter)
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

        getViewModel().getTransferListLiveData().observe(getViewLifecycleOwner(), new Observer<List<Object>>() {
            @Override
            public void onChanged(List<Object> list) {
                submitData(list);
            }
        });
    }

    private void submitData(List<Object> list) {
        resort(list);

        adapter.submitList(list);
    }

    public void showBottomSheet(Object entity) {
        int rid = R.menu.bottom_sheet_op_transfer_list;
        BottomSheetMenuFragment.Builder builder = BottomSheetHelper.buildSheet(getActivity(), rid, menuItem -> {
            int itemId = menuItem.getItemId();

            if (itemId == R.id.delete) {
                onBottomSheetFileDelete(entity);
            }
        });

        builder.removeMenu(R.id.upload);
        builder.removeMenu(R.id.download);

        builder.show(getChildFragmentManager());
    }

    protected void onBottomSheetFileDelete(Object entity) {
        if (TransferAction.DOWNLOAD == getTransferAction()) {
            showDeleteDownloadConfirmDialog((TransferModel) entity);
        } else {
            showDeleteUploadConfirmDialog((TransferModel) entity);
        }
    }

    //
    private void showDeleteDownloadConfirmDialog(TransferModel model) {
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

                try {
                    ToastUtils.showLong(R.string.deleted);
                    doDelete(model, booleans[0]);
                    removeSpecialEntity(model.getId());
                } catch (ExecutionException | InterruptedException e) {
                    throw new RuntimeException(e);
                } finally {
                    dialog.dismiss();
                }
            }
        });

        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

    private void showDeleteUploadConfirmDialog(TransferModel model) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
        builder.setTitle(R.string.delete_records);

        builder.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {

                try {
                    ToastUtils.showLong(R.string.deleted);
                    doDelete(model, false);
                    removeSpecialEntity(model.getId());
                } catch (ExecutionException | InterruptedException e) {
                    throw new RuntimeException(e);
                } finally {
                    dialog.dismiss();
                }

            }
        });

        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

    private void doDelete(TransferModel transferModel, boolean isDeleteLocalFile) throws ExecutionException, InterruptedException {
        if (TransferDataSource.DOWNLOAD == transferModel.data_source) {
            if (transferModel.transfer_status == TransferStatus.IN_PROGRESS) {
                BackgroundJobManagerImpl.getInstance().cancelDownloadWorker();
            }

            GlobalTransferCacheList.DOWNLOAD_QUEUE.remove(transferModel.getId());

            if (isDeleteLocalFile) {
                FileUtils.delete(transferModel.target_path);
            }

            BackgroundJobManagerImpl.getInstance().startDownloadWorker();

        } else if (TransferDataSource.FILE_BACKUP == transferModel.data_source) {
            GlobalTransferCacheList.FILE_UPLOAD_QUEUE.remove(transferModel.getId());

            if (transferModel.transfer_status == TransferStatus.IN_PROGRESS) {
                BackgroundJobManagerImpl.getInstance().cancelFolderBackupWorker();
            }

        } else if (TransferDataSource.FOLDER_BACKUP == transferModel.data_source) {
            GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.remove(transferModel.getId());

            if (transferModel.transfer_status == TransferStatus.IN_PROGRESS) {
                BackgroundJobManagerImpl.getInstance().startFolderBackupChain(true);
            }
        } else if (TransferDataSource.ALBUM_BACKUP == transferModel.data_source) {
            GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.remove(transferModel.getId());

            if (transferModel.transfer_status == TransferStatus.IN_PROGRESS) {
                BackgroundJobManagerImpl.getInstance().startMediaBackupChain(true);
            }
        }
    }


    public abstract TransferAction getTransferAction();

    protected void removeSpecialEntity(String uid) {
        Integer integer = positionMap.get(uid);
        if (integer == null) {
            return;
        }

        adapter.notifyItemRemoved(integer);
        adapter.getItems().remove(integer.intValue());

        resort(adapter.getItems());
    }

    protected void resort(List<Object> list) {
        //
        positionMap.clear();

        if (CollectionUtils.isEmpty(list)) {
            return;
        }

        for (int i = 0; i < list.size(); i++) {
            Object o = list.get(i);
            if (o instanceof TransferModel) {
                TransferModel u = (TransferModel) o;
                positionMap.put(u.getId(), i);
            }
        }
    }

    protected void loadData() {
        positionMap.clear();
        getViewModel().loadData(getTransferAction(), 0, pageSize);
    }

    protected TransferModel getUploadModel(String tId) {
        if (TextUtils.isEmpty(tId)) {
            return null;
        }

        TransferModel u1 = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getById(tId);
        if (u1 != null) {
            return u1;
        }

        TransferModel u2 = GlobalTransferCacheList.FILE_UPLOAD_QUEUE.getById(tId);
        if (u2 != null) {
            return u2;
        }

        TransferModel u3 = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getById(tId);
        if (u3 != null) {
            return u3;
        }

        TransferModel u4 = GlobalTransferCacheList.DOWNLOAD_QUEUE.getById(tId);
        if (u4 != null) {
            return u4;
        }
        return null;
    }

    public void notifyProgressById(TransferModel progressTransferModel, String event) {
        if (progressTransferModel == null) {
            return;
        }

        Integer i = positionMap.getOrDefault(progressTransferModel.getId(), -1);
        if (i == null || i == -1) {
            return;
        }

        TransferModel transferModel = (TransferModel) adapter.getItems().get(i);
        if (transferModel == null) {
            return;
        }

        Bundle bundle = null;
        if (TransferEvent.EVENT_FILE_TRANSFER_FAILED.equals(event)) {
            transferModel.transferred_size = 0;
            transferModel.transfer_status = TransferStatus.FAILED;
            transferModel.err_msg = progressTransferModel.err_msg;

        } else if (TransferEvent.EVENT_FILE_TRANSFER_SUCCESS.equals(event)) {
            transferModel.transferred_size = progressTransferModel.transferred_size;
            transferModel.transfer_status = TransferStatus.SUCCEEDED;

        } else if (TransferEvent.EVENT_FILE_IN_TRANSFER.equals(event)) {
            transferModel.transferred_size = progressTransferModel.transferred_size;
            transferModel.transfer_status = TransferStatus.IN_PROGRESS;

            bundle = new Bundle();
            bundle.putLong(TransferWorker.KEY_TRANSFER_TRANSFERRED_SIZE, progressTransferModel.transferred_size);
            bundle.putLong(TransferWorker.KEY_TRANSFER_TOTAL_SIZE, progressTransferModel.file_size);
        }

        adapter.getItems().set(i, transferModel);
        if (bundle != null) {
            adapter.notifyItemChanged(i, bundle);
        } else {
            adapter.notifyItemChanged(i);
        }
    }
}