package com.seafile.seadroid2.ui.transfer_list;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.DiffUtil;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.databinding.ItemTransferListBinding;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.util.ArrayList;
import java.util.List;

public class TransferListAdapter extends BaseAdapter<FileTransferEntity, TransferItemViewHolder> {
    private boolean actionModeOn;

    private TransferAction transferAction;

    /**
     * -1 no limited
     * 0 no this value
     * >=1 max count
     */
    private int selectedMaxCount = 1;

    public void setSelectedMaxLimitCount(int selectedMaxCount) {
        this.selectedMaxCount = selectedMaxCount;
    }

    public void setTransferAction(TransferAction transferAction) {
        this.transferAction = transferAction;
    }

    @NonNull
    @Override
    protected TransferItemViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemTransferListBinding binding = ItemTransferListBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new TransferItemViewHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull TransferItemViewHolder viewHolder, int i, @Nullable FileTransferEntity item) {
        if (item == null) {
            return;
        }

        onBindHolder(viewHolder, item);
    }

    @Override
    protected void onBindViewHolder(@NonNull TransferItemViewHolder holder, int position, @Nullable FileTransferEntity item, @NonNull List<?> payloads) {
        super.onBindViewHolder(holder, position, item, payloads);

        if (CollectionUtils.isEmpty(payloads)) {
            return;
        }

        if (item == null) {
            return;
        }

        Bundle bundle = (Bundle) payloads.get(0);
        long transferredSize = bundle.getLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, 0);
        int percent = bundle.getInt(TransferWorker.KEY_DATA_PROGRESS, 0);
        onBindPayloadHolder(holder, item, transferredSize, percent);
    }

    private void onBindHolder(TransferItemViewHolder holder, FileTransferEntity entity) {

        //action mode
        if (actionModeOn) {
            holder.binding.itemMultiSelect.setVisibility(View.VISIBLE);
            if (entity.is_checked) {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_checked);
            } else {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
            }
        } else {
            holder.binding.itemMultiSelect.setVisibility(View.GONE);
            holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
        }

        //target path
        String targetPath = Utils.pathJoin(entity.repo_name, entity.getParent_path());
        holder.binding.transferTargetPath.setText(targetPath);

        //file name
        holder.binding.transferFileName.setText(entity.file_name);

        //icon
        int iconId = Icons.getFileIcon(entity.file_name);
        holder.binding.transferFileIcon.setImageResource(iconId);

        holder.binding.transferTime.setText(Utils.translateCommitTime(entity.created_at));

        //
        long totalSize = entity.file_size;
        long transferredSize = entity.transferred_size;
        String sizeStr = Utils.readableFileSize(totalSize);

        boolean progressBarVisible = false;
        boolean isRed = false;

        int stateTextRes = 0;
        if (TransferStatus.WAITING == entity.transfer_status) {
            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.download_waiting;
            } else {
                stateTextRes = R.string.upload_waiting;
            }
        } else if (TransferStatus.IN_PROGRESS == entity.transfer_status) {
            sizeStr = String.format("%s / %s",
                    Utils.readableFileSize(transferredSize),
                    Utils.readableFileSize(totalSize));

            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.notification_download_started_title;
            } else {
                stateTextRes = R.string.notification_upload_started_title;
            }

            int percent;
            if (totalSize == 0) {
                percent = 0;
            } else {
                percent = (int) (transferredSize * 100 / totalSize);
            }

            holder.binding.transferFileProgressBar.setProgress(percent);

            progressBarVisible = true;

        } else if (TransferStatus.FAILED == entity.transfer_status) {
            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.download_failed;
            } else {
                stateTextRes = R.string.upload_failed;
            }
            isRed = true;
        } else if (TransferStatus.CANCELLED == entity.transfer_status) {
            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.download_cancelled;
            } else {
                stateTextRes = R.string.upload_cancelled;
            }
            isRed = true;
        } else if (TransferStatus.SUCCEEDED == entity.transfer_status) {
            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.download_finished;
            } else {
                stateTextRes = R.string.upload_finished;
            }
        }

        if (stateTextRes != 0) {
            holder.binding.transferFileState.setText(stateTextRes);
        } else {
            holder.binding.transferFileState.setText(null);
        }

        if (TransferResult.NO_RESULT == entity.transfer_result) {
            holder.binding.transferFileErrorState.setVisibility(View.GONE);
            holder.binding.transferFileErrorState.setText(null);
        } else if (TransferResult.TRANSMITTED == entity.transfer_result) {
            holder.binding.transferFileErrorState.setVisibility(View.GONE);
            holder.binding.transferFileErrorState.setText(null);
        } else {
            holder.binding.transferFileErrorState.setVisibility(View.VISIBLE);
            holder.binding.transferFileErrorState.setText(entity.transfer_result.toString());
        }

        if (isRed) {
            holder.binding.transferFileState.setTextColor(ContextCompat.getColor(getContext(), R.color.red));
        } else {
            holder.binding.transferFileState.setTextColor(ContextCompat.getColor(getContext(), R.color.item_subtitle_color));
        }

        holder.binding.transferFileProgressBar.setVisibility(progressBarVisible ? View.VISIBLE : View.GONE);
        holder.binding.transferFileSize.setText(sizeStr);
    }

    private void onBindPayloadHolder(TransferItemViewHolder holder, FileTransferEntity entity, long transferredSize, int percent) {
//
        long totalSize = entity.file_size;
        String sizeStr = Utils.readableFileSize(totalSize);

        sizeStr = String.format("%s / %s", Utils.readableFileSize(transferredSize), sizeStr);
        holder.binding.transferFileSize.setText(sizeStr);

        holder.binding.transferFileProgressBar.setProgress(percent);
    }

    public void notifyDataChanged(List<FileTransferEntity> list) {
        if (CollectionUtils.isEmpty(list)) {
            submitList(list);
            return;
        }

        if (CollectionUtils.isEmpty(getItems())) {
            submitList(list);
            return;
        }

        DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffUtil.Callback() {
            @Override
            public int getOldListSize() {
                return getItems().size();
            }

            @Override
            public int getNewListSize() {
                return list.size();
            }

            @Override
            public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
                String oldClassName = getItems().get(oldItemPosition).getClass().getName();
                String newClassName = list.get(newItemPosition).getClass().getName();
                if (!oldClassName.equals(newClassName)) {
                    return false;
                }

                FileTransferEntity newT = getItems().get(oldItemPosition);
                FileTransferEntity oldT = list.get(newItemPosition);
                return TextUtils.equals(newT.uid, oldT.uid);
            }

            @Override
            public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
                String oldClassName = getItems().get(oldItemPosition).getClass().getName();
                String newClassName = list.get(newItemPosition).getClass().getName();
                if (!oldClassName.equals(newClassName)) {
                    return false;
                }

                FileTransferEntity newT = getItems().get(oldItemPosition);
                FileTransferEntity oldT = list.get(newItemPosition);

                return TextUtils.equals(newT.uid, oldT.uid)
                        && TextUtils.equals(newT.full_path, oldT.full_path)
                        && TextUtils.equals(newT.target_path, oldT.target_path)
                        && TextUtils.equals(newT.repo_id, oldT.repo_id)
                        && TextUtils.equals(newT.repo_name, oldT.repo_name)
                        && TextUtils.equals(newT.related_account, oldT.related_account)
                        && TextUtils.equals(newT.file_id, oldT.file_id)
                        && TextUtils.equals(newT.getParent_path(), oldT.getParent_path())
                        && TextUtils.equals(newT.file_name, oldT.file_name)
                        && TextUtils.equals(newT.file_format, oldT.file_format)
                        && TextUtils.equals(newT.mime_type, oldT.mime_type)
                        && TextUtils.equals(newT.file_md5, oldT.file_md5)
                        && newT.data_source == oldT.data_source
                        && newT.file_size == oldT.file_size
//                        && newT.is_block == oldT.is_block
                        && newT.is_copy_to_local == oldT.is_copy_to_local
                        && newT.file_strategy == oldT.file_strategy
                        && newT.created_at == oldT.created_at
                        && newT.modified_at == oldT.modified_at
                        && newT.action_end_at == oldT.action_end_at
                        && newT.transfer_status == oldT.transfer_status
                        && newT.transfer_result == oldT.transfer_result
                        && newT.transfer_action == oldT.transfer_action;

            }
        });

        setItems(list);
        diffResult.dispatchUpdatesTo(this);
    }

    public void setActionModeOn(boolean actionModeOn) {
        this.actionModeOn = actionModeOn;

        if (!actionModeOn) {
            setItemSelected(false);
        }

        notifyItemRangeChanged(0, getItemCount());
    }

    public boolean getActionMode() {
        return actionModeOn;
    }

    public void setItemSelected(boolean itemSelected) {
        for (FileTransferEntity item : getItems()) {
            item.is_checked = itemSelected;
        }

        notifyItemRangeChanged(0, getItemCount());
    }

    public List<FileTransferEntity> getSelectedList() {
        List<FileTransferEntity> list = new ArrayList<>();
        for (FileTransferEntity item : getItems()) {
            if (item.is_checked) {
                list.add(item);
            }
        }
        return list;
    }

    /**
     * @return is selected?
     */
    public boolean selectItemByMode(int position) {
        FileTransferEntity item = getItems().get(position);

        //single
        if (selectedMaxCount == 1) {
            int selectedPosition = getSelectedPositionByMode();
            if (selectedPosition == position) {
                item.is_checked = !item.is_checked;
                notifyItemChanged(selectedPosition);
                return item.is_checked;

            } else if (selectedPosition > -1) {
                //Deselect an item that has already been selected
                getItems().get(selectedPosition).is_checked = false;
                notifyItemChanged(selectedPosition);

                item.is_checked = true;
                notifyItemChanged(position);
            } else {
                item.is_checked = true;
                notifyItemChanged(position);
            }
        } else {
            long selectedCount = getSelectedCountByMode();
            if (selectedCount >= selectedMaxCount) {
                return false;
            }

            item.is_checked = !item.is_checked;
            notifyItemChanged(position);

            return item.is_checked;
        }

        return true;
    }


    private int getSelectedPositionByMode() {
        for (int i = 0; i < getItems().size(); i++) {
            if (getItems().get(i).is_checked) {
                return i;
            }
        }
        return -1;
    }

    public long getSelectedCountByMode() {
        return getItems().stream()
                .filter(f -> f.is_checked)
                .count();
    }
}
