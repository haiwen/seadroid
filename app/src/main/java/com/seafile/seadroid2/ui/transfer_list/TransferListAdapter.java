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

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.databinding.ItemGroupItemBinding;
import com.seafile.seadroid2.databinding.ItemTransferListBinding;
import com.seafile.seadroid2.databinding.ItemUnsupportedBinding;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.repo.vh.UnsupportedViewHolder;
import com.seafile.seadroid2.ui.viewholder.GroupItemViewHolder;

import java.util.List;

public class TransferListAdapter extends BaseMultiAdapter<Object> {
    private TransferAction transferAction;

    public void setTransferAction(TransferAction transferAction) {
        this.transferAction = transferAction;
    }

    public TransferListAdapter() {
        addItemType(AbsLayoutItemType.LOCAL_LIST, new OnMultiItem<Object, TransferItemViewHolder>() {
            @NonNull
            @Override
            public TransferItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemTransferListBinding binding = ItemTransferListBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new TransferItemViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull TransferItemViewHolder viewHolder, int i, @Nullable Object baseModel) {
                onBindLocalList(viewHolder, (TransferModel) baseModel);
            }

            @Override
            public void onBind(@NonNull TransferItemViewHolder holder, int position, @Nullable Object item, @NonNull List<?> payloads) {
                super.onBind(holder, position, item, payloads);

                if (CollectionUtils.isEmpty(payloads)) {
                    return;
                }

                if (item == null) {
                    return;
                }

                Bundle bundle = (Bundle) payloads.get(0);
                onBindLocalListPayloadHolder(holder, bundle);
            }
        })
                .addItemType(AbsLayoutItemType.DB_LIST, new OnMultiItem<Object, TransferItemViewHolder>() {
                    @NonNull
                    @Override
                    public TransferItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                        ItemTransferListBinding binding = ItemTransferListBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                        return new TransferItemViewHolder(binding);
                    }

                    @Override
                    public void onBind(@NonNull TransferItemViewHolder viewHolder, int i, @Nullable Object model) {
                        onBindDbList(viewHolder, (FileBackupStatusEntity) model);
                    }
                })
                .addItemType(AbsLayoutItemType.GROUP_ITEM, new OnMultiItem<Object, GroupItemViewHolder>() {
                    @NonNull
                    @Override
                    public GroupItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                        ItemGroupItemBinding binding = ItemGroupItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                        return new GroupItemViewHolder(binding);
                    }

                    @Override
                    public void onBind(@NonNull GroupItemViewHolder holder, int i, @Nullable Object model) {
                        onBindGroup(holder, model);
                    }
                })
                .addItemType(AbsLayoutItemType.NOT_SUPPORTED, new OnMultiItem<Object, UnsupportedViewHolder>() {
                    @NonNull
                    @Override
                    public UnsupportedViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                        ItemUnsupportedBinding binding = ItemUnsupportedBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                        return new UnsupportedViewHolder(binding);
                    }

                    @Override
                    public void onBind(@NonNull UnsupportedViewHolder unsupportedViewHolder, int i, @Nullable Object o) {

                    }
                })
                .onItemViewType(new OnItemViewTypeListener<Object>() {
                    @Override
                    public int onItemViewType(int i, @NonNull List<?> list) {
                        if (list.get(i) instanceof GroupItemModel) {
                            return AbsLayoutItemType.GROUP_ITEM;
                        } else if (list.get(i) instanceof TransferModel) {
                            return AbsLayoutItemType.LOCAL_LIST;
                        } else if (list.get(i) instanceof FileBackupStatusEntity) {
                            return AbsLayoutItemType.DB_LIST;
                        }
                        return AbsLayoutItemType.NOT_SUPPORTED;

                    }
                });
    }

    private void onBindGroup(GroupItemViewHolder holder, Object model) {
        GroupItemModel groupItemModel = (GroupItemModel) model;
        holder.binding.itemGroupTitle.setText(groupItemModel.title);
        holder.binding.itemGroupExpand.setVisibility(View.GONE);
    }

    private void onBindLocalList(TransferItemViewHolder holder, TransferModel model) {
        holder.binding.itemMultiSelect.setVisibility(View.GONE);
        holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);

        //target path
        if (!TextUtils.isEmpty(model.repo_name)) {
            String targetPath = Utils.pathJoin(model.repo_name, model.getParentPath());
            holder.binding.transferTargetPath.setText(targetPath);
        } else {
            holder.binding.transferTargetPath.setText(model.getParentPath());
        }

        holder.binding.expandableToggleButton.setVisibility(View.VISIBLE);

        //file name
        holder.binding.transferFileName.setText(model.file_name);

        //icon
        int iconId = Icons.getFileIcon(model.file_name);
        holder.binding.transferFileIcon.setImageResource(iconId);

        holder.binding.transferTime.setText(Utils.translateCommitTime(model.created_at));

        //
        long totalSize = model.file_size;
        long transferredSize = model.transferred_size;
        String sizeStr = Utils.readableFileSize(totalSize);

        boolean progressBarVisible = false;
        boolean isRed = false;

        int stateTextRes = 0;
        if (TransferStatus.WAITING == model.transfer_status) {
            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.download_waiting;
            } else {
                stateTextRes = R.string.upload_waiting;
            }
        } else if (TransferStatus.IN_PROGRESS == model.transfer_status) {
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

        } else if (TransferStatus.FAILED == model.transfer_status) {
            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.download_failed;
            } else {
                stateTextRes = R.string.upload_failed;
            }
            isRed = true;
        } else if (TransferStatus.CANCELLED == model.transfer_status) {
            if (transferAction == TransferAction.DOWNLOAD) {
                stateTextRes = R.string.download_cancelled;
            } else {
                stateTextRes = R.string.upload_cancelled;
            }
            isRed = true;
        } else if (TransferStatus.SUCCEEDED == model.transfer_status) {
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

        if (TextUtils.isEmpty(model.err_msg)) {
            holder.binding.transferFileErrorState.setVisibility(View.GONE);
            holder.binding.transferFileErrorState.setText(null);
        } else if (TextUtils.equals(model.err_msg, TransferResult.TRANSMITTED.name())) {
            holder.binding.transferFileErrorState.setVisibility(View.GONE);
            holder.binding.transferFileErrorState.setText(null);
        } else {
            holder.binding.transferFileErrorState.setVisibility(View.VISIBLE);
            holder.binding.transferFileErrorState.setText(model.err_msg);
        }

        if (isRed) {
            holder.binding.transferFileState.setTextColor(ContextCompat.getColor(getContext(), R.color.red));
        } else {
            holder.binding.transferFileState.setTextColor(ContextCompat.getColor(getContext(), R.color.item_subtitle_color));
        }

        holder.binding.transferFileProgressBar.setVisibility(progressBarVisible ? View.VISIBLE : View.GONE);
        holder.binding.transferFileSize.setText(sizeStr);
    }

    private void onBindLocalListPayloadHolder(TransferItemViewHolder holder, Bundle bundle) {
        if (bundle.containsKey(TransferWorker.KEY_TRANSFER_TRANSFERRED_SIZE)) {
            long transferredSize = bundle.getLong(TransferWorker.KEY_TRANSFER_TRANSFERRED_SIZE, 0);
            long totalSize = bundle.getLong(TransferWorker.KEY_TRANSFER_TOTAL_SIZE, 0);

            String sizeStr = Utils.readableFileSize(totalSize);

            sizeStr = String.format("%s / %s", Utils.readableFileSize(transferredSize), sizeStr);
            holder.binding.transferFileSize.setText(sizeStr);

            int p = calc(transferredSize, totalSize);
            holder.binding.transferFileProgressBar.setProgress(p);
        }
    }


    private void onBindDbList(TransferItemViewHolder holder, FileBackupStatusEntity entity) {
        holder.binding.itemMultiSelect.setVisibility(View.GONE);
        holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
        holder.binding.expandableToggleButton.setVisibility(View.GONE);

        //icon
        int iconId = Icons.getFileIcon(entity.file_name);
        holder.binding.transferFileIcon.setImageResource(iconId);

        //file name
        holder.binding.transferFileName.setText(entity.file_name);

        //target path
        String targetPath = Utils.pathJoin(entity.repo_name, entity.getParent_path());
        holder.binding.transferTargetPath.setText(targetPath);

        holder.binding.transferTime.setText(Utils.translateCommitTime(entity.created_at));

        holder.binding.transferFileErrorState.setVisibility(View.GONE);
        holder.binding.transferFileState.setVisibility(View.GONE);
        holder.binding.transferFileProgressBar.setVisibility(View.GONE);
        holder.binding.transferFileSize.setText(Utils.readableFileSize(entity.file_size));
    }


    public int calc(long cur, long total) {
        return (int) ((float) cur / (float) total * 100);
    }
}
