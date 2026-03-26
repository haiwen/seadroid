package com.seafile.seadroid2.ui.folder_sync;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.SyncExclusionManager;
import com.seafile.seadroid2.framework.datastore.SyncRule;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;

import java.util.ArrayList;
import java.util.List;

/**
 * RecyclerView adapter for the per-file toggle list.
 * Checking/unchecking immediately persists the exclusion state
 * and purges/stops matching downloads from the queue.
 */
public class SyncFileAdapter extends RecyclerView.Adapter<SyncFileAdapter.ViewHolder> {

    private final SyncRule rule;
    private final List<SyncFileItem> items = new ArrayList<>();

    public SyncFileAdapter(SyncRule rule) {
        this.rule = rule;
    }

    public void setItems(List<SyncFileItem> newItems) {
        items.clear();
        items.addAll(newItems);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.item_sync_file, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        SyncFileItem item = items.get(position);

        holder.tvFileName.setText(item.relativePath);
        holder.tvFileSize.setText(Utils.readableFileSize(item.fileSize));

        // Avoid triggering listener during bind
        holder.cbFile.setOnCheckedChangeListener(null);
        holder.cbFile.setChecked(item.included);

        holder.cbFile.setOnCheckedChangeListener((buttonView, isChecked) -> {
            item.included = isChecked;
            if (isChecked) {
                SyncExclusionManager.includeFile(rule.id, item.relativePath);
            } else {
                SyncExclusionManager.excludeFile(rule.id, item.relativePath);
                // Immediately remove from download queue if queued/in-progress
                removeFromDownloadQueue(item.relativePath);
            }
        });

        // Make entire row clickable to toggle
        holder.itemView.setOnClickListener(v -> {
            holder.cbFile.setChecked(!holder.cbFile.isChecked());
        });
    }

    /**
     * Remove any queued or actively downloading transfer for this file.
     */
    private void removeFromDownloadQueue(String relativePath) {
        // Remove from the pending queue
        GlobalTransferCacheList.DOWNLOAD_QUEUE.removeIf(tm ->
                rule.repoId.equals(tm.repo_id)
                && tm.full_path != null
                && relativePath.equals(rule.getRelativePath(tm.full_path))
        );

        // Cancel the active download if it matches this file
        String remotePath = rule.remotePath;
        if (!remotePath.endsWith("/")) remotePath += "/";
        String fullPath = remotePath + relativePath;
        String modelId = EncryptUtils.encryptMD5ToString(
                FeatureDataSource.DOWNLOAD.toString() + rule.repoId + fullPath);
        BackupThreadExecutor.getInstance()
                .stopSpecialTransmitter(modelId, FeatureDataSource.DOWNLOAD);
    }

    @Override
    public int getItemCount() {
        return items.size();
    }

    static class ViewHolder extends RecyclerView.ViewHolder {
        final CheckBox cbFile;
        final TextView tvFileName;
        final TextView tvFileSize;

        ViewHolder(@NonNull View itemView) {
            super(itemView);
            cbFile = itemView.findViewById(R.id.cb_file);
            tvFileName = itemView.findViewById(R.id.tv_file_name);
            tvFileSize = itemView.findViewById(R.id.tv_file_size);
        }
    }
}
