package com.seafile.seadroid2.framework.worker;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.net.Uri;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.documentfile.provider.DocumentFile;
import androidx.work.ForegroundInfo;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.datastore.SyncExclusionManager;
import com.seafile.seadroid2.framework.datastore.SyncRule;
import com.seafile.seadroid2.framework.datastore.SyncRuleManager;
import com.seafile.seadroid2.framework.datastore.SyncStateManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DeleteDirentModel;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.notification.FolderSyncNotificationHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Periodic background worker that syncs files bidirectionally between
 * Seafile repo folders and local Android folders based on configured sync rules.
 */
public class FolderSyncWorker extends Worker {
    public static final String TAG = "FolderSyncWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(TAG.getBytes());

    private final FolderSyncNotificationHelper notificationHelper;

    public FolderSyncWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
        notificationHelper = new FolderSyncNotificationHelper(context);
    }

    @NonNull
    @Override
    public Result doWork() {
        SafeLogs.d(TAG, "Folder sync worker started");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            SafeLogs.d(TAG, "No account logged in");
            return Result.success();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "No network connection");
            return Result.success();
        }

        List<SyncRule> rules = SyncRuleManager.getAll();
        if (rules.isEmpty()) {
            SafeLogs.d(TAG, "No sync rules configured");
            return Result.success();
        }

        int downloadCount = 0;
        int uploadCount = 0;

        for (SyncRule rule : rules) {
            if (!rule.enabled) continue;

            try {
                int[] counts = processRule(account, rule);
                downloadCount += counts[0];
                uploadCount += counts[1];
            } catch (Exception e) {
                SafeLogs.e(TAG, "Error processing sync rule: "
                        + rule.getDisplaySummary() + " - " + e.getMessage());
            }
        }

        SafeLogs.d(TAG, "Sync scan complete. Downloads: "
                + downloadCount + ", Uploads: " + uploadCount);

        if (downloadCount == 0 && uploadCount == 0) {
            return Result.success();
        }

        // Promote to foreground service so Android keeps the process alive
        showForegroundNotification();

        // Kick off download and upload tasks
        if (downloadCount > 0) {
            BackupThreadExecutor.getInstance().runDownloadTask();
        }
        if (uploadCount > 0) {
            BackupThreadExecutor.getInstance().runFolderSyncUploadTask();
        }

        // Wait for transfers to finish (keeps the Worker alive)
        waitForTransfersComplete();

        SafeLogs.d(TAG, "Folder sync transfers complete");
        return Result.success();
    }

    /**
     * Promote this Worker to a foreground service with a persistent notification.
     * This prevents Android from killing the process during long transfers.
     */
    private void showForegroundNotification() {
        try {
            ForegroundInfo foregroundInfo = notificationHelper.getForegroundNotification();
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
                try {
                    setForegroundAsync(foregroundInfo);
                } catch (ForegroundServiceStartNotAllowedException e) {
                    SLogs.e("Cannot start foreground service: " + e.getMessage());
                }
            } else {
                setForegroundAsync(foregroundInfo);
            }
        } catch (Exception e) {
            SafeLogs.e(TAG, "Failed to show foreground notification: " + e.getMessage());
        }
    }

    /**
     * Block until both downloads and uploads triggered by this sync cycle are done.
     * Polls every 2 seconds, respecting Worker cancellation via {@link #isStopped()}.
     */
    private void waitForTransfersComplete() {
        while (!isStopped()) {
            boolean downloading = BackupThreadExecutor.getInstance().isDownloading();
            boolean uploading = BackupThreadExecutor.getInstance().isFolderSyncUploading();
            if (!downloading && !uploading) {
                break;
            }
            try {
                Thread.sleep(2000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }

    /**
     * Process a single sync rule using 4-way comparison with sync state index.
     *
     * <p>The sync state tracks files that were previously synced. This lets us
     * distinguish "new server file" from "user deleted locally":
     * <ul>
     *   <li>On server + in previous state + NOT local → local delete → delete from server</li>
     *   <li>On server + NOT in previous state → new file → download</li>
     *   <li>Local + in previous state + NOT on server → server delete → delete locally</li>
     *   <li>Local + NOT in previous state → new file → upload</li>
     * </ul>
     *
     * @return int[]{downloadCount, uploadCount}
     */
    private int[] processRule(Account account, SyncRule rule) throws IOException {
        // 1. Fetch remote file list recursively
        List<DirentRecursiveFileModel> remoteFiles =
                fetchRemoteFiles(rule.repoId, rule.remotePath);

        // 2. Scan local folder via SAF
        Uri treeUri = Uri.parse(rule.localUri);
        DocumentFile localRoot;
        try {
            localRoot = DocumentFile.fromTreeUri(
                    getApplicationContext(), treeUri);
        } catch (SecurityException e) {
            SafeLogs.e(TAG, "SAF permission revoked for rule: "
                    + rule.getDisplaySummary());
            return new int[]{0, 0};
        }
        if (localRoot == null || !localRoot.exists()) {
            SafeLogs.d(TAG, "Local folder not accessible for rule: "
                    + rule.getDisplaySummary());
            return new int[]{0, 0};
        }

        // Build sets of relative paths for comparison
        Map<String, DirentRecursiveFileModel> remoteMap = new HashMap<>();
        for (DirentRecursiveFileModel rf : remoteFiles) {
            String fullPath = rf.getParent_dir() + rf.name;
            String relativePath = rule.getRelativePath(fullPath);
            remoteMap.put(relativePath, rf);
        }

        Map<String, DocumentFile> localMap = new HashMap<>();
        scanLocalFiles(localRoot, "", localMap);

        // 3. Load the previously-synced file index and per-file exclusions
        Set<String> previousState =
                SyncStateManager.getSyncedFiles(rule.id);
        Set<String> exclusions =
                SyncExclusionManager.getExcludedFiles(rule.id);
        Set<String> newState = new HashSet<>();

        // Remove already-queued downloads for files that are now excluded
        // (per-file exclusions or rule-level filters)
        GlobalTransferCacheList.DOWNLOAD_QUEUE.removeIf(tm -> {
            if (!rule.repoId.equals(tm.repo_id) || tm.full_path == null) {
                return false;
            }
            String rel = rule.getRelativePath(tm.full_path);
            return exclusions.contains(rel)
                    || rule.isFilteredOut(tm.file_name, tm.file_size);
        });

        int downloadCount = 0;
        int uploadCount = 0;

        // 4a. Files on server
        Set<String> deletedRemotePaths = new HashSet<>();
        for (Map.Entry<String, DirentRecursiveFileModel> entry
                : remoteMap.entrySet()) {
            String rel = entry.getKey();
            DirentRecursiveFileModel rf = entry.getValue();
            boolean isLocal = localMap.containsKey(rel);
            boolean wasSynced = previousState.contains(rel);

            // Check if file is excluded by rule-level filters or per-file exclusion
            boolean isExcluded = exclusions.contains(rel)
                    || rule.isFilteredOut(rf.name, rf.size);

            if (isExcluded) {
                // File is excluded — if it exists locally, delete it
                if (isLocal) {
                    deleteLocalFile(localRoot, rel);
                    SafeLogs.d(TAG, "Deleted excluded file locally: " + rel);
                }
                // Do NOT add to newState and do NOT propagate delete to server
                continue;
            }

            if (isLocal) {
                // In sync — keep in state
                newState.add(rel);
            } else if (wasSynced) {
                // Was synced before but user deleted locally → propagate
                boolean deleted = deleteRemoteFile(
                        rule.repoId, rule.remotePath, rel);
                if (deleted) {
                    deletedRemotePaths.add(rel);
                } else {
                    // Keep in state to retry next cycle
                    newState.add(rel);
                }
            } else {
                // New server file → download
                enqueueDownload(account, rule, entry.getValue());
                downloadCount++;
                // Do NOT add to newState — file isn't local yet.
                // Next sync will re-enqueue if download hasn't completed.
            }
        }

        // 4b. Files only local
        for (Map.Entry<String, DocumentFile> entry : localMap.entrySet()) {
            String rel = entry.getKey();
            boolean isRemote = remoteMap.containsKey(rel);
            boolean wasSynced = previousState.contains(rel);

            if (isRemote) {
                // Already handled above
                continue;
            }
            if (wasSynced) {
                // Was synced before but deleted on server → remove locally
                deleteLocalFile(localRoot, rel);
            } else {
                // New local file → upload
                enqueueUpload(account, rule, rel, entry.getValue());
                uploadCount++;
                // Do NOT add to newState — file isn't on server yet.
                // Next sync will re-enqueue if upload hasn't completed.
            }
        }

        // 5. Clean up empty remote directories after file deletions
        if (!deletedRemotePaths.isEmpty()) {
            cleanupEmptyRemoteDirs(
                    rule.repoId, rule.remotePath,
                    deletedRemotePaths, newState);
        }

        // 6. Persist new sync state (merge with concurrent addSyncedFile calls)
        SyncStateManager.setSyncedFiles(rule.id, previousState, newState);

        return new int[]{downloadCount, uploadCount};
    }

    /**
     * After deleting files, remove any remote directories that are now empty.
     * Directories are sorted deepest-first so nested dirs are removed before parents.
     */
    private void cleanupEmptyRemoteDirs(
            String repoId, String remotePath,
            Set<String> deletedPaths, Set<String> survivingPaths) {
        // Collect parent directories of deleted files
        Set<String> candidateDirs = new HashSet<>();
        for (String deleted : deletedPaths) {
            int slash = deleted.lastIndexOf('/');
            while (slash > 0) {
                candidateDirs.add(deleted.substring(0, slash));
                slash = deleted.substring(0, slash).lastIndexOf('/');
            }
        }

        // Only delete dirs that have no surviving files underneath
        // Sort deepest-first (longest path first) so children go before parents
        List<String> dirsToDelete = new ArrayList<>();
        for (String dir : candidateDirs) {
            String prefix = dir + "/";
            boolean hasLiveFile = false;
            for (String alive : survivingPaths) {
                if (alive.startsWith(prefix)) {
                    hasLiveFile = true;
                    break;
                }
            }
            if (!hasLiveFile) {
                dirsToDelete.add(dir);
            }
        }
        // Sort longest first → deepest directories deleted first
        dirsToDelete.sort((a, b) -> b.length() - a.length());

        for (String dir : dirsToDelete) {
            deleteRemoteDir(repoId, remotePath, dir);
        }
    }

    /**
     * Delete a file from the Seafile server via the REST API.
     *
     * @return true if deletion succeeded, false on failure (caller should retry)
     */
    private boolean deleteRemoteFile(
            String repoId, String remotePath, String relativePath) {
        String fullPath = remotePath;
        if (!fullPath.endsWith("/")) fullPath += "/";
        fullPath += relativePath;

        try {
            retrofit2.Response<DeleteDirentModel> resp =
                    HttpIO.getCurrentInstance()
                            .execute(FileService.class)
                            .deleteFileSync(repoId, fullPath)
                            .execute();
            if (resp.isSuccessful()) {
                SafeLogs.d(TAG, "Deleted remote file: " + fullPath);
                return true;
            } else {
                SafeLogs.e(TAG, "Failed to delete remote file: "
                        + fullPath + " (HTTP " + resp.code() + ")");
                return false;
            }
        } catch (IOException e) {
            SafeLogs.e(TAG, "Error deleting remote file: "
                    + fullPath + " - " + e.getMessage());
            return false;
        }
    }

    /**
     * Delete an empty directory from the Seafile server via the REST API.
     */
    private void deleteRemoteDir(
            String repoId, String remotePath, String relativePath) {
        String fullPath = remotePath;
        if (!fullPath.endsWith("/")) fullPath += "/";
        fullPath += relativePath;

        try {
            retrofit2.Response<DeleteDirentModel> resp =
                    HttpIO.getCurrentInstance()
                            .execute(FileService.class)
                            .deleteDirSync(repoId, fullPath)
                            .execute();
            if (resp.isSuccessful()) {
                SafeLogs.d(TAG, "Deleted remote dir: " + fullPath);
            } else {
                SafeLogs.e(TAG, "Failed to delete remote dir: "
                        + fullPath + " (HTTP " + resp.code() + ")");
            }
        } catch (IOException e) {
            SafeLogs.e(TAG, "Error deleting remote dir: "
                    + fullPath + " - " + e.getMessage());
        }
    }

    /**
     * Delete a locally-synced file via SAF when it was removed on the server.
     */
    private void deleteLocalFile(DocumentFile localRoot, String relativePath) {
        DocumentFile target = resolveLocalFile(localRoot, relativePath);
        if (target != null && target.exists()) {
            if (target.delete()) {
                SafeLogs.d(TAG, "Deleted local file: " + relativePath);
            } else {
                SafeLogs.e(TAG, "Failed to delete local file: "
                        + relativePath);
            }
        }
    }

    /**
     * Resolve a relative path like "subdir/file.mp3" into a DocumentFile
     * by traversing the SAF tree.
     */
    private DocumentFile resolveLocalFile(
            DocumentFile root, String relativePath) {
        String[] parts = relativePath.split("/");
        DocumentFile current = root;
        for (String part : parts) {
            if (current == null) return null;
            current = current.findFile(part);
        }
        return current;
    }

    private List<DirentRecursiveFileModel> fetchRemoteFiles(
            String repoId, String path) throws IOException {
        retrofit2.Response<List<DirentRecursiveFileModel>> response =
                HttpIO.getCurrentInstance()
                        .execute(FileService.class)
                        .getDirRecursiveFileCall(repoId, path)
                        .execute();
        if (!response.isSuccessful() || response.body() == null) {
            return Collections.emptyList();
        }
        return response.body();
    }

    /**
     * Recursively scan a SAF DocumentFile tree, collecting files with their relative paths.
     */
    private void scanLocalFiles(DocumentFile dir, String prefix, Map<String, DocumentFile> result) {
        if (dir == null || !dir.exists()) return;
        DocumentFile[] children = dir.listFiles();
        if (children == null) return;

        for (DocumentFile child : children) {
            String name = child.getName();
            if (name == null) continue;

            String relativePath = prefix.isEmpty() ? name : prefix + "/" + name;
            if (child.isDirectory()) {
                scanLocalFiles(child, relativePath, result);
            } else {
                result.put(relativePath, child);
            }
        }
    }

    private void enqueueDownload(Account account, SyncRule rule,
            DirentRecursiveFileModel remoteFile) {
        String parentDir = remoteFile.getParent_dir();
        if (parentDir == null) parentDir = "/";
        String fullPath = parentDir + remoteFile.name;

        TransferModel tm = new TransferModel();
        tm.save_to = SaveTo.DB;
        tm.repo_id = rule.repoId;
        tm.repo_name = rule.repoName;
        tm.related_account = account.getSignature();
        tm.file_name = remoteFile.name;
        tm.file_size = remoteFile.size;
        tm.full_path = fullPath;
        tm.setParentPath(Utils.getParentPath(fullPath));
        tm.target_path = DataManager.getLocalFileCachePath(
                account, rule.repoId, rule.repoName, fullPath)
                .getAbsolutePath();
        tm.transfer_status = TransferStatus.WAITING;
        tm.data_source = FeatureDataSource.DOWNLOAD;
        tm.created_at = System.nanoTime();
        tm.transfer_strategy = ExistingFileStrategy.REPLACE;
        tm.setId(tm.genStableId());

        GlobalTransferCacheList.DOWNLOAD_QUEUE.put(tm);
    }

    private void enqueueUpload(Account account, SyncRule rule,
            String relativePath, DocumentFile localFile) {
        // Compute remote target path
        String remotePath = rule.remotePath;
        if (!remotePath.endsWith("/")) remotePath += "/";
        String targetPath = remotePath + relativePath;

        TransferModel tm = new TransferModel();
        tm.save_to = SaveTo.DB;
        tm.repo_id = rule.repoId;
        tm.repo_name = rule.repoName;
        tm.related_account = account.getSignature();
        tm.file_name = localFile.getName();
        tm.file_size = localFile.length();
        // content:// URI — handled by ProgressUriRequestBody
        tm.full_path = localFile.getUri().toString();
        tm.target_path = targetPath;
        tm.setParentPath(Utils.getParentPath(targetPath));
        tm.transfer_status = TransferStatus.WAITING;
        tm.data_source = FeatureDataSource.FOLDER_SYNC;
        tm.created_at = System.nanoTime();
        tm.transfer_strategy = ExistingFileStrategy.APPEND;
        tm.setId(tm.genStableId());

        GlobalTransferCacheList.FOLDER_SYNC_QUEUE.put(tm);
    }
}
