package com.seafile.seadroid2.worker;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.RepoDirMappingEntity;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.Locale;

public abstract class TransferWorker extends BaseWorker {
    public static final int SEGMENT_SIZE = 8192;
    public static final String DATA_PROGRESS_KEY = "data_progress_key";
    public static final String DATA_TRANSFERRED_SIZE_KEY = "data_transferred_size_key";
    public static final String DATA_TOTAL_SIZE_KEY = "data_total_size_key";

    public static final String DATA_DIRENT_KEY = "data_dirent_key";
    public static final String DATA_TRANSFER_KEY = "data_transfer_key";
    public static final String DATA_TRANSFER_NAME_KEY = "data_transfer_name_key";

    TransferWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    public File getLocalSaveDir(FileTransferEntity transferEntity) {
        String repoDir = getRepoDir(transferEntity.repo_id, transferEntity.repo_name);
        if (TextUtils.isEmpty(repoDir)) {
            return null;
        }

        String localPath = Utils.pathJoin(repoDir, transferEntity.full_path);

        //build valid file path and name
        localPath = com.seafile.seadroid2.util.FileUtils.buildValidFilePathName(localPath);

        File parentDir = new File(Utils.getParentPath(localPath));
        if (!parentDir.exists()) {
            // TODO should check if the directory creation succeeds
            parentDir.mkdirs();
        }

        return new File(localPath);
    }

    synchronized String getRepoDir(String repoId, String repoName) throws RuntimeException {
        File repoDir;

        RepoDirMappingEntity dbEntity = AppDatabase.getInstance().repoDirMappingDAO().getOneByRepoId(repoId);
        if (dbEntity != null) {
            repoDir = new File(getAccountDir(), dbEntity.repo_dir);
            if (!repoDir.exists()) {
                if (!repoDir.mkdirs()) {
                    throw new RuntimeException("Could not create library directory " + repoDir);
                }
            }
            return repoDir.getAbsolutePath();
        }

        String uniqueRepoName;
        int i = 0;
        while (true) {
            if (i == 0) {
                uniqueRepoName = repoName;
            } else {
                uniqueRepoName = repoName + " (" + i + ")";
            }
            repoDir = new File(getAccountDir(), uniqueRepoName);
            RepoDirMappingEntity mappingEntity = AppDatabase.getInstance().repoDirMappingDAO().getOneByUniqueName(getCurrentAccount().getSignature(), uniqueRepoName);
            boolean isDbExists = mappingEntity != null;


            if (!repoDir.exists() && !isDbExists) {
                // This repo dir does not exist yet, we can use it
                break;
            }
            i++;
        }

        if (!repoDir.mkdirs()) {
            throw new RuntimeException("Could not create repo directory " + uniqueRepoName
                    + "Phone storage space is insufficient or too many " + uniqueRepoName + " directory in phone");
        }

        RepoDirMappingEntity entity = new RepoDirMappingEntity();
        entity.repo_id = repoId;
        entity.repo_dir = uniqueRepoName;
        entity.related_account = getCurrentAccount().getSignature();
        AppDatabase.getInstance().repoDirMappingDAO().insert(entity);

        return repoDir.getAbsolutePath();

    }

    public String getAccountDir() {

        if (getCurrentAccount() == null) {
            return null;
        }

        String userName = getCurrentAccount().email;
        String server = Utils.stripSlashes(getCurrentAccount().getServerHost());

        // strip port, like :8000 in 192.168.1.116:8000
        if (server.contains(":")) {
            server = server.substring(0, server.indexOf(':'));
        }

        String p = String.format(Locale.getDefault(), "%s (%s)", userName, server);
        p = p.replaceAll("[^\\w\\d\\.@\\(\\) ]", "_");

        File mediaDir = StorageManager.getInstance().getMediaDir();
        String accountDir = Utils.pathJoin(mediaDir.getAbsolutePath(), p);

        return accountDir;
    }
}
