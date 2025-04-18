package com.seafile.seadroid2.framework.worker.download;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class DownloadFileScannerWorker extends TransferWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(DownloadFileScannerWorker.class.getSimpleName().getBytes());

    private final DownloadNotificationHelper notificationHelper;

    public DownloadFileScannerWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationHelper = new DownloadNotificationHelper(context);
    }

    @NonNull
    @Override
    public Result doWork() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        String direntIdStr = getInputData().getString(DATA_DIRENT_LIST_KEY);
        if (!TextUtils.isEmpty(direntIdStr)) {
            ForegroundInfo foregroundInfo = notificationHelper.getForegroundNotification(R.string.download_waiting);
            showForegroundAsync(foregroundInfo);
        }

        //multiple download
        if (TextUtils.isEmpty(direntIdStr)) {
            return returnSuccess();
        }

        //send a scan event
        sendWorkerEvent(TransferDataSource.DOWNLOAD, TransferEvent.EVENT_SCANNING);

        String[] direntIds = direntIdStr.split(",");
        List<String> ids = Arrays.asList(direntIds);

        List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByIdsSync(ids);
        if (CollectionUtils.isEmpty(direntModels)) {
            return Result.success();
        }

        for (DirentModel direntModel : direntModels) {
            try {
                if (direntModel.isDir()) {
                    List<DirentRecursiveFileModel> list = fetchRecursiveFiles(direntModel);
                    insertIntoDbWhenDirentIsDir(account, direntModel, list);
                } else {
                    insertIntoDbWhenDirentIsFile(account, direntModel);
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        //success
        return returnSuccess();
    }

    protected Result returnSuccess() {
        sendWorkerEvent(TransferDataSource.DOWNLOAD, TransferEvent.EVENT_SCAN_FINISH);
        return Result.success();
    }


    /**
     *
     */
    private void insertIntoDbWhenDirentIsFile(Account account, DirentModel pendingModel) throws IOException {

        TransferModel transferModel = new TransferModel();
        transferModel.save_to = SaveTo.DB;
        transferModel.repo_id = pendingModel.repo_id;
        transferModel.repo_name = pendingModel.repo_name;
        transferModel.related_account = pendingModel.related_account;
        transferModel.file_name = pendingModel.name;
        transferModel.file_size = pendingModel.size;

        if (pendingModel.parent_dir.endsWith("/")) {
            transferModel.full_path = String.format("%s%s", pendingModel.parent_dir, pendingModel.name);
        } else {
            transferModel.full_path = String.format("%s/%s", pendingModel.parent_dir, pendingModel.name);
        }
        transferModel.setParentPath(Utils.getParentPath(transferModel.full_path));
        transferModel.target_path = DataManager.getLocalRepoFile(account, transferModel.repo_id, transferModel.repo_name, transferModel.full_path).getAbsolutePath();

        transferModel.transfer_status = TransferStatus.WAITING;
        transferModel.data_source = TransferDataSource.DOWNLOAD;
        transferModel.created_at = System.nanoTime();
        transferModel.transfer_strategy = ExistingFileStrategy.REPLACE;
        transferModel.setId(transferModel.genStableId());

        GlobalTransferCacheList.DOWNLOAD_QUEUE.put(transferModel);
    }

    /**
     * insert into db
     */
    private void insertIntoDbWhenDirentIsDir(Account account, DirentModel parentDirent, List<DirentRecursiveFileModel> list) {

        if (CollectionUtils.isEmpty(list)) {
            return;
        }

        for (DirentRecursiveFileModel model : list) {
            TransferModel transferModel = new TransferModel();
            transferModel.save_to = SaveTo.DB;
            transferModel.repo_id = parentDirent.repo_id;
            transferModel.repo_name = parentDirent.repo_name;
            transferModel.related_account = parentDirent.related_account;
            transferModel.file_name = model.name;
            transferModel.file_size = model.size;

            if (model.parent_dir.endsWith("/")) {
                transferModel.full_path = String.format("%s%s", model.parent_dir, model.name);
            } else {
                transferModel.full_path = String.format("%s/%s", model.parent_dir, model.name);
            }
            transferModel.setParentPath(Utils.getParentPath(transferModel.full_path));
            transferModel.target_path = DataManager.getLocalRepoFile(account, transferModel.repo_id, transferModel.repo_name, transferModel.full_path).getAbsolutePath();

            transferModel.transfer_status = TransferStatus.WAITING;
            transferModel.data_source = TransferDataSource.DOWNLOAD;
            transferModel.created_at = System.nanoTime();
            transferModel.transfer_strategy = ExistingFileStrategy.REPLACE;
            transferModel.setId(transferModel.genStableId());
            SLogs.d("download: " + transferModel.full_path);
            GlobalTransferCacheList.DOWNLOAD_QUEUE.put(transferModel);
        }

    }

    /**
     * get recursive files from server
     */
    private List<DirentRecursiveFileModel> fetchRecursiveFiles(DirentModel direntModel) throws IOException {
        retrofit2.Response<List<DirentRecursiveFileModel>> res = HttpIO.getCurrentInstance().execute(FileService.class).getDirRecursiveFileCall(direntModel.repo_id, direntModel.full_path).execute();
        if (!res.isSuccessful()) {
            return Collections.emptyList();
        }

        return res.body();
    }

}
