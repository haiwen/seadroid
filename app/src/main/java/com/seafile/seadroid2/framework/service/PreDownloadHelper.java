package com.seafile.seadroid2.framework.service;

import android.content.Context;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public class PreDownloadHelper {
    private static final String TAG = "PreDownloadHelper";

    public static void preDownload(Context context, Account account, String direntIds) {
        String[] direntIdArr = direntIds.split(",");
        List<String> ids = Arrays.asList(direntIdArr);
        preDownload(context, account, ids);
    }

    public static void preDownload(Context context, Account account, List<String> ids) {
        CompletableFuture.runAsync(new Runnable() {
            @Override
            public void run() {

                SafeLogs.d(TAG, "start scan");

                if (CollectionUtils.isEmpty(ids)) {
                    return;
                }
                SafeLogs.d(TAG, "size: " + ids.size());

                if (ids.size() > 100) {
                    Toasts.show(R.string.download_waiting);
                    return;
                }

                List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByIdsSync(ids);
                if (CollectionUtils.isEmpty(direntModels)) {
                    return;
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
                        SafeLogs.e(TAG, e.getMessage());
                    }
                }

                //start download service
//                Intent intent = new Intent(context, DownloadService.class);
//                ContextCompat.startForegroundService(context, intent);
                TransferService.startDownloadService(context);
            }
        });
    }


    /**
     *
     */
    private static void insertIntoDbWhenDirentIsFile(Account account, DirentModel pendingModel) throws IOException {

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
        transferModel.target_path = DataManager.getLocalFileCachePath(account, transferModel.repo_id, transferModel.repo_name, transferModel.full_path).getAbsolutePath();

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
    private static void insertIntoDbWhenDirentIsDir(Account account, DirentModel parentDirent, List<DirentRecursiveFileModel> list) {

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
            transferModel.target_path = DataManager.getLocalFileCachePath(account, transferModel.repo_id, transferModel.repo_name, transferModel.full_path).getAbsolutePath();

            transferModel.transfer_status = TransferStatus.WAITING;
            transferModel.data_source = TransferDataSource.DOWNLOAD;
            transferModel.created_at = System.nanoTime();
            transferModel.transfer_strategy = ExistingFileStrategy.REPLACE;
            transferModel.setId(transferModel.genStableId());
            SafeLogs.d(TAG, transferModel.full_path);

            GlobalTransferCacheList.DOWNLOAD_QUEUE.put(transferModel);
        }
    }

    /**
     * get recursive files from server
     */
    private static List<DirentRecursiveFileModel> fetchRecursiveFiles(DirentModel direntModel) throws IOException {
        retrofit2.Response<List<DirentRecursiveFileModel>> res = HttpIO.getCurrentInstance().execute(FileService.class).getDirRecursiveFileCall(direntModel.repo_id, direntModel.full_path).execute();
        if (!res.isSuccessful()) {
            return Collections.emptyList();
        }

        return res.body();
    }
}
