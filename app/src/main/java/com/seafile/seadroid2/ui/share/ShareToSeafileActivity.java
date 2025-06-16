package com.seafile.seadroid2.ui.share;

import android.app.Activity;
import android.content.ContentResolver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.text.TextUtils;
import android.util.Log;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.databinding.ActivityShareToSeafileBinding;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;

import java.util.ArrayList;
import java.util.List;

import io.reactivex.functions.Consumer;

public class ShareToSeafileActivity extends BaseActivityWithVM<ShareToSeafileViewModel> {
    public static final String TAG = "ShareToSeafileActivity";

    private ActivityShareToSeafileBinding binding;
    private String dstRepoId, dstRepoName, dstDir;
    private Account account;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityShareToSeafileBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        getViewModel().getActionLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                SLogs.d(TAG, "can start file upload worker? " + aBoolean);

                TransferService.startShareToSeafileUploadService(ShareToSeafileActivity.this);
            }
        });

        initWorkerBusObserver();

        //launch obj selector activity
        Bundle bundle = new Bundle();
        bundle.putBoolean("isFilterUnavailable", false);
        bundle.putBoolean("isAddStarredGroup", true);
        Intent intent = ObjSelectorActivity.getIntent(this, ObjSelectType.ACCOUNT, ObjSelectType.DIR, bundle);
        objSelectorLauncher.launch(intent);
    }

    private final ActivityResultLauncher<Intent> objSelectorLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {

        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                finish();
                return;
            }

            Intent intent = o.getData();
            if (intent == null) {
                finish();
                return;
            }

            account = intent.getParcelableExtra(ObjKey.ACCOUNT);
            dstRepoId = intent.getStringExtra(ObjKey.REPO_ID);
            dstRepoName = intent.getStringExtra(ObjKey.REPO_NAME);
            dstDir = intent.getStringExtra(ObjKey.DIR);

            SLogs.d(TAG, "account: " + account.getSignature(), "repoId: " + dstRepoId, "repoName: " + dstRepoName, "dir: " + dstDir);
            notifyFileOverwriting();
        }
    });

    private void initWorkerBusObserver() {
        BusHelper.getTransferProgressObserver().observe(this, new Observer<Bundle>() {
            @Override
            public void onChanged(Bundle bundle) {
                doBusWork(bundle);
            }
        });
    }

    private void doBusWork(Bundle map) {

        String dataSource = map.getString(TransferWorker.KEY_DATA_SOURCE);
        String statusEvent = map.getString(TransferWorker.KEY_DATA_STATUS);
        String result = map.getString(TransferWorker.KEY_DATA_RESULT);
        String transferId = map.getString(TransferWorker.KEY_TRANSFER_ID);
        int transferCount = map.getInt(TransferWorker.KEY_TRANSFER_COUNT);

        if (!TextUtils.equals(FeatureDataSource.SHARE_FILE_TO_SEAFILE.name(), dataSource)) {
            return;
        }

        SLogs.d(TAG, "on event: " + statusEvent, "dataSource: " + dataSource, "result: " + result);
        if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCANNING)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCAN_COMPLETE)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_IN_TRANSFER)) {
            TransferModel transferModel = getUploadModel(transferId);
            if (transferModel == null) {
                return;
            }

            binding.fileName.setText(transferModel.file_name);
            int percent;
            if (transferModel.file_size == 0) {
                percent = 0;
            } else {
                percent = (int) (transferModel.transferred_size * 100 / transferModel.file_size);
            }
            binding.progressText.setText(percent + "%");
            binding.progressBar.setProgress(percent);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_FAILED)) {
            binding.progressText.setText("0%");
            binding.progressBar.setProgress(0);
            Toasts.show(R.string.upload_failed);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS)) {
            binding.progressText.setText("100%");
            binding.progressBar.setProgress(100);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE)) {

            finish();
        }
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

    private void notifyFileOverwriting() {
        Intent intent = getIntent();
        if (intent == null) {
            Toasts.show("Invalid shared content");
            finish();
            return;
        }

        String action = intent.getAction();
        String type = intent.getType();

        ArrayList<Uri> fileUris = new ArrayList<>();
        if (Intent.ACTION_SEND.equals(action)) {
            if ("text/plain".equals(type)) {
                String sharedText = intent.getStringExtra(Intent.EXTRA_TEXT);
                SLogs.d(TAG, "Seafile receive content: " + sharedText);
                Toasts.show(R.string.not_supported_share);
            } else {
                Uri fileUri = (Uri) intent.getParcelableExtra(Intent.EXTRA_STREAM);
                if (fileUri != null) {
                    fileUris.add(fileUri);
                }
            }
        } else if (Intent.ACTION_SEND_MULTIPLE.equals(action)) {
            fileUris = intent.getParcelableArrayListExtra(Intent.EXTRA_STREAM);
        } else {
            // Handle other intents, such as being started from the home screen
        }

        if (CollectionUtils.isEmpty(fileUris)) {
            SLogs.d(TAG, "no shared files");
            finish();
            return;
        }

        SLogs.d(TAG, "fileUris: " + fileUris);

        ArrayList<Uri> finalFileUris = fileUris;
        getViewModel().checkRemoteExists(this, account, dstRepoId, dstRepoName, dstDir, fileUris, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean b) throws Exception {
                if (b) {
                    showExistsDialog(ShareToSeafileActivity.this, finalFileUris);
                } else {
                    SLogs.d(TAG, "gen transfer model, and insert into queue, and upload it");
                    getViewModel().upload(ShareToSeafileActivity.this, account, dstRepoId, dstRepoName, dstDir, finalFileUris, false);
                }
            }
        });
    }

    private void showExistsDialog(Context context, List<Uri> fileUris) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this)
                .setTitle(getString(R.string.overwrite_existing_file_title))
                .setMessage(getString(R.string.overwrite_existing_file_msg))
                .setPositiveButton(R.string.yes, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        getViewModel().upload(context, account, dstRepoId, dstRepoName, dstDir, fileUris, true);
                    }
                })
                .setNeutralButton(R.string.cancel, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        Log.d(TAG, "finish!");
                        finish();
                    }
                })
                .setNegativeButton(R.string.no,
                        new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialog, int which) {
                                getViewModel().upload(context, account, dstRepoId, dstRepoName, dstDir, fileUris, false);
                            }
                        });
        builder.show();
    }


    //
    private void loadSharedFiles(Object extraStream) {
//        if (extraStream instanceof ArrayList) {
//            ConcurrentAsyncTask.execute(new LoadSharedFileTask(),
//                    ((ArrayList<Uri>) extraStream).toArray(new Uri[]{}));
//        } else if (extraStream instanceof Uri) {
//            ConcurrentAsyncTask.execute(new LoadSharedFileTask(),
//                    (Uri) extraStream);
//        }

    }

    private String getSharedFilePath(Uri uri) {
        if (uri == null) {
            return null;
        }

        if (uri.getScheme().equals("file")) {
            return uri.getPath();
        } else {
            ContentResolver contentResolver = getContentResolver();
            Cursor cursor = contentResolver.query(uri, null, null, null, null);
            if (cursor == null || !cursor.moveToFirst()) {
                return null;
            }
            String filePath = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media.DATA));
            return filePath;
        }
    }


}
