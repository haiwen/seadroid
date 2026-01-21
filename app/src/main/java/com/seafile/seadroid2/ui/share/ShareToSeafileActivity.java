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

import androidx.activity.OnBackPressedCallback;
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
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.selector.obj.ObjSelectorActivity;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import io.reactivex.Observable;
import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Predicate;
import io.reactivex.schedulers.Schedulers;

public class ShareToSeafileActivity extends BaseActivityWithVM<ShareToSeafileViewModel> {
    public static final String TAG = "ShareToSeafileActivity";

    private ActivityShareToSeafileBinding binding;
    private String dstRepoId, dstRepoName, dstDir;
    private ShareParser.ShareResult shareResult;
    private Account account;
    private Disposable disposable;
    private ActivityResultLauncher<Intent> objSelectorLauncher;

    @Override
    public void onDestroy() {
        super.onDestroy();

        if (disposable != null && !disposable.isDisposed()) {
            disposable.dispose();
        }
    }

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityShareToSeafileBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        registerResultLauncher();

        initViewModel();

        initWorkerBusObserver();

        getOnBackPressedDispatcher().addCallback(this, new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
//                BackupThreadExecutor.getInstance().stopShareToSeafileUpload();

                finish();
            }
        });

        binding.progressBar.setIndeterminate(true);

        checkReceiveParams();
    }

    private void launchObjSelector() {
        //launch obj selector activity
        Bundle bundle = new Bundle();
        bundle.putBoolean("isFilterUnavailable", false);
        bundle.putBoolean("isAddStarredGroup", true);
        Intent intent = ObjSelectorActivity.getIntent(this, ObjSelectType.ACCOUNT, ObjSelectType.DIR, bundle);
        objSelectorLauncher.launch(intent);
    }


    private void initViewModel() {
        getViewModel().getActionLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                SLogs.d(TAG, "can start file upload worker? " + aBoolean);
                binding.progressBar.setIndeterminate(false);

                BackupThreadExecutor.getInstance().runShareToSeafileUploadTask();
            }
        });
    }

    private void checkReceiveParams() {
        Intent intent = getIntent();
        if (intent == null) {
            Toasts.show("Invalid shared content");
            finish();
            return;
        }

        String action = intent.getAction();
        String type = intent.getType();

        final ArrayList<Uri> fileUris = new ArrayList<>();

        SLogs.d(TAG, "action: " + action, "type: " + type);

        if (Intent.ACTION_SEND.equals(action) || Intent.ACTION_SEND_MULTIPLE.equals(action)) {
            shareResult = ShareParser.parseSharedContent(intent, this);
            if (!TextUtils.isEmpty(shareResult.plainText) && CollectionUtils.isEmpty(shareResult.uriList)) {
                SLogs.d(TAG, "text content：" + shareResult.plainText);
                Toasts.show(R.string.not_supported_share);
                finish();
                return;
            }

            for (Uri uri : shareResult.uriList) {
                SLogs.d(TAG, "Receive Uri：" + uri);

                fileUris.add(uri);
            }
        }


        if (CollectionUtils.isEmpty(fileUris)) {
            SLogs.d(TAG, "no shared files");
            finish();
            return;
        }


        disposable = isSafeSharedUri(fileUris.get(0))
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        if (aBoolean) {
                            launchObjSelector();
                        } else {
                            Toasts.show(R.string.not_supported);
                            finish();
                        }
                    }
                });

    }

    private void registerResultLauncher() {

        objSelectorLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {

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

    }

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

        return GlobalTransferCacheList.SHARE_FILE_TO_SEAFILE_QUEUE.getById(tId);
    }

    private void notifyFileOverwriting() {
        if (shareResult == null || CollectionUtils.isEmpty(shareResult.uriList)) {
            return;
        }

        getViewModel().checkRemoteExists(this, account, dstRepoId, dstRepoName, dstDir, shareResult.uriList, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean b) throws Exception {
                if (b) {
                    showExistsDialog(ShareToSeafileActivity.this, shareResult.uriList);
                } else {
                    SLogs.d(TAG, "gen transfer model, and insert into queue, and upload it");
                    getViewModel().upload(ShareToSeafileActivity.this, account, dstRepoId, dstRepoName, dstDir, shareResult.uriList, false);
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

    /**
     * Security-critical check:
     * 0、如果uri 是空，返回 false
     * 1、不接收非 content 协议的文件
     * 2、不接收 '/data/data/'目录和'/data/user/'目录里的文件
     * 3、不接收文件路径包含包名路径
     */
    private Single<Boolean> isSafeSharedUri(Uri uri) {
        return Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                // 0
                if (uri == null) {
                    emitter.onSuccess(false);
                    return;
                }

                // 1、Reject file:// outright
                if (!"content".equals(uri.getScheme())) {
                    emitter.onSuccess(false);
                    return;
                }

                //
                if (uri.toString().contains("/data/data") || uri.toString().contains("/data/user")) {
                    emitter.onSuccess(false);
                    return;
                }

                // 2、Verify system-granted read capability
                try {
                    ContentResolver resolver = getContentResolver();
                    try (java.io.InputStream is = resolver.openInputStream(uri)) {
                        if (is == null) {
                            emitter.onSuccess(false);
                            return;
                        }
                    }
                } catch (SecurityException | IOException e) {
                    SLogs.w(TAG, "Cannot read shared uri: " + uri, e);
                    emitter.onSuccess(false);
                    return;
                }


                // 4、Verify path is not in private app directory, except cache
                String streamToUpload = getSharedFilePath(uri);
                if (TextUtils.isEmpty(streamToUpload)) {

                    emitter.onSuccess(true);
                    return;
                }

                // 3、Verify path does not contain package name
                if (streamToUpload.contains(getPackageName())) {
                    SLogs.w(TAG, "Rejected uri with package name: " + uri);
                    emitter.onSuccess(false);
                    return;
                }

                // 
                if (streamToUpload.startsWith("/data/data") || streamToUpload.contains("/data/user")) {
                    emitter.onSuccess(false);
                    return;
                }

                //
                emitter.onSuccess(true);
            }
        });
    }

    private String getSharedFilePath(Uri uri) {
        if (uri == null) {
            return null;
        }

        if (Objects.equals(uri.getScheme(), "file")) {
            return uri.getPath();
        }

        ContentResolver contentResolver = getContentResolver();
        String filePath;
        try (Cursor cursor = contentResolver.query(uri, null, null, null, null)) {
            if (cursor == null || !cursor.moveToFirst()) {
                return null;
            }

            int dataIndex = cursor.getColumnIndexOrThrow(MediaStore.Images.Media.DATA);
            if (dataIndex == -1) {
                return null;
            }
            filePath = cursor.getString(dataIndex);
        }
        return filePath;
    }

}
