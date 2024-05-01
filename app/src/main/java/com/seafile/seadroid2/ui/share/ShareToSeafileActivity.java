package com.seafile.seadroid2.ui.share;

import static com.seafile.seadroid2.ui.selector.ObjSelectorActivity.DATA_ACCOUNT;
import static com.seafile.seadroid2.ui.selector.ObjSelectorActivity.DATA_DIR;
import static com.seafile.seadroid2.ui.selector.ObjSelectorActivity.DATA_REPO_ID;
import static com.seafile.seadroid2.ui.selector.ObjSelectorActivity.DATA_REPO_NAME;

import android.app.Activity;
import android.content.ContentResolver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.util.Log;
import android.util.Pair;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;

import java.util.ArrayList;
import java.util.List;

import io.reactivex.functions.Consumer;

public class ShareToSeafileActivity extends BaseActivityWithVM<ShareToSeafileViewModel> {
    private static final String DEBUG_TAG = "ShareToSeafileActivity";

    private String dstRepoId, dstRepoName, dstDir;
    private Account account;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_share_to_seafile);


        getViewModel().getActionLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    finish();
                }
            }
        });

        Intent chooserIntent = new Intent(this, ObjSelectorActivity.class);
        objSelectorLauncher.launch(chooserIntent);

    }


    private final ActivityResultLauncher<Intent> objSelectorLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                finish();
                return;
            }

            Intent intent = o.getData();
            dstRepoId = intent.getStringExtra(DATA_REPO_ID);
            dstRepoName = intent.getStringExtra(DATA_REPO_NAME);
            dstDir = intent.getStringExtra(DATA_DIR);
            account = intent.getParcelableExtra(DATA_ACCOUNT);

            notifyFileOverwriting();
        }
    });

    private void notifyFileOverwriting() {

        Intent intent = getIntent();
        if (intent == null) {
            ToastUtils.showLong("Invalid shared content");
            finish();
            return;
        }

        String action = intent.getAction();
        String type = intent.getType();

        ArrayList<Uri> fileUris = new ArrayList<>();
        if (Intent.ACTION_SEND.equals(action)) {
            if ("text/plain".equals(type)) {
                String sharedText = intent.getStringExtra(Intent.EXTRA_TEXT);
                SLogs.d(sharedText);
            } else {
                Uri fileUri = (Uri) intent.getParcelableExtra(Intent.EXTRA_STREAM);
                fileUris = CollectionUtils.newArrayList(fileUri);
            }
        } else if (Intent.ACTION_SEND_MULTIPLE.equals(action)) {
            fileUris = intent.getParcelableArrayListExtra(Intent.EXTRA_STREAM);
        } else {
            // Handle other intents, such as being started from the home screen

        }


        ArrayList<Uri> finalFileUris = fileUris;
        getViewModel().checkRemoteExists(this, account, dstRepoId, dstRepoName, dstDir, fileUris, new Consumer<Pair<List<DirentModel>, List<DirentModel>>>() {
            @Override
            public void accept(Pair<List<DirentModel>, List<DirentModel>> listListPair) throws Exception {
                if (listListPair != null && listListPair.second != null && !listListPair.second.isEmpty()) {
                    showExistsDialog(ShareToSeafileActivity.this, finalFileUris, listListPair.first);
                } else {
                    getViewModel().upload(ShareToSeafileActivity.this, account, dstRepoId, dstRepoName, dstDir, finalFileUris, listListPair.first, false);
                }
            }
        });
    }

    private void showExistsDialog(Context context, List<Uri> fileUris, List<DirentModel> direntModels) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this)
                .setTitle(getString(R.string.overwrite_existing_file_title))
                .setMessage(getString(R.string.overwrite_existing_file_msg))
                .setPositiveButton(R.string.yes, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        getViewModel().upload(context, account, dstRepoId, dstRepoName, dstDir, fileUris, direntModels, true);
                    }
                })
                .setNeutralButton(R.string.cancel, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        Log.d(DEBUG_TAG, "finish!");
                        finish();
                    }
                })
                .setNegativeButton(R.string.no,
                        new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialog, int which) {
                                getViewModel().upload(context, account, dstRepoId, dstRepoName, dstDir, fileUris, direntModels, false);
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
