package com.seafile.seadroid2.ui.activity;

import android.app.AlertDialog;
import android.content.*;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Log;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.util.Utils;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

public class ShareToSeafileActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "ShareToSeafileActivity";

    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_dialog_fragment_tag";
    private static final int CHOOSE_COPY_MOVE_DEST_REQUEST = 1;

    private TransferService mTxService;
    private ServiceConnection mConnection;
    private ArrayList<String> localPathList;
    private Intent dstData;
    private Boolean isFinishActivity = false;
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Intent intent = getIntent();

        Bundle extras = intent.getExtras();
        if (extras != null) {
            Object extraStream = extras.get(Intent.EXTRA_STREAM);

            if (extraStream instanceof ArrayList) {
                ArrayList<Uri> array = (ArrayList<Uri>)extraStream;
                ConcurrentAsyncTask.execute(new LoadFileFromUriTask(), array.toArray(new Uri[]{}));
            } else if (extraStream instanceof Uri) {
                ConcurrentAsyncTask.execute(new LoadFileFromUriTask(), (Uri)extraStream);
            }
        }
    }

    class LoadFileFromUriTask extends AsyncTask<Uri, Void, File[]> {

        @Override
        protected File[] doInBackground(Uri... uriList) {
            if (uriList == null)
                return null;

            List<File> fileList = new ArrayList<File>();
            for (Uri uri: uriList) {
                File tempDir = new File(DataManager.getExternalTempDirectory(), "upload-"+System.currentTimeMillis());
                File tempFile = new File(tempDir, Utils.getFilenamefromUri(ShareToSeafileActivity.this, uri));

                InputStream in = null;
                OutputStream out = null;

                try {
                    tempDir.mkdirs();

                    if (!tempFile.createNewFile()) {
                        throw new RuntimeException("could not create temporary file");
                    }

                    in = getContentResolver().openInputStream(uri);
                    out = new FileOutputStream(tempFile);
                    IOUtils.copy(in, out);

                } catch (IOException e) {
                    Log.d(DEBUG_TAG, "Could not open requested document", e);
                    tempFile = null;
                } catch (RuntimeException e) {
                    Log.d(DEBUG_TAG, "Could not open requested document", e);
                    tempFile = null;
                } finally {
                    IOUtils.closeQuietly(in);
                    IOUtils.closeQuietly(out);
                }
                fileList.add(tempFile);
            }
            return fileList.toArray(new File[]{});
        }

        @Override
        protected void onPostExecute(File... fileList) {
            if (fileList == null || fileList.length == 0) {
                ToastUtils.show(ShareToSeafileActivity.this, R.string.not_supported_share);
                finish();
                return;
            }

            if(localPathList == null) localPathList = Lists.newArrayList();
            for (File f: fileList) {
                localPathList.add(f.getAbsolutePath());
            }

            Log.d(DEBUG_TAG, "share " + localPathList);
            Intent chooserIntent = new Intent(ShareToSeafileActivity.this, SeafilePathChooserActivity.class);
            startActivityForResult(chooserIntent, CHOOSE_COPY_MOVE_DEST_REQUEST);
        }
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (mTxService != null) {
            unbindService(mConnection);
            mTxService = null;
        }

        super.onDestroy();
    }

    /**
     * update the file to its latest version to avoid duplicate files
     *
     * @param account
     * @param repoName
     * @param repoID
     * @param targetDir
     * @param localFilePaths
     */
    private void addUpdateTask(Account account, String repoName, String repoID, String targetDir, ArrayList<String> localFilePaths) {
        bindTransferService(account, repoName, repoID, targetDir, localFilePaths, true);
    }

    /**
     * upload the file, which may lead up to duplicate files
     *
     * @param account
     * @param repoName
     * @param repoID
     * @param targetDir
     * @param localFilePaths
     */
    private void addUploadTask(Account account, String repoName, String repoID, String targetDir, ArrayList<String> localFilePaths) {
        bindTransferService(account, repoName, repoID, targetDir, localFilePaths, false);
    }

    /**
     * Trying to bind {@link TransferService} in order to upload files after the service was connected
     *
     * @param account
     * @param repoName
     * @param repoID
     * @param targetDir
     * @param localPaths
     * @param update
     *          update the file to avoid duplicates if true,
     *          upload directly, otherwise.
     *
     */
    private void bindTransferService(final Account account, final String repoName, final String repoID,
                                        final String targetDir, final ArrayList<String> localPaths, final boolean update) {
        // start transfer service
        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);

        mConnection = new ServiceConnection() {
            @Override
            public void onServiceConnected(ComponentName className, IBinder service) {
                TransferBinder binder = (TransferBinder) service;
                mTxService = binder.getService();
                for (String path : localPaths) {
                    mTxService.addUploadTask(account, repoID, repoName, targetDir,
                            path, update, false);
                    Log.d(DEBUG_TAG, path + (update ? " updated" : " uploaded"));
                }
                ToastUtils.show(ShareToSeafileActivity.this, R.string.upload_started);
                finish();
            }

            @Override
            public void onServiceDisconnected(ComponentName arg0) {
                mTxService = null;
            }
        };
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode != CHOOSE_COPY_MOVE_DEST_REQUEST) {
            return;
        }

        isFinishActivity = true;

        if (resultCode == RESULT_OK) {
            if (!Utils.isNetworkOn()) {
                ToastUtils.show(this, R.string.network_down);
                return;
            }
            dstData = data;
            String dstRepoId, dstRepoName, dstDir;
            Account account;
            dstRepoName = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
            dstRepoId = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
            dstDir = dstData.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
            account = dstData.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
            notifyFileOverwriting(account, dstRepoName, dstRepoId, dstDir);
            Log.i(DEBUG_TAG, "CHOOSE_COPY_MOVE_DEST_REQUEST returns");
        }
    }

    @Override
    protected void onPostResume() {
        super.onPostResume();
        /*if (dstData != null) {

            String dstRepoId, dstRepoName, dstDir;
            Account account;
            dstRepoName = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
            dstRepoId = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
            dstDir = dstData.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
            account = (Account)dstData.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
            addUploadTask(account, dstRepoName, dstRepoId, dstDir, localPath);
            Log.d(DEBUG_TAG, "dstRepoName: " + dstRepoName);
            Log.d(DEBUG_TAG, "dstDir: " + dstDir);
        }

        if(isFinishActivity) {
            Log.d(DEBUG_TAG, "finish!");
            isFinishActivity = false;
            finish();
        }*/
    }

    /**
     * Popup a dialog to notify user if allow to overwrite the file.
     * There are two buttons in the dialog, "Allow duplicate" and "Overwrite".
     * "Allow duplicate" will upload the file again, while "Overwrite" will update (overwrite) the file.
     *
     * @param account
     * @param repoName
     * @param repoID
     * @param targetDir
     */
    private void notifyFileOverwriting(final Account account,
                                       final String repoName,
                                       final String repoID,
                                       final String targetDir) {
        boolean fileExistent = false;
        DataManager dm = new DataManager(account);
        List<SeafDirent> dirents = dm.getCachedDirents(repoID, targetDir);
        if (dirents != null) {
            for (String path : localPathList) {
                for (SeafDirent dirent : dirents) {
                    if (dirent.isDir())
                        continue;
                    if (Utils.fileNameFromPath(path).equals(dirent.getTitle())) {
                        fileExistent = true;
                        break;
                    }
                }
            }

            if (fileExistent) {
                AlertDialog.Builder builder = new SeafileStyleDialogBuilder(this)
                        .setTitle(getString(R.string.overwrite_existing_file_title))
                        .setMessage(getString(R.string.overwrite_existing_file_msg))
                        .setPositiveButton(R.string.yes, new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialog, int which) {
                                addUpdateTask(account, repoName, repoID, targetDir, localPathList);
                                if(isFinishActivity) {
                                    Log.d(DEBUG_TAG, "finish!");
                                    finish();
                                }
                            }
                        })
                        .setNeutralButton(R.string.cancel, new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialog, int which) {
                                if(isFinishActivity) {
                                    Log.d(DEBUG_TAG, "finish!");
                                    finish();
                                }
                            }
                        })
                        .setNegativeButton(R.string.no,
                                new DialogInterface.OnClickListener() {
                                    @Override
                                    public void onClick(DialogInterface dialog, int which) {
                                        addUploadTask(account, repoName, repoID, targetDir, localPathList);
                                        if (isFinishActivity) {
                                            Log.d(DEBUG_TAG, "finish!");
                                            finish();
                                        }
                                    }
                                });
                builder.show();
            } else {
                if (!Utils.isNetworkOn()) {
                    ToastUtils.show(this, R.string.network_down);
                    return;
                }

                // asynchronously check existence of the file from server
                asyncCheckDrientFromServer(account, repoName, repoID, targetDir);
            }
        }
    }

    private void asyncCheckDrientFromServer(Account account,
                                            String repoName,
                                            String repoID,
                                            String targetDir) {

        CheckDirentExistentTask task = new CheckDirentExistentTask(account, repoName, repoID, targetDir);
        ConcurrentAsyncTask.execute(task);
    }

    class CheckDirentExistentTask extends AsyncTask<Void, Void, Void> {

        private Account account;
        private String repoName;
        private String repoID;
        private String targetDir;
        private DataManager dm;
        private boolean fileExistent = false;

        public CheckDirentExistentTask(Account account,
                                       String repoName,
                                       String repoID,
                                       String targetDir) {
            this.account = account;
            this.repoName = repoName;
            this.repoID = repoID;
            this.targetDir = targetDir;
            dm = new DataManager(account);
        }

        @Override
        protected Void doInBackground(Void... params) {
            List<SeafDirent> dirents = null;
            try {
                dirents = dm.getDirentsFromServer(repoID, targetDir);
            } catch (SeafException e) {
                Log.e(DEBUG_TAG, e.getMessage() + e.getCode());
            }
            boolean existent = false;
            if (dirents != null) {
                for (String path : localPathList) {
                    for (SeafDirent dirent : dirents) {
                        if (dirent.isDir())
                            continue;
                        if (Utils.fileNameFromPath(path).equals(dirent.getTitle())) {
                            existent = true;
                            break;
                        }
                    }
                }
            }
            if (!existent)
                // upload the file directly
                addUploadTask(account, repoName, repoID, targetDir, localPathList);

            return null;
        }

        @Override
        protected void onPostExecute(Void aVoid) {
            if (fileExistent)
                ToastUtils.show(ShareToSeafileActivity.this, R.string.overwrite_existing_file_exist);

            finish();
        }
    }
}
