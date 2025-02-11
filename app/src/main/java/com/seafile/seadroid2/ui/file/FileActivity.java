package com.seafile.seadroid2.ui.file;

import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;

import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.databinding.FileActivityBinding;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;

import java.io.File;

import io.reactivex.functions.Consumer;
import kotlin.Triple;

/**
 * Display a file
 */
public class FileActivity extends BaseActivityWithVM<FileViewModel> implements Toolbar.OnMenuItemClickListener {

    private FileActivityBinding binding;
    private RepoModel repoModel;
    private File destinationFile;
    private String repoId;
    private String action;

    private DirentModel direntModel;

    public static Intent start(Context context, DirentModel direntModel, String action) {
        Intent starter = new Intent(context, FileActivity.class);
        starter.putExtra("dirent", direntModel);
        starter.putExtra("action", action);
        return starter;
    }

    public static Intent startFromStarred(Context context, StarredModel model, String action) {
        DirentModel direntModel = DirentModel.convertStarredModelToThis(model);
        return start(context, direntModel, action);
    }

    public static Intent startFromSearch(Context context, SearchModel model, String action) {
        DirentModel direntModel = DirentModel.convertSearchModelToThis(model);
        return start(context, direntModel, action);
    }

    public static Intent startFromActivity(Context context, ActivityModel model, String action) {
        DirentModel direntModel = DirentModel.convertActivityModelToThis(model);
        return start(context, direntModel, action);
    }


    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = FileActivityBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        Intent intent = getIntent();

        if (intent == null) {
            throw new IllegalArgumentException("missing args");
        }

        if (!intent.hasExtra("dirent")) {
            throw new IllegalArgumentException("missing args");
        }

        action = intent.getStringExtra("action");
        direntModel = intent.getParcelableExtra("dirent");
        if (null == direntModel) {
            throw new IllegalArgumentException("missing dirent args");
        }

        repoId = direntModel.repo_id;

        destinationFile = getLocalDestinationFile(repoId, direntModel.repo_name, direntModel.full_path);

        //
        binding.progressBar.setIndeterminate(true);

        initWidgets(direntModel.name);

        initViewModel();

        loadData();
    }


    private void initViewModel() {
        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException seafException) {
                onFileDownloadFailed(seafException);
            }
        });

        getViewModel().getProgressLiveData().observe(this, new Observer<Long[]>() {
            @Override
            public void onChanged(Long[] longs) {
                long transferredSize = longs[0];
                long totalSize = longs[1];
                onFileDownloadProgress(transferredSize, totalSize);
            }
        });

        getViewModel().getOutFileLiveData().observe(this, new Observer<File>() {
            @Override
            public void onChanged(File outFile) {
                onFileDownloaded(outFile, true);
            }
        });

        getViewModel().getCancelLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                ToastUtils.showLong(R.string.download_cancelled);
                finishWithCancel();
            }
        });
    }

    private void loadData() {
        getViewModel().loadFileDetail(repoId, direntModel.full_path, new Consumer<Triple<RepoModel, DirentFileModel, FileTransferEntity>>() {
            @Override
            public void accept(Triple<RepoModel, DirentFileModel, FileTransferEntity> triple) throws Exception {
                repoModel = triple.getFirst();
                DirentFileModel direntFileModel = triple.getSecond();
                FileTransferEntity fileTransfer = triple.getThird();

                direntModel.size = direntFileModel.size;

                ExistingFileStrategy strategy = checkFileStrategy(direntFileModel, fileTransfer);
                if (ExistingFileStrategy.APPEND == strategy) {
                    getViewModel().preDownload(repoModel, direntModel, destinationFile);
                } else if (ExistingFileStrategy.SKIP == strategy) {
                    onFileDownloaded(destinationFile, false);
                } else if (ExistingFileStrategy.ASK == strategy) {
                    showFileExistDialog(destinationFile);
                } else if (ExistingFileStrategy.NOT_FOUND_IN_REMOTE == strategy) {
                    onFileDownloadFailed(SeafException.NOT_FOUND_EXCEPTION);
                }
            }
        });
    }

    private void initWidgets(String titleBarName) {
        binding.fileName.setText(titleBarName);

        // icon
        binding.fileIcon.setImageResource(Icons.getFileIcon(titleBarName));

        binding.opCancel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                cancelDownload();
            }
        });

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(titleBarName);
    }

    private void cancelDownload() {
        getViewModel().cancelDownload(direntModel.repo_id, direntModel.full_path);
    }

    private ExistingFileStrategy checkFileStrategy(DirentFileModel direntFileModel, FileTransferEntity fileTransferEntity) {
        if (null == direntFileModel || TextUtils.isEmpty(direntFileModel.id)) {
            //has been deleted in remote repo
            return ExistingFileStrategy.NOT_FOUND_IN_REMOTE;
        }

        //locally not exists
        if (!destinationFile.exists()) {
            return ExistingFileStrategy.APPEND;
        }

        if (fileTransferEntity != null && fileTransferEntity.file_id.equals(direntModel.id)) {
            // IMPROVE: might be better to use MD5 or other ways.

            //Ask the user how to deal with it
            return ExistingFileStrategy.SKIP;
        }

        long fileLastModified = FileUtils.getFileLastModified(destinationFile);
        long direntLastModified = direntFileModel.getMtimeInMills();

        if (direntLastModified > fileLastModified) {
            return ExistingFileStrategy.ASK;
        } else if (fileLastModified > direntLastModified) {
            return ExistingFileStrategy.APPEND;
        }

        //Ask the user how to deal with it
        return ExistingFileStrategy.ASK;
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        File localFile = DataManager.getLocalRepoFile(account, repoId, repoName, fullPathInRepo);
        return localFile;
    }

    private void showFileExistDialog(final File destinationFile) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
        builder.setTitle(R.string.overwrite_existing_file_title);
        builder.setMessage(R.string.overwrite_existing_file_msg);

        builder.setNeutralButton(R.string.close, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                finish();
            }
        });

        builder.setNegativeButton(R.string.keep_both, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                dialog.dismiss();

                onFileDownloaded(destinationFile, false);
            }
        });

        builder.setPositiveButton(R.string.replace_local_file, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {

                getViewModel().preDownload(repoModel, direntModel, destinationFile);
            }
        });
        builder.show();
    }


    private void onFileDownloadProgress(long transferredSize, long totalSize) {
        if (binding.progressBar.isIndeterminate()) {
            binding.progressBar.setIndeterminate(false);
            binding.progressBar.setMax(100);
        }

        int progress = (int) (((double) transferredSize / (double) (totalSize)) * 100);

        binding.progressBar.setProgress(progress);

        String txt = Utils.readableFileSize(transferredSize) + " / " + Utils.readableFileSize(totalSize);
        binding.progressText.setText(txt);

        SLogs.d("progress: " + progress + "%, " + txt);

    }

    private void onFileDownloaded(File destinationFile, boolean isUpdateWhenFileExists) {

        binding.opCancel.setEnabled(false);

        Intent result = new Intent();
        result.putExtra("target_file", direntModel.full_path);
        result.putExtra("destination_path", destinationFile.getAbsolutePath());
        result.putExtra("repo_id", repoId);
        result.putExtra("action", action);
        result.putExtra("dirent_uid", direntModel.uid);
        result.putExtra("is_update", isUpdateWhenFileExists);
        setResult(RESULT_OK, result);

        finish();
    }

    private void onFileDownloadFailed(SeafException seafException) {
        binding.opCancel.setEnabled(false);

        if (seafException == SeafException.NOT_FOUND_EXCEPTION) {
            // file deleted
            ToastUtils.showLong(String.format("The file \"%s\" has been deleted", direntModel.name));

            finishWithCancel();
        } else if (seafException == SeafException.INVALID_PASSWORD) {
            handlePassword();
        } else {
            ToastUtils.showLong(String.format("Failed to download file \"%s\"", seafException.getMessage()));

            finishWithCancel();
        }
    }

    private void finishWithCancel() {
        setResult(RESULT_CANCELED);

        finish();
    }

    private void handlePassword() {
        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance(repoModel.repo_id, repoModel.repo_name);
        dialogFragment.setResultListener(new OnResultListener<RepoModel>() {
            @Override
            public void onResultData(RepoModel newRepoModel) {
                loadData();
            }
        });

        dialogFragment.show(getSupportFragmentManager(), PasswordDialogFragment.class.getSimpleName());
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return false;
    }
}
