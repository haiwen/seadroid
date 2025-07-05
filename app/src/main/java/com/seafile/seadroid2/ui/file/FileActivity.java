package com.seafile.seadroid2.ui.file;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;

import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.FileActivityBinding;
import com.seafile.seadroid2.enums.FileReturnActionEnum;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetPasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;

import java.io.File;

import io.reactivex.functions.Consumer;

/**
 * Display a file
 */
public class FileActivity extends BaseActivityWithVM<FileViewModel> implements Toolbar.OnMenuItemClickListener {

    private FileActivityBinding binding;

    private File destinationFile;
    private String repoId;
    private String action;

    private DirentModel direntModel;
    private Account account;

    public static Intent start(Context context, DirentModel direntModel, FileReturnActionEnum actionEnum) {
        Intent starter = new Intent(context, FileActivity.class);
        starter.putExtra("dirent", direntModel);
        starter.putExtra("action", actionEnum.name());
        return starter;
    }

    public static Intent startFromStarred(Context context, StarredModel model, FileReturnActionEnum actionEnum) {
        DirentModel direntModel = DirentModel.convertStarredModelToThis(model);
        return start(context, direntModel, actionEnum);
    }

    public static Intent startFromActivity(Context context, ActivityModel model, FileReturnActionEnum actionEnum) {
        DirentModel direntModel = DirentModel.convertActivityModelToThis(model);
        return start(context, direntModel, actionEnum);
    }

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = FileActivityBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            finishWithCancel();
            return;
        }

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
        account = SupportAccountManager.getInstance().getCurrentAccount();
        //
        binding.progressBar.setIndeterminate(true);

        initWidgets(direntModel.name);

        initViewModel();

        if (NetworkUtils.isConnected()) {
            loadData();
        } else {
            Toasts.show(R.string.network_error);
            finishWithCancel();
        }
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
                getViewModel().saveToDb(account, direntModel, destinationFile, new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        onFileDownloaded(outFile, true);
                    }
                });
            }
        });
    }

    private void loadData() {
        getViewModel().loadFileDetail(repoId, direntModel.full_path, new Consumer<DirentFileModel>() {
            @Override
            public void accept(DirentFileModel direntFileModel) throws Exception {
                if (direntFileModel == null) {
                    Toasts.show(R.string.file_not_found);
                    finishWithCancel();
                    return;
                }

                direntModel.id = direntFileModel.id;
                direntModel.mtime = direntFileModel.mtime;
                direntModel.size = direntFileModel.size;

                getViewModel().download(account, direntModel, destinationFile);
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
        getViewModel().disposeAll();
        Toasts.show(R.string.download_cancelled);
        finishWithCancel();
    }

    private ExistingFileStrategy checkFileStrategy(DirentFileModel direntFileModel) {
        if (null == direntFileModel || TextUtils.isEmpty(direntFileModel.id)) {
            //has been deleted in remote repo
            return ExistingFileStrategy.NOT_FOUND_IN_REMOTE;
        }

        if (FileUtils.isFileExists(destinationFile)) {
            return ExistingFileStrategy.ASK;
        }

        return ExistingFileStrategy.REPLACE;
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        return DataManager.getLocalFileCachePath(account, repoId, repoName, fullPathInRepo);
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
            Toasts.show(String.format("The file \"%s\" has been deleted", direntModel.name));

            finishWithCancel();
        } else if (seafException == SeafException.INVALID_PASSWORD) {
            handlePassword();
        } else {
            Toasts.show(String.format("Failed to download file \"%s\"", seafException.getMessage()));

            finishWithCancel();
        }
    }

    private void finishWithCancel() {
        setResult(RESULT_CANCELED);

        finish();
    }

    private void handlePassword() {
        BottomSheetPasswordDialogFragment dialogFragment = BottomSheetPasswordDialogFragment.newInstance(direntModel.repo_id, direntModel.repo_name);
        dialogFragment.setResultListener(new OnResultListener<RepoModel>() {
            @Override
            public void onResultData(RepoModel newRepoModel) {
                if (newRepoModel == null) {
                    return;
                }

                loadData();
            }
        });

        dialogFragment.show(getSupportFragmentManager(), BottomSheetPasswordDialogFragment.class.getSimpleName());
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
