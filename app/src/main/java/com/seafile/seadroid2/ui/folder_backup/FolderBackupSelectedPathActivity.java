package com.seafile.seadroid2.ui.folder_backup;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.work.NetworkType;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.framework.file_monitor.FileSyncService;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;

import java.util.ArrayList;
import java.util.List;

public class FolderBackupSelectedPathActivity extends BaseActivity {
    private RecyclerView mRecyclerView;
    private FolderBackupSelectedPathAdapter mAdapter;
    private QuickAdapterHelper helper;
    private String initialPathStr;
    private FileSyncService fileSyncService;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        setContentView(R.layout.folder_backup_selected_path_activity);

        initOnBackPressedDispatcher();

        findViewById(R.id.add_backup_folder).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(FolderBackupSelectedPathActivity.this, FolderBackupConfigActivity.class);
                intent.putExtra(FolderBackupConfigActivity.FOLDER_BACKUP_SELECT_MODE, "folder");
                folderBackupConfigLauncher.launch(intent);
            }
        });


        setSupportActionBar(getActionBarToolbar());
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.settings_folder_backup_select_title);

        mRecyclerView = findViewById(R.id.lv_search);
        mRecyclerView.setLayoutManager(new LinearLayoutManager(this, RecyclerView.VERTICAL, false));

        initAdapter();


        initialPathStr = FolderBackupManager.readBackupPathStr();

        initData();
    }

    private void initData() {
        List<String> backupSelectPaths = FolderBackupManager.readBackupPaths();
        mAdapter.submitList(backupSelectPaths);
    }

    private void initAdapter() {
        mAdapter = new FolderBackupSelectedPathAdapter();
        mAdapter.setStateViewEnable(false);
        mAdapter.setOnItemClickListener((baseQuickAdapter, view, i) -> showBottomDialog(mAdapter.getItems().get(i)));
        mAdapter.addOnItemChildClickListener(R.id.more, (baseQuickAdapter, view, i) -> showRepoBottomSheet(i));

        helper = new QuickAdapterHelper.Builder(mAdapter).build();
        mRecyclerView.setAdapter(helper.getAdapter());
    }

    private void showRepoBottomSheet(int position) {
        BottomSheetHelper.showSheet(this, R.menu.folder_backup_bottom_sheet_delete, menuItem -> {
            if (menuItem.getItemId() == R.id.delete) {
                deletePath(position);
            }
        });
    }

    private void deletePath(int position) {
        //clear last scan time
        String path = mAdapter.getItems().get(position);
        FolderBackupManager.clearBackupPathLastScanTime(path);

        mAdapter.removeAt(position);

        List<String> list = mAdapter.getItems();
        FolderBackupManager.writeBackupPaths(list);

        if (fileSyncService != null) {
            fileSyncService.stopFolderMonitor();

            fileSyncService.startFolderMonitor();
        }

        //restart
        BackgroundJobManagerImpl.getInstance().restartFolderUploadWorker();
    }

    private void showBottomDialog(String text) {
        new BottomSheetMenuFragment.Builder(this)
                .setTitle(text)
                .setCancelable(true)
                .show(getSupportFragmentManager());
    }

    private void initOnBackPressedDispatcher() {
        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                setFinishPage();
            }
        });
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            getOnBackPressedDispatcher().onBackPressed();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    public void setFinishPage() {
        String lastestPathStr = FolderBackupManager.readBackupPathStr();
        if (!TextUtils.equals(lastestPathStr, initialPathStr)) {
            setResult(RESULT_OK);
        } else {
            setResult(RESULT_CANCELED);
        }
        finish();
    }

    private final ActivityResultLauncher<Intent> folderBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                return;
            }

            initData();
        }
    });


    private boolean isBound = false;

    private final ServiceConnection syncConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            FileSyncService.FileSyncBinder binder = (FileSyncService.FileSyncBinder) service;
            fileSyncService = binder.getService();
            isBound = true;
            SLogs.d("SettingsFragment: bond FileSyncService");
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            fileSyncService = null;
            isBound = false;
            SLogs.d("SettingsFragment: FileSyncService disconnected");
        }
    };

    private void bindService() {
        if (!isBound) {
            Intent syncIntent = new Intent(this, FileSyncService.class);
            bindService(syncIntent, syncConnection, Context.BIND_AUTO_CREATE);
        }
    }

    private void unbindService() {
        if (isBound) {
            unbindService(syncConnection);
        }
    }

    @Override
    public void onDestroy() {

        unbindService();

        super.onDestroy();
    }
}
