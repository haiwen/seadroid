package com.seafile.seadroid2.ui.folder_backup;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.gson.Gson;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;

import java.util.ArrayList;
import java.util.List;

public class FolderBackupSelectedPathActivity extends BaseActivity {
    private RecyclerView mRecyclerView;
    private FolderBackupSelectedPathAdapter mAdapter;
    private QuickAdapterHelper helper;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        setContentView(R.layout.folder_backup_selected_path_activity);


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

        initData();
    }

    private void initData() {
        List<String> backupSelectPaths = FolderBackupConfigSPs.getBackupPathList();
        mAdapter.submitList(backupSelectPaths);
    }

    private void initAdapter() {
        mAdapter = new FolderBackupSelectedPathAdapter();
        View t = findViewById(R.id.ll_message_content);
        mAdapter.setStateView(t);
        mAdapter.setStateViewEnable(true);
        mAdapter.setOnItemClickListener((baseQuickAdapter, view, i) -> showBottomDialog(mAdapter.getItems().get(i)));
        mAdapter.addOnItemChildClickListener(R.id.more, (baseQuickAdapter, view, i) -> showRepoBottomSheet(i));

        helper = new QuickAdapterHelper.Builder(mAdapter).build();
        mRecyclerView.setAdapter(helper.getAdapter());
    }

    private void showRepoBottomSheet(int position) {
        BottomSheetHelper.showSheet(this, R.menu.folder_backup_bottom_sheet_delete, menuItem -> {
            if (menuItem.getItemId() == R.id.delete) {
                mAdapter.removeAt(position);

                String strJsonPath = new Gson().toJson(mAdapter.getItems());
                FolderBackupConfigSPs.saveBackupPathsByCurrentAccount(strJsonPath);
            }
        });
    }

    private void showBottomDialog(String text) {
        new BottomSheetMenuFragment.Builder(this)
                .setTitle(text)
                .setCancelable(true)
                .show(getSupportFragmentManager());
    }

    @Override
    public void onBackPressed() {
        setFinishPage();

        super.onBackPressed();
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            setFinishPage();

            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    public void setFinishPage() {
        Intent intent = new Intent();
        if (!CollectionUtils.isEmpty(mAdapter.getItems())) {
            intent.putStringArrayListExtra(FolderBackupConfigActivity.BACKUP_SELECT_PATHS, (ArrayList<String>) mAdapter.getItems());
        }
        setResult(RESULT_OK, intent);
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
}
