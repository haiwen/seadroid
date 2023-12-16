package com.seafile.seadroid2.folderbackup;

import static com.seafile.seadroid2.folderbackup.FolderBackupConfigActivity.BACKUP_SELECT_PATHS;
import static com.seafile.seadroid2.folderbackup.FolderBackupConfigActivity.BACKUP_SELECT_PATHS_SWITCH;
import static com.seafile.seadroid2.ui.settings.SettingsFragment.FOLDER_BACKUP_REMOTE_PATH;

import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter.base.BaseQuickAdapter;
import com.chad.library.adapter.base.QuickAdapterHelper;
import com.cocosw.bottomsheet.BottomSheet;
import com.google.gson.Gson;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.folderbackup.selectfolder.StringTools;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.ui.bottomsheet.BottomSheetTextFragment;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class FolderBackupSelectedPathActivity extends BaseActivity {
    private RecyclerView mRecyclerView;
    private FolderBackSelectedPathRecyclerViewAdapter mAdapter;
    private QuickAdapterHelper helper;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        setContentView(R.layout.folder_backup_selected_path_activity);


        findViewById(R.id.add_backup_folder).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(FolderBackupSelectedPathActivity.this, FolderBackupConfigActivity.class);
                intent.putExtra(FOLDER_BACKUP_REMOTE_PATH, true);
                startActivity(intent);
            }
        });


        setSupportActionBar(getActionBarToolbar());
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.settings_folder_backup_select_title);

        mRecyclerView = findViewById(R.id.lv_search);
        mRecyclerView.setLayoutManager(new LinearLayoutManager(this, RecyclerView.VERTICAL, false));

        initAdapter();
    }

    @Override
    protected void onResume() {
        super.onResume();
        initData();
    }

    private void initData() {
        String backupPaths = SettingsManager.instance().getBackupPaths();
        if (!TextUtils.isEmpty(backupPaths)) {
            List<String> backupSelectPaths = StringTools.getJsonToList(backupPaths);
            mAdapter.submitList(backupSelectPaths);
        }
    }

    private void initAdapter() {
        mAdapter = new FolderBackSelectedPathRecyclerViewAdapter();
        View t = findViewById(R.id.ll_message_content);
        mAdapter.setEmptyView(t);
        mAdapter.setEmptyViewEnable(true);
        mAdapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<String>() {
            @Override
            public void onClick(@NotNull BaseQuickAdapter<String, ?> baseQuickAdapter, @NotNull View view, int i) {
                showBottomDialog(mAdapter.getItems().get(i));
            }
        });
        mAdapter.addOnItemChildClickListener(R.id.more, new BaseQuickAdapter.OnItemChildClickListener<String>() {
            @Override
            public void onItemClick(@NotNull BaseQuickAdapter<String, ?> baseQuickAdapter, @NotNull View view, int i) {
                showRepoBottomSheet(i);
            }
        });

        helper = new QuickAdapterHelper.Builder(mAdapter).build();
        mRecyclerView.setAdapter(helper.getAdapter());
    }

    private void showRepoBottomSheet(int position) {
        new BottomSheet.Builder(this).sheet(R.menu.folder_backup_bottom_sheet_delete).listener(new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (which == R.id.delete) {
                    mAdapter.removeAt(position);

                    String strJsonPath = new Gson().toJson(mAdapter.getItems());
                    SettingsManager.instance().saveBackupPaths(strJsonPath);
                }
            }
        }).show();
    }

    private void showBottomDialog(String text) {
        BottomSheetTextFragment.newInstance(text).show(getSupportFragmentManager(), BottomSheetTextFragment.class.getSimpleName());
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
            intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, (ArrayList<String>) mAdapter.getItems());
            intent.putExtra(BACKUP_SELECT_PATHS_SWITCH, true);
        }
        setResult(RESULT_OK, intent);
    }
}
