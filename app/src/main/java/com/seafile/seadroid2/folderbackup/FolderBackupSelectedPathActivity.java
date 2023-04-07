package com.seafile.seadroid2.folderbackup;

import static com.seafile.seadroid2.folderbackup.FolderBackupConfigActivity.BACKUP_SELECT_PATHS;
import static com.seafile.seadroid2.folderbackup.FolderBackupConfigActivity.BACKUP_SELECT_PATHS_SWITCH;
import static com.seafile.seadroid2.ui.fragment.SettingsFragment.FOLDER_BACKUP_REMOTE_PATH;

import android.content.Intent;
import android.os.Bundle;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.support.v7.widget.Toolbar;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.folderbackup.selectfolder.StringTools;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.bottomsheet.BottomSheetTextFragment;
import com.seafile.seadroid2.ui.widget.SupportRecyclerView;

import java.util.ArrayList;
import java.util.List;

public class FolderBackupSelectedPathActivity extends BaseActivity {
    private SupportRecyclerView mRecyclerView;
    private FolderBackSelectedPathRecyclerViewAdapter mAdapter;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        setContentView(R.layout.folder_backup_selected_path_activity);

        mRecyclerView = findViewById(R.id.lv_search);
        mAdapter = new FolderBackSelectedPathRecyclerViewAdapter(this);
        mAdapter.setOnItemClickListener(new OnItemClickListener<String>() {
            @Override
            public void onItemClick(String text, int position) {
                showBottomDialog(text);
            }
        });

        findViewById(R.id.add_backup_folder).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(FolderBackupSelectedPathActivity.this, FolderBackupConfigActivity.class);
                intent.putExtra(FOLDER_BACKUP_REMOTE_PATH, true);
                startActivity(intent);
            }
        });

        mRecyclerView.setAdapter(mAdapter);
        mRecyclerView.setLayoutManager(new LinearLayoutManager(this, RecyclerView.VERTICAL, false));
        View t = findViewById(R.id.ll_message_content);
        mRecyclerView.setEmptyView(t);

        mRecyclerView.setLoadingMoreEnabled(false);
        mRecyclerView.setPullRefreshEnabled(false);

        setSupportActionBar(getActionBarToolbar());
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.settings_folder_backup_select_title);

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
            mAdapter.notifyDataChanged(backupSelectPaths);
        }
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
        if (!CollectionUtils.isEmpty(mAdapter.getItemList())) {
            intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, (ArrayList<String>) mAdapter.getItemList());
            intent.putExtra(BACKUP_SELECT_PATHS_SWITCH, true);
        }
        setResult(RESULT_OK, intent);
    }
}
