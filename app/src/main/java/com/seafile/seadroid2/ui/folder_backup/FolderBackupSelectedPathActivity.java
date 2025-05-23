package com.seafile.seadroid2.ui.folder_backup;

import static com.seafile.seadroid2.ui.folder_backup.FolderBackupConfigActivity.BACKUP_SELECT_PATHS;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.ui.settings.TabSettings2Fragment;

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

        initOnBackPressedDispatcher();

        findViewById(R.id.add_backup_folder).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(FolderBackupSelectedPathActivity.this, FolderBackupConfigActivity.class);
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

    private List<String> initBackupSelectPaths;

    private void initData() {
        initBackupSelectPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        mAdapter.submitList(initBackupSelectPaths);
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
        FolderBackupSharePreferenceHelper.clearLastScanTimeForPath(path);

        mAdapter.removeAt(position);
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

    private boolean isSettingsChanged() {
        if (mAdapter.getItems().isEmpty()) {
            return true;
        }

        List<String> selectedFolderPaths = mAdapter.getItems();
        return !selectedFolderPaths.equals(initBackupSelectPaths);
    }

    public void setFinishPage() {
        Intent intent = new Intent();
        intent.putExtra(TabSettings2Fragment.FB_SELECT_TYPE, "folder");

        if (isSettingsChanged()){
            List<String> selectedFolderPaths = mAdapter.getItems();
            intent.putStringArrayListExtra(BACKUP_SELECT_PATHS, (ArrayList<String>) selectedFolderPaths);
            setResult(RESULT_OK, intent);
        }else {
            setResult(RESULT_CANCELED, intent);
        }
        finish();
    }

    private final ActivityResultLauncher<Intent> folderBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                return;
            }

            Intent data = o.getData();
            if (null == data) {
                return;
            }

            ArrayList<String> selectedFolderPaths = data.getStringArrayListExtra(BACKUP_SELECT_PATHS);
            mAdapter.submitList(selectedFolderPaths);
        }
    });
}
