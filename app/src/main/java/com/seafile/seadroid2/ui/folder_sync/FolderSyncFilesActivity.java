package com.seafile.seadroid2.ui.folder_sync;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.datastore.SyncExclusionManager;
import com.seafile.seadroid2.framework.datastore.SyncRule;
import com.seafile.seadroid2.framework.datastore.SyncRuleManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.file.FileService;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executors;

/**
 * Shows all remote files for a sync rule with checkboxes.
 * Checked = included in sync (will be downloaded).
 * Unchecked = excluded (will be deleted locally if present).
 * New server files are auto-checked.
 */
public class FolderSyncFilesActivity extends BaseActivity {
    public static final String EXTRA_RULE_ID = "rule_id";

    private RecyclerView recyclerView;
    private TextView tvLoading;
    private TextView tvEmpty;
    private SyncRule rule;
    private SyncFileAdapter adapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_folder_sync_files);

        Toolbar toolbar = getActionBarToolbar();
        if (toolbar != null) {
            toolbar.setNavigationOnClickListener(v -> finish());
        }
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(R.string.settings_folder_sync_files_title);
        }

        recyclerView = findViewById(R.id.rv_files);
        tvLoading = findViewById(R.id.tv_loading);
        tvEmpty = findViewById(R.id.tv_empty);

        String ruleId = getIntent().getStringExtra(EXTRA_RULE_ID);
        if (ruleId == null) {
            finish();
            return;
        }

        rule = SyncRuleManager.findById(ruleId);
        if (rule == null) {
            finish();
            return;
        }

        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        adapter = new SyncFileAdapter(rule);
        recyclerView.setAdapter(adapter);

        // Apply system bar insets so the list doesn't go behind the nav bar
        androidx.core.view.ViewCompat.setOnApplyWindowInsetsListener(recyclerView, (v, insets) -> {
            androidx.core.graphics.Insets nav = insets.getInsets(
                    androidx.core.view.WindowInsetsCompat.Type.systemBars());
            v.setPadding(v.getPaddingLeft(), v.getPaddingTop(),
                    v.getPaddingRight(), nav.bottom);
            return insets;
        });

        loadRemoteFiles();
    }

    private void loadRemoteFiles() {
        tvLoading.setVisibility(View.VISIBLE);
        recyclerView.setVisibility(View.GONE);
        tvEmpty.setVisibility(View.GONE);

        Executors.newSingleThreadExecutor().execute(() -> {
            try {
                retrofit2.Response<List<DirentRecursiveFileModel>> response =
                        HttpIO.getCurrentInstance()
                                .execute(FileService.class)
                                .getDirRecursiveFileCall(rule.repoId, rule.remotePath)
                                .execute();

                List<SyncFileItem> items = new ArrayList<>();
                if (response.isSuccessful() && response.body() != null) {
                    Set<String> exclusions = SyncExclusionManager.getExcludedFiles(rule.id);
                    for (DirentRecursiveFileModel rf : response.body()) {
                        String fullPath = rf.getParent_dir() + rf.name;
                        String relativePath = rule.getRelativePath(fullPath);
                        boolean included = !exclusions.contains(relativePath);
                        items.add(new SyncFileItem(relativePath, rf.name, rf.size, included));
                    }
                }

                // Sort by path
                items.sort((a, b) -> a.relativePath.compareToIgnoreCase(b.relativePath));

                runOnUiThread(() -> {
                    tvLoading.setVisibility(View.GONE);
                    if (items.isEmpty()) {
                        tvEmpty.setVisibility(View.VISIBLE);
                    } else {
                        recyclerView.setVisibility(View.VISIBLE);
                        adapter.setItems(items);
                    }
                });
            } catch (Exception e) {
                runOnUiThread(() -> {
                    tvLoading.setVisibility(View.GONE);
                    tvEmpty.setVisibility(View.VISIBLE);
                    tvEmpty.setText(e.getMessage());
                });
            }
        });
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    protected void onPause() {
        super.onPause();
        // Trigger a sync to apply exclusion changes (delete excluded files, etc.)
        BackgroundJobManagerImpl.getInstance().runFolderSyncNow();
    }
}
