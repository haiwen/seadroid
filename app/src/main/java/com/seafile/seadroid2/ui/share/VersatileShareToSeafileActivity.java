package com.seafile.seadroid2.ui.share;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.Fragment;

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.ActivityVersatileShareToSeafileBinding;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.model.versatile.RecentlyUsedModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetNewDirFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.selector.versatile.VersatileRepoSelectorFragment;
import com.seafile.seadroid2.ui.selector.versatile.VersatileSelectorActivity;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class VersatileShareToSeafileActivity extends BaseActivity {
    private ActivityVersatileShareToSeafileBinding binding;
    private final List<Fragment> fragments = new ArrayList<>();


    private String startRepoId;
    private String startPath;
    private String fileName;
    private int actionType;
    private String accountSignature;
    private Account mAccount;

    public static Intent getSpecialAccountIntent(Context context, String accountSignature, String startRepoId, String startPath, String fileName, int actionType) {
        Intent intent = new Intent(context, VersatileShareToSeafileActivity.class);
        intent.putExtra("accountSignature", accountSignature);
        intent.putExtra("startRepoId", startRepoId);
        intent.putExtra("startPath", startPath);
        intent.putExtra("actionType", actionType);
        intent.putExtra("fileName", fileName);
        return intent;
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {

        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_share_to_seafile, menu);

        return true;
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == R.id.create_new_folder) {
            showNewDirDialog();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityVersatileShareToSeafileBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());

        receiveParams();

        initView();
        initTabLayout();
        initViewPager();
    }

    private void receiveParams() {
        Intent intent = getIntent();
        if (intent == null) {
            throw new IllegalArgumentException("Intent is null");
        }

        startRepoId = intent.getStringExtra("startRepoId");
        startPath = intent.getStringExtra("startPath");
        fileName = intent.getStringExtra("fileName");
        actionType = intent.getIntExtra("actionType", 0);

        accountSignature = intent.getStringExtra("accountSignature");
        if (StringUtils.isEmpty(accountSignature)) {
            throw new IllegalArgumentException("accountSignature is null");
        }

        mAccount = SupportAccountManager.getInstance().getSpecialAccount(accountSignature);
        if (mAccount == null) {
            throw new IllegalArgumentException("account is null");
        }

    }

    private void finishSelf() {
        finish();
    }

    private void showNewDirDialog() {
        if (mAccount == null) {
            Toasts.show(R.string.choose_an_account);
            return;
        }

        int index = binding.pager.getCurrentItem();
        if (index == 2) {
            return;
        }

        VersatileRepoSelectorFragment vrsf = (VersatileRepoSelectorFragment) fragments.get(index);
        RecentlyUsedModel r = vrsf.genRecentUsedModel();
        if (r == null) {
            Toasts.show(R.string.choose_a_library);
            return;
        }

        String rid = r.repoId;
        String parentPath = r.path;
        BottomSheetNewDirFileDialogFragment dialogFragment = BottomSheetNewDirFileDialogFragment.newInstance(mAccount, rid, parentPath, true);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    vrsf.initLoad();
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), BottomSheetNewDirFileDialogFragment.class.getSimpleName());
    }

    private void initView() {
        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                finishSelf();
            }
        });

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                finishSelf();
            }
        });

        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);

            String action = null;
            if (actionType == 0) {
                action = getString(R.string.file_action_copy);
            } else if (actionType == 1) {
                action = getString(R.string.file_action_move);
            } else if (actionType == 2) {
                action = getString(R.string.file_share);
            }


            String title = TextUtils.isEmpty(fileName) ? action : action + " " + fileName;
            getSupportActionBar().setTitle(title);
        }

        binding.ok.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                onOkClicked();
            }
        });

        binding.textViewNegative.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                onCancelClicked();
            }
        });
    }

    private void initTabLayout() {
        binding.tabLayout.setTabIndicatorAnimationMode(TabLayout.INDICATOR_ANIMATION_MODE_ELASTIC);
        binding.tabLayout.setSelectedTabIndicator(R.drawable.cat_tabs_rounded_line_indicator);
        binding.tabLayout.setTabIndicatorFullWidth(false);
        binding.tabLayout.setTabGravity(TabLayout.GRAVITY_CENTER);

        binding.tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                onTabLayoutSelected();
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });
    }

    private void onTabLayoutSelected() {
        supportInvalidateOptionsMenu();
    }

    private void initViewPager() {
        fragments.clear();
        fragments.add(VersatileRepoSelectorFragment.newInstance(accountSignature, startRepoId, startPath));
        fragments.add(VersatileRepoSelectorFragment.newInstance(accountSignature, null, null));
        fragments.add(StarredQuickFragment.newInstance(accountSignature, true));

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragments);
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setOffscreenPageLimit(1);
        binding.pager.setUserInputEnabled(false);

        String[] tabs = getResources().getStringArray(R.array.versatile_share_to_seafile_fragment_titles);

        new TabLayoutMediator(binding.tabLayout, binding.pager, false, new TabLayoutMediator.TabConfigurationStrategy() {
            @Override
            public void onConfigureTab(@NonNull TabLayout.Tab tab, int position) {
                tab.setText(tabs[position]);
            }
        }).attach();
    }

    private void onCancelClicked() {
        setResult(RESULT_CANCELED);
        finish();
    }

    private void onOkClicked() {
        Intent intent = new Intent();

        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            intent.putExtras(bundle);
        }

        int index = binding.pager.getCurrentItem();
        if (index == 0 || index == 1) {
            VersatileRepoSelectorFragment vrsf = (VersatileRepoSelectorFragment) fragments.get(index);
            Pair<Account, NavContext> pair = vrsf.getBackupInfo();
            NavContext navContext = pair.second;
            if (!navContext.inRepo()) {
                return;
            }

            RepoModel model = navContext.getRepoModel();
            if (model == null) {
                return;
            }

            String repoName = model.repo_name;
            String repoID = model.repo_id;
            String dir = pair.second.getNavPath();

            intent.putExtra(ObjKey.ACCOUNT, mAccount);
            intent.putExtra(ObjKey.REPO_NAME, repoName);
            intent.putExtra(ObjKey.REPO_ID, repoID);
            intent.putExtra(ObjKey.DIR, dir);

        } else if (index == 2) {
            StarredQuickFragment starredQuickFragment = (StarredQuickFragment) fragments.get(2);
            StarredModel starredModel = starredQuickFragment.getSingleSelectedModel();
            if (starredModel == null) {
                return;
            }

            String repoName = starredModel.repo_name;
            String repoID = starredModel.repo_id;
            String dir = starredModel.path;

            intent.putExtra(ObjKey.ACCOUNT, mAccount);
            intent.putExtra(ObjKey.REPO_NAME, repoName);
            intent.putExtra(ObjKey.REPO_ID, repoID);
            intent.putExtra(ObjKey.DIR, dir);
        }

        setResult(RESULT_OK, intent);
        finish();
    }
}