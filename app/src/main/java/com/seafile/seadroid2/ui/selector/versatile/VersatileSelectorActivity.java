package com.seafile.seadroid2.ui.selector.versatile;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.util.Pair;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.Fragment;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.ActivityVersatileSelectorBinding;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.versatile.RecentlyUsedModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

public class VersatileSelectorActivity extends BaseActivity {
    private ActivityVersatileSelectorBinding binding;
    private final List<Fragment> fragments = new ArrayList<>();

    private String startRepoId;
    private String startPath;
    private MenuItem createFolderMenuItem;
    private Account mAccount;

    public static Intent getCurrentAccountIntent(Context context) {
        return getIntent(context, null);
    }

    public static Intent getCurrentAccountIntent(Context context, String startRepoId, String startPath) {
        Intent intent = getIntent(context, null);
        intent.putExtra("startRepoId", startRepoId);
        intent.putExtra("startPath", startPath);
        return intent;
    }

    public static Intent getIntent(Context context) {
        return getIntent(context, null);
    }

    public static Intent getIntent(Context context, Bundle extras) {
        Intent intent = new Intent(context, VersatileSelectorActivity.class);
        if (extras != null) {
            intent.putExtras(extras);
        }
        return intent;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityVersatileSelectorBinding.inflate(getLayoutInflater());
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

        mAccount = SupportAccountManager.getInstance().getCurrentAccount();

    }

    private void finishSelf() {
        finish();
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
            getSupportActionBar().setTitle(R.string.app_name);
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
        fragments.add(VersatileRepoSelectorFragment.newInstance(startRepoId, startPath));
        fragments.add(VersatileRepoSelectorFragment.newInstance());
        fragments.add(RecentlyUsedFragment.newInstance());

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragments);
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setOffscreenPageLimit(1);
        binding.pager.setUserInputEnabled(false);

        String[] tabs = getResources().getStringArray(R.array.versatile_selector_fragment_titles);

        new TabLayoutMediator(binding.tabLayout, binding.pager, false, new TabLayoutMediator.TabConfigurationStrategy() {
            @Override
            public void onConfigureTab(@NonNull TabLayout.Tab tab, int position) {
                tab.setText(tabs[position]);
            }
        }).attach();
    }


    private boolean isExistsInUsedList(RecentlyUsedModel r) {
        List<RecentlyUsedModel> list = getRecentUsedList();
        if (CollectionUtils.isEmpty(list)) {
            return false;
        }

        boolean isExists = false;
        for (RecentlyUsedModel recentlyUsedModel : list) {
            if (recentlyUsedModel.equals(r)) {
                isExists = true;
                break;
            }
        }
        return isExists;
    }

    private void updateAvailableUsedModel() {
        int index = binding.pager.getCurrentItem();
        if (index == 2) {
            return;
        }

        VersatileRepoSelectorFragment vrsf = (VersatileRepoSelectorFragment) fragments.get(index);
        RecentlyUsedModel r = vrsf.genRecentUsedModel();
        if (r == null) {
            return;
        }

        boolean isExists = isExistsInUsedList(r);
        if (isExists) {
            return;
        }

        List<RecentlyUsedModel> list = getRecentUsedList();
        list.add(r);

        String s = GsonUtils.toJson(list);

        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return;
        }

        sp.edit().putString(SettingsManager.SELECTOR_RECENTLY_USED, s).apply();
    }

    private void onCancelClicked() {
        setResult(RESULT_CANCELED);
        finish();
    }

    private void onOkClicked() {
        updateAvailableUsedModel();

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
            RecentlyUsedFragment ruf = (RecentlyUsedFragment) fragments.get(2);
            RecentlyUsedModel model = ruf.getBackupInfo();

            String repoName = model.repoName;
            String repoID = model.repoId;
            String dir = model.path;

            intent.putExtra(ObjKey.ACCOUNT, mAccount);
            intent.putExtra(ObjKey.REPO_NAME, repoName);
            intent.putExtra(ObjKey.REPO_ID, repoID);
            intent.putExtra(ObjKey.DIR, dir);

        }

        setResult(RESULT_OK, intent);
        finish();
    }

    public static List<RecentlyUsedModel> getRecentUsedList() {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return new ArrayList<>();
        }

        String d = sp.getString(SettingsManager.SELECTOR_RECENTLY_USED, "");
        if (d.isEmpty()) {
            return new ArrayList<>();
        }

        Type listType = new TypeToken<List<RecentlyUsedModel>>() {
        }.getType();

        List<RecentlyUsedModel> list = GsonUtils.fromJson(d, listType);

        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        return list;

    }
}
