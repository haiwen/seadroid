package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
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
import androidx.lifecycle.Observer;

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
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.versatile.RecentlyUsedModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetNewDirFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.selector.versatile.RecentlyUsedFragment;
import com.seafile.seadroid2.ui.selector.versatile.VersatileRepoSelectorFragment;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;

import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import io.reactivex.functions.Consumer;
import kotlin.Triple;

public class OpSelectorActivity extends BaseActivityWithVM<OpSelectorViewModel> {

    public enum OpSelectorType {
        COPY(0),
        MOVE(1),
        SHARE(2);

        OpSelectorType(int i) {

        }
    }

    private ActivityVersatileSelectorBinding binding;
    private final List<Fragment> fragments = new ArrayList<>();

    private String startRepoId;
    private String startPath;
    private String fileName;
    private OpSelectorType opSelectorTypeType;
    private Account mAccount;

    public static Intent getCurrentAccountIntent(Context context, String startRepoId, String startPath, String fileName, OpSelectorType opSelectorType) {
        Intent intent = new Intent(context, OpSelectorActivity.class);
        intent.putExtra("startRepoId", startRepoId);
        intent.putExtra("startPath", startPath);
        intent.putExtra("opType", opSelectorType.ordinal());
        intent.putExtra("fileName", fileName);
        return intent;
    }

    public static Intent getSpecialAccountIntent(Context context, String accountSignature, String startRepoId, String startPath, String fileName, OpSelectorType opSelectorType) {
        Intent intent = new Intent(context, OpSelectorActivity.class);
        intent.putExtra("accountSignature", accountSignature);
        intent.putExtra("startRepoId", startRepoId);
        intent.putExtra("startPath", startPath);
        intent.putExtra("opType", opSelectorType.ordinal());
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

        binding = ActivityVersatileSelectorBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());

        receiveParams();

        initView();
        initTabLayout();
        initViewPager();
        initViewModel();
    }

    private void initViewModel() {
        getViewModel().getSecondRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showLoadingDialog();
                } else {
                    dismissLoadingDialog();
                }
            }
        });
    }

    private void receiveParams() {
        Intent intent = getIntent();
        if (intent == null) {
            throw new IllegalArgumentException("Intent is null");
        }

        if (!intent.hasExtra("startRepoId")
                || !intent.hasExtra("startPath")
                || !intent.hasExtra("opType")) {
            throw new IllegalArgumentException("Intent missing params");
        }

        startRepoId = intent.getStringExtra("startRepoId");
        startPath = intent.getStringExtra("startPath");
        fileName = intent.getStringExtra("fileName");
        int o = intent.getIntExtra("opType", 0);
        opSelectorTypeType = OpSelectorType.values()[o];

        if (intent.hasExtra("accountSignature")) {
            String accountSignature = intent.getStringExtra("accountSignature");
            mAccount = SupportAccountManager.getInstance().getSpecialAccount(accountSignature);
        } else {
            mAccount = SupportAccountManager.getInstance().getCurrentAccount();
        }

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

        String repoId = null, repoName = null, dir = null;
        if (index == 0 || (index == 1 && (opSelectorTypeType == OpSelectorType.COPY || opSelectorTypeType == OpSelectorType.MOVE))) {
            Triple<String, String, String> triple = getVersatileRepoSelectorBackupInfo(index);
            if (null == triple) {
                Toasts.show(R.string.choose_a_library);
                return;
            }

            repoId = triple.component1();
            repoName = triple.component2();
            dir = triple.component3();
        } else {
            // can not create dir in Starred page
            return;
        }

        if (StringUtils.isEmpty(repoId)) {
            return;
        }

        BottomSheetNewDirFileDialogFragment dialogFragment = BottomSheetNewDirFileDialogFragment.newInstance(mAccount, repoId, dir, true);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    VersatileRepoSelectorFragment vrsf = (VersatileRepoSelectorFragment) fragments.get(index);
                    if (vrsf != null) {
                        vrsf.initLoad();
                    }
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
            String action;
            if (opSelectorTypeType == OpSelectorType.COPY) {
                action = getResources().getString(R.string.file_action_copy);
            } else if (opSelectorTypeType == OpSelectorType.MOVE) {
                action = getResources().getString(R.string.file_action_move);
            } else {
                action = getResources().getString(R.string.file_share);
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
        fragments.add(VersatileRepoSelectorFragment.newInstance(mAccount.getSignature(), startRepoId, startPath));

        if (opSelectorTypeType == OpSelectorType.COPY || opSelectorTypeType == OpSelectorType.MOVE) {
            fragments.add(VersatileRepoSelectorFragment.newInstance(mAccount.getSignature()));
        } else {
            fragments.add(StarredQuickFragment.newInstance(mAccount.getSignature(), true));
        }

        fragments.add(RecentlyUsedFragment.newInstance(mAccount));

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragments);
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setOffscreenPageLimit(1);
        binding.pager.setUserInputEnabled(false);

        String[] tabs;
        if (opSelectorTypeType == OpSelectorType.COPY || opSelectorTypeType == OpSelectorType.MOVE) { //
            tabs = getResources().getStringArray(R.array.versatile_selector_fragment_titles);
        } else {
            tabs = getResources().getStringArray(R.array.versatile_share_to_seafile_fragment_titles);
        }


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
        int index = binding.pager.getCurrentItem();
        if (index == 1 && opSelectorTypeType == OpSelectorType.SHARE) {
            Pair<String, String> pair = getViewModel().getStarredNavContextRepoIdAndName();
            if (pair == null) {
                Toasts.show(R.string.choose_a_library);
                return;
            }

            checkRepoPermission(pair.first, new java.util.function.Consumer<Boolean>() {
                @Override
                public void accept(Boolean canWrite) {
                    if (canWrite) {
                        Intent intent = setStarredIntent();
                        if (null == intent) {
                            return;
                        }

                        setResult(RESULT_OK, intent);
                        finish();
                    } else {
                        Toasts.show(R.string.library_read_only);
                    }
                }
            });
        } else {
            Intent intent = null;
            if (index == 0) {
                intent = setIntent(0);
            } else if (index == 1) {
                if (opSelectorTypeType == OpSelectorType.COPY) {
                    intent = setIntent(1);
                } else if (opSelectorTypeType == OpSelectorType.MOVE) {
                    intent = setIntent(1);
                }
            } else if (index == 2) {
                intent = setRecentlyIntent(2);
            }

            if (null == intent) {
                return;
            }

            setResult(RESULT_OK, intent);
            finish();
        }


    }

    private Triple<String, String, String> getVersatileRepoSelectorBackupInfo(int selectedIndex) {
        VersatileRepoSelectorFragment vrsf = (VersatileRepoSelectorFragment) fragments.get(selectedIndex);
        Pair<Account, NavContext> pair = vrsf.getBackupInfo();
        NavContext navContext = pair.second;
        if (!navContext.inRepo()) {
            return null;
        }

        RepoModel model = navContext.getRepoModel();
        if (model == null) {
            return null;
        }

        return new Triple<String, String, String>(model.repo_id, model.repo_name, pair.second.getNavPath());
    }

    private Intent setIntent(int selectedIndex) {
        Intent intent = new Intent();

        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            intent.putExtras(bundle);
        }

        Triple<String, String, String> triple = getVersatileRepoSelectorBackupInfo(selectedIndex);
        if (null == triple) {
            Toasts.show(R.string.choose_a_library);
            return null;
        }

        String repoId = triple.component1();
        String repoName = triple.component2();
        String dir = triple.component3();

        intent.putExtra(ObjKey.ACCOUNT, mAccount);
        intent.putExtra(ObjKey.REPO_NAME, repoName);
        intent.putExtra(ObjKey.REPO_ID, repoId);
        intent.putExtra(ObjKey.DIR, dir);

        // save recently used
        saveRecentlyUsedModelIfNotExists(repoId, repoName, dir);

        return intent;
    }

    private Intent setStarredIntent() {

        Intent intent = new Intent();

        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            intent.putExtras(bundle);
        }

        Pair<String, String> pair = getViewModel().getStarredNavContextRepoIdAndName();
        if (pair == null) {
            Toasts.show(R.string.choose_a_library);
            return null;
        }
        String repoID = pair.first;
        String repoName = pair.second;
        String dir = getViewModel().getStarredPath();
        if (TextUtils.isEmpty(dir)) {
            Toasts.show(R.string.choose_a_folder);
            return null;
        }

        intent.putExtra(ObjKey.ACCOUNT, mAccount);
        intent.putExtra(ObjKey.REPO_NAME, repoName);
        intent.putExtra(ObjKey.REPO_ID, repoID);
        intent.putExtra(ObjKey.DIR, dir);

        // save recently used
        saveRecentlyUsedModelIfNotExists(repoID, repoName, dir);

        return intent;
    }

    private void checkRepoPermission(String repoId, java.util.function.Consumer<Boolean> consumer) {
        // check permission
        getViewModel().getRepoModelAndPermissionEntity(mAccount, repoId, new Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(android.util.Pair<RepoModel, PermissionEntity> pair) throws Exception {
                RepoModel repoModel = pair.first;
                PermissionEntity permissionEntity = pair.second;
                if (repoModel == null) {
                    if (consumer != null) {
                        consumer.accept(false);
                    }
                } else if (repoModel.isCustomPermission()) {
                    if (permissionEntity == null) {
                        if (consumer != null) {
                            consumer.accept(false);
                        }
                    } else {
                        if (consumer != null) {
                            consumer.accept(permissionEntity.canWrite());
                        }
                    }
                } else {
                    if (consumer != null) {
                        consumer.accept(repoModel.hasWritePermission());
                    }
                }
            }
        });
    }

    private Intent setRecentlyIntent(int selectedIndex) {
        Intent intent = new Intent();

        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            intent.putExtras(bundle);
        }

        RecentlyUsedFragment ruf = (RecentlyUsedFragment) fragments.get(selectedIndex);
        RecentlyUsedModel model = ruf.getBackupInfo();
        if (model == null) {
            return null;
        }

        String repoName = model.repoName;
        String repoID = model.repoId;
        String dir = model.path;

        intent.putExtra(ObjKey.ACCOUNT, mAccount);
        intent.putExtra(ObjKey.REPO_NAME, repoName);
        intent.putExtra(ObjKey.REPO_ID, repoID);
        intent.putExtra(ObjKey.DIR, dir);

        // save recently used
        saveRecentlyUsedModelIfNotExists(repoID, repoName, dir);

        return intent;
    }

    private void saveRecentlyUsedModelIfNotExists(String repoId, String repoName, String path) {
        int index = binding.pager.getCurrentItem();
        if (index == 2) {
            return;
        }

        RecentlyUsedModel r = new RecentlyUsedModel();
        r.repoId = repoId;
        r.repoName = repoName;
        r.path = path;
        r.account = mAccount.getSignature();

        List<RecentlyUsedModel> list = getRecentUsedList(mAccount);
        if (CollectionUtils.isEmpty(list)) {
            list.add(r);
        } else {
            long c = list.stream().filter(f -> f.equals(r)).count();
            if (c == 0) {
                list.add(r);
            } else {
                // if exists in the list, return
                return;
            }
        }

        String s = GsonUtils.toJson(list);
        SharedPreferences sp = Settings.getSpecialUserSharedPreferences(mAccount);
        sp.edit().putString(SettingsManager.SELECTOR_RECENTLY_USED, s).apply();
    }

    public static List<RecentlyUsedModel> getRecentUsedList(Account account) {
        SharedPreferences sp = Settings.getSpecialUserSharedPreferences(account);
        String d = sp.getString(SettingsManager.SELECTOR_RECENTLY_USED, "");
        if (StringUtils.isEmpty(d)) {
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
