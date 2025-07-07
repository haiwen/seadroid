package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.ActivitySelectorObjBinding;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetNewDirFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetPasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.ui.repo.RepoQuickAdapter;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;

import io.reactivex.functions.Consumer;

/**
 * can select account、repo、dir
 */
public class ObjSelectorActivity extends BaseActivityWithVM<ObjSelectorViewModel> {
    private ObjSelectType mCurrentStepType = ObjSelectType.ACCOUNT;
    private ObjSelectType initType = ObjSelectType.ACCOUNT;
    private ObjSelectType selectType = ObjSelectType.ACCOUNT;

    //    private boolean canChooseAccount;
    private boolean isOnlyChooseRepo;
    private boolean isOnlyChooseAccount;

    private ActivitySelectorObjBinding binding;
    private MenuItem createFolderMenuItem;

    private final NavContext mNavContext = new NavContext(false);

    private RepoQuickAdapter adapter;
    private Account mAccount;

    public static Intent getCurrentAccountIntent(Context context, ObjSelectType initType, ObjSelectType selectType) {
        Intent intent = getIntent(context, initType, selectType, null);
        intent.putExtra(ObjKey.ACCOUNT, SupportAccountManager.getInstance().getCurrentAccount());
        return intent;
    }

    public static Intent getCurrentAccountIntent(Context context, ObjSelectType initType, ObjSelectType selectType, Bundle extras) {
        Intent intent = getIntent(context, initType, selectType, extras);
        intent.putExtra(ObjKey.ACCOUNT, SupportAccountManager.getInstance().getCurrentAccount());
        return intent;
    }

    public static Intent getIntent(Context context, ObjSelectType initType, ObjSelectType selectType) {
        return getIntent(context, initType, selectType, null);
    }

    public static Intent getIntent(Context context, ObjSelectType initType, ObjSelectType selectType, Bundle extras) {
        Intent intent = new Intent(context, ObjSelectorActivity.class);
        intent.putExtra("init_type", initType.ordinal());
        intent.putExtra("select_type", selectType.ordinal());
        if (extras != null) {
            intent.putExtras(extras);
        }
        return intent;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySelectorObjBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());

        Intent intent = getIntent();
        if (intent == null) {
            throw new IllegalArgumentException("Intent is null");
        }

        initType = ObjSelectType.values()[intent.getIntExtra("init_type", 0)];
        selectType = ObjSelectType.values()[intent.getIntExtra("select_type", 0)];

        if (initType == null || selectType == null) {
            throw new IllegalArgumentException("initType or selectType is null");
        }

        mCurrentStepType = initType;
        isOnlyChooseRepo = selectType == ObjSelectType.REPO && initType == ObjSelectType.REPO;
        isOnlyChooseAccount = selectType == ObjSelectType.ACCOUNT && initType == ObjSelectType.ACCOUNT;

        if (ObjSelectType.ACCOUNT != initType) {
            Account account = intent.getParcelableExtra(ObjKey.ACCOUNT);
            if (account == null) {
                mAccount = SupportAccountManager.getInstance().getCurrentAccount();
            } else {
                mAccount = account;
            }
        }

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                stepBack();
            }
        });

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                stepBack();
            }
        });

        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(R.string.app_name);
        }

        initView();
        initViewModel();
        initRv();

        checkLoginState();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        if (isOnlyChooseAccount || isOnlyChooseRepo) {
            binding.ok.setVisibility(View.VISIBLE);
            return true;
        }

        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_share_to_seafile, menu);
        createFolderMenuItem = menu.findItem(R.id.create_new_folder);
        if (mCurrentStepType == ObjSelectType.ACCOUNT) {
            setOperateViewVisible(false);
        } else if (mCurrentStepType == ObjSelectType.REPO) {
            setOperateViewVisible(false);
        } else {
            setOperateViewVisible(true);
        }
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

    public void setOperateViewVisible(boolean visible) {
        if (isOnlyChooseAccount || isOnlyChooseRepo) {
            return;
        }

        if (createFolderMenuItem != null) {
            createFolderMenuItem.setVisible(visible);
        }

        if (visible) {
            binding.ok.setVisibility(View.VISIBLE);
        } else {
            binding.ok.setVisibility(View.GONE);
        }
    }

    private void initView() {
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);
        binding.ok.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                onOkClick();
            }
        });
    }

    private void checkLoginState() {
        boolean isLogin = SupportAccountManager.getInstance().isLogin();
        if (!isLogin) {
            binding.ok.setEnabled(false);
        }

        loadData();
    }

    private void onOkClick() {
        if (selectType == ObjSelectType.REPO || selectType == ObjSelectType.DIR) {
            if (mAccount == null) {
                Toasts.show(R.string.choose_an_account);
                return;
            }
            if (mNavContext.getRepoModel() == null) {
                Toasts.show(R.string.choose_a_library);
                return;
            }
        }

        if (selectType == ObjSelectType.ACCOUNT && mAccount == null) {
            Toasts.show(R.string.choose_an_account);
            return;
        }


        Intent intent = new Intent();

        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            intent.putExtras(bundle);
        }

        if (selectType == ObjSelectType.REPO) {

            String repoName = mNavContext.getRepoModel().repo_name;
            String repoID = mNavContext.getRepoModel().repo_id;

            intent.putExtra(ObjKey.ACCOUNT, mAccount);
            intent.putExtra(ObjKey.REPO_NAME, repoName);
            intent.putExtra(ObjKey.REPO_ID, repoID);
        } else if (selectType == ObjSelectType.DIR) {

            String repoName = mNavContext.getRepoModel().repo_name;
            String repoID = mNavContext.getRepoModel().repo_id;
            String dir = mNavContext.getNavPath();

            intent.putExtra(ObjKey.ACCOUNT, mAccount);
            intent.putExtra(ObjKey.REPO_NAME, repoName);
            intent.putExtra(ObjKey.REPO_ID, repoID);
            intent.putExtra(ObjKey.DIR, dir);

        } else if (selectType == ObjSelectType.ACCOUNT) {
            intent.putExtra(ObjKey.ACCOUNT, mAccount);
        }

        setResult(RESULT_OK, intent);
        finish();
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getObjsListLiveData().observe(this, new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> baseModels) {
                notifyDataChanged(baseModels);
            }
        });
    }

    private void initRv() {
        adapter = new RepoQuickAdapter();
        adapter.setSelectType(selectType);
        adapter.setFileViewType(FileViewType.LIST);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            BaseModel baseModel = adapter.getItems().get(i);
            onItemClick(baseModel, i);
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());
        binding.rv.setPadding(0, Constants.DP.DP_8, 0, Constants.DP.DP_32);
        binding.rv.setClipToPadding(false);
    }

    private void notifyDataChanged(List<BaseModel> models) {
        if (CollectionUtils.isEmpty(models)) {
            showEmptyTip();
        } else {
            adapter.notifyDataChanged(models);
        }
    }

    private void onItemClick(BaseModel baseModel, int position) {
        if (baseModel instanceof Account account) {
            mAccount = account;

            if (isOnlyChooseAccount) {
                adapter.selectItemByMode(position);
                return;
            }

            mCurrentStepType = ObjSelectType.REPO;
            loadData();
        } else if (baseModel instanceof RepoModel repoModel) {

            if (isOnlyChooseRepo) {

                boolean isSelected = adapter.getItems().get(position).is_checked;
                if (isSelected) {
                    mNavContext.pop();
                } else {
                    mNavContext.push(repoModel);
                }
                adapter.selectItemByMode(position);
                return;
            }

            if (repoModel.encrypted) {
                doEncrypt(repoModel);
            } else {
                mCurrentStepType = ObjSelectType.DIR;
                mNavContext.push(repoModel);
                loadData();
            }

        } else if (baseModel instanceof DirentModel) {
            DirentModel model = (DirentModel) baseModel;
            if (!model.isDir()) {
                return;
            }

            mNavContext.push(model);
            loadData();
        }
    }

    private void doEncrypt(RepoModel repoModel) {
        getViewModel().getEncCacheDB(repoModel.repo_id, new Consumer<EncKeyCacheEntity>() {
            @Override
            public void accept(EncKeyCacheEntity encKeyCacheEntity) throws Exception {
                long now = TimeUtils.getNowMills();
                if (encKeyCacheEntity == null || encKeyCacheEntity.expire_time_long == 0) {
                    showPasswordDialog(repoModel);
                } else if (now < encKeyCacheEntity.expire_time_long) {
                    mCurrentStepType = ObjSelectType.DIR;
                    mNavContext.push(repoModel);
                    loadData();
                } else {
                    showPasswordDialog(repoModel);
                }
            }
        });
    }

    private void showPasswordDialog(RepoModel repoModel) {
        BottomSheetPasswordDialogFragment passwordDialogFragment = BottomSheetPasswordDialogFragment.newInstance(mAccount, repoModel.repo_id, repoModel.repo_name);
        passwordDialogFragment.setResultListener(new OnResultListener<RepoModel>() {
            @Override
            public void onResultData(RepoModel repoModel) {
                if (repoModel != null) {
                    mCurrentStepType = ObjSelectType.DIR;
                    mNavContext.push(repoModel);
                    loadData();
                }
            }
        });
        passwordDialogFragment.show(getSupportFragmentManager(), BottomSheetPasswordDialogFragment.class.getSimpleName());
    }

    private void checkCurrentPathHasWritePermission(java.util.function.Consumer<Boolean> consumer) {
        BaseModel model = mNavContext.getTopModel();

        String repo_id = null;
        int pNum = 0;
        if (model instanceof RepoModel m) {
            if (!m.isCustomPermission()) {
                consumer.accept(m.hasWritePermission());
                return;
            } else {
                repo_id = m.repo_id;
                pNum = m.getCustomPermissionNum();
            }
        } else if (model instanceof DirentModel m) {
            if (!m.isCustomPermission()) {
                consumer.accept(m.hasWritePermission());
                return;
            } else {
                repo_id = m.repo_id;
                pNum = m.getCustomPermissionNum();
            }
        } else {
            consumer.accept(false);
            return;
        }

        getViewModel().getPermissionFromLocal(repo_id, pNum, new Consumer<PermissionEntity>() {
            @Override
            public void accept(PermissionEntity entity) throws Exception {
                if (!entity.isValid()) {
                    consumer.accept(false);
                    return;
                }

                consumer.accept(entity.create);
            }
        });
    }

    private void showNewDirDialog() {
        if (mAccount == null) {
            Toasts.show(R.string.choose_an_account);
            return;
        }

        if (!mNavContext.inRepo()) {
            Toasts.show(R.string.choose_a_library);
            return;
        }

        checkCurrentPathHasWritePermission(aBoolean -> {

            String rid = mNavContext.getRepoModel().repo_id;
            String parentPath = mNavContext.getNavPath();
            BottomSheetNewDirFileDialogFragment dialogFragment = BottomSheetNewDirFileDialogFragment.newInstance(mAccount, rid, parentPath, true);
            dialogFragment.setRefreshListener(new OnRefreshDataListener() {
                @Override
                public void onActionStatus(boolean isDone) {
                    if (isDone) {
                        loadData();
                    }
                }
            });
            dialogFragment.show(getSupportFragmentManager(), BottomSheetNewDirFileDialogFragment.class.getSimpleName());
        });
    }

    private void loadData() {
        // update action bar
        ActionBar bar = getSupportActionBar();
        if (bar == null) {
            return;
        }

        if (mCurrentStepType == ObjSelectType.ACCOUNT) {

            bar.setTitle(R.string.choose_an_account);

            setOperateViewVisible(false);

            getViewModel().loadAccount();
        } else if (mCurrentStepType == ObjSelectType.REPO) {

            bar.setTitle(R.string.choose_a_library);

            setOperateViewVisible(false);

            boolean isFilterUnavailable = true;
            boolean isAddStarredGroup = false;
            if (getIntent().hasExtra("isFilterUnavailable")) {
                isFilterUnavailable = getIntent().getBooleanExtra("isFilterUnavailable", true);
            }

            if (getIntent().hasExtra("isAddStarredGroup")) {
                isAddStarredGroup = getIntent().getBooleanExtra("isAddStarredGroup", false);
            }

            getViewModel().loadReposFromNet(mAccount, isFilterUnavailable, isAddStarredGroup);
        } else if (mCurrentStepType == ObjSelectType.DIR) {

            setOperateViewVisible(true);

            bar.setTitle(R.string.choose_a_folder);
            getViewModel().loadDirentsFromNet(mAccount, mNavContext);
        }
    }

    private void showEmptyTip() {
        if (mCurrentStepType == ObjSelectType.ACCOUNT) {
            showAdapterTip(R.string.no_account);
        } else if (mCurrentStepType == ObjSelectType.REPO) {
            showAdapterTip(R.string.no_repo);
        } else if (mCurrentStepType == ObjSelectType.DIR) {
            showAdapterTip(R.string.dir_empty);
        }
    }

    private void showAdapterTip(int textRes) {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(this);
        tipView.setText(textRes);
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    private void stepBack() {
        switch (mCurrentStepType) {
            case ACCOUNT: {
                setResult(RESULT_CANCELED);
                finish();
            }
            break;
            case REPO: {
                if (initType == ObjSelectType.ACCOUNT) {
                    mAccount = null;
                    mCurrentStepType = ObjSelectType.ACCOUNT;
                    loadData();
                } else {
                    setResult(RESULT_CANCELED);
                    finish();
                }
            }
            break;
            case DIR: {
                if (mNavContext.inRepoRoot()) {
                    mCurrentStepType = ObjSelectType.REPO;
                }
                mNavContext.pop();
                loadData();
            }
            break;
        }
    }


}
