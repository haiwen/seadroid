package com.seafile.seadroid2.ui.selector;

import android.content.Intent;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.databinding.ActivitySelectorObjBinding;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.ui.repo.RepoQuickAdapter;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;

/**
 * can select repo、dir、account
 */
public class ObjSelectorActivity extends BaseActivity {
    private static final int STEP_CHOOSE_ACCOUNT = 1;
    private static final int STEP_CHOOSE_REPO = 2;
    private static final int STEP_CHOOSE_DIR = 3;
    private int mStep = 1;

    public static final String DATA_ACCOUNT = "account";
    public static final String DATA_REPO_ID = "repoID";
    public static final String DATA_REPO_NAME = "repoNAME";
    public static final String DATA_REPO_PERMISSION = "permission";
    public static final String DATA_DIRECTORY_PATH = "dirPath";
    public static final String DATA_DIR = "dir";


    private boolean canChooseAccount;
    private boolean isOnlyChooseRepo;

    private ActivitySelectorObjBinding binding;
    private NavContext mNavContext = new NavContext();

    private RepoQuickAdapter adapter;
    private ObjSelectorViewModel viewModel;
    private Account mAccount;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySelectorObjBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        //view model
        viewModel = new ViewModelProvider(this).get(ObjSelectorViewModel.class);

        Intent intent = getIntent();
        if (intent != null) {
            Account account = intent.getParcelableExtra(DATA_ACCOUNT);
            if (account == null) {
                canChooseAccount = true;
            } else {
                mAccount = account;
            }
        }


        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(R.string.app_name);
        }

        initView();
        initViewModel();
        initAdapter();

        if (canChooseAccount) {
            mStep = STEP_CHOOSE_ACCOUNT;
        } else {
            mStep = STEP_CHOOSE_REPO;
        }

        loadData();
    }

    private void initView() {
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);

        if (isOnlyChooseRepo) {
            binding.ok.setVisibility(View.GONE);
            binding.newFolder.setVisibility(View.GONE);
        }

        binding.ok.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String repoName = mNavContext.getRepoModel().repo_name;
                String repoID = mNavContext.getRepoModel().repo_id;
                String dir = mNavContext.getNavPath();

                Intent intent = new Intent();
                intent.putExtra(DATA_REPO_NAME, repoName);
                intent.putExtra(DATA_REPO_ID, repoID);
                intent.putExtra(DATA_DIR, dir);
                intent.putExtra(DATA_ACCOUNT, mAccount);
                setResult(RESULT_OK, intent);
                finish();
            }
        });

        binding.newFolder.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
//                createNewFolder();
            }
        });

        binding.cancel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                setResult(RESULT_CANCELED);
                finish();
            }
        });
    }


    private void initViewModel() {
        viewModel.getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        viewModel.getObjsListLiveData().observe(this, new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> baseModels) {
                notifyDataChanged(baseModels);
            }
        });
    }

    private void initAdapter() {
        adapter = new RepoQuickAdapter();
        adapter.setSelectorMode(2);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            BaseModel baseModel = adapter.getItems().get(i);
            onItemClick(baseModel);
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());
    }

    private void notifyDataChanged(List<BaseModel> models) {
        if (CollectionUtils.isEmpty(models)) {
            showEmptyTip();
        } else {
            adapter.notifyDataChanged(models);
        }
    }

    private void onItemClick(BaseModel baseModel) {
        if (baseModel instanceof Account) {
            mAccount = (Account) baseModel;
            mStep = STEP_CHOOSE_REPO;
        } else if (baseModel instanceof RepoModel) {
            RepoModel model = (RepoModel) baseModel;
            mStep = STEP_CHOOSE_DIR;
            mNavContext.push(model);

        } else if (baseModel instanceof DirentModel) {
            DirentModel model = (DirentModel) baseModel;
            if (!model.isDir()) {
                return;
            }

            mNavContext.push(model);

        }

        loadData();
    }

    private void loadData() {
        // update action bar
        ActionBar bar = getSupportActionBar();
        if (bar == null) {
            return;
        }

        if (mStep == STEP_CHOOSE_ACCOUNT) {

            bar.setDisplayHomeAsUpEnabled(false);
            bar.setTitle(R.string.choose_an_account);

            viewModel.loadAccount();
        } else if (mStep == STEP_CHOOSE_REPO) {

            //TODO 校验密码

            bar.setDisplayHomeAsUpEnabled(true);
            bar.setTitle(R.string.choose_a_library);

            viewModel.loadReposFromNet(mAccount);
        } else if (mStep == STEP_CHOOSE_DIR) {

            bar.setDisplayHomeAsUpEnabled(true);
            bar.setTitle(R.string.choose_a_folder);

            viewModel.loadDirentsFromNet(mAccount, mNavContext);
        }
    }

    private void showEmptyTip() {
        if (mStep == STEP_CHOOSE_ACCOUNT) {
            showAdapterTip(R.string.no_account);
        } else if (mStep == STEP_CHOOSE_REPO) {
            showAdapterTip(R.string.no_repo);
        } else if (mStep == STEP_CHOOSE_DIR) {
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

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            stepBack();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public void onBackPressed() {
        stepBack();
    }

    private void stepBack() {
        switch (mStep) {
            case STEP_CHOOSE_ACCOUNT: {
                setResult(RESULT_CANCELED);
                finish();
            }
            break;
            case STEP_CHOOSE_REPO: {
                if (canChooseAccount) {
                    mStep = STEP_CHOOSE_ACCOUNT;
                    loadData();
                } else {
                    setResult(RESULT_CANCELED);
                    finish();
                }
            }
            break;
            case STEP_CHOOSE_DIR: {
                if (mNavContext.isInRepoRoot()) {
                    mStep = STEP_CHOOSE_REPO;
                } else {
                    mNavContext.pop();
                }
                loadData();
            }
            break;
        }
    }


}
