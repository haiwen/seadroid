package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

import androidx.activity.OnBackPressedCallback;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.databinding.ActivitySelectorObjBinding;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.repo.RepoQuickAdapter;
import com.seafile.seadroid2.ui.selector.obj.ObjSelectorActivity;
import com.seafile.seadroid2.ui.selector.obj.ObjSelectorViewModel;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;

public class AccountSelectorActivity extends BaseActivityWithVM<ObjSelectorViewModel> {
    private ActivitySelectorObjBinding binding;
    private int maxCount = -1;
    private boolean isSingleSelect = false;
    private RepoQuickAdapter adapter;
    private Account mAccount;


    public static Intent getIntent(Context context, int maxCount) {
        Intent intent = new Intent(context, AccountSelectorActivity.class);
        intent.putExtra("max", maxCount);
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

        maxCount = intent.getIntExtra("max", -1);
        isSingleSelect = maxCount == 1;

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
            getSupportActionBar().setTitle(R.string.choose_an_account);
        }

        initView();
        initViewModel();
        initRv();

        loadData();
    }

    private void initView() {
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);
        
        if (!isSingleSelect){
            binding.ok.setVisibility(View.VISIBLE);
            binding.ok.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    onOkClick();
                }
            });
        }
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

        adapter.setSelectType(ObjSelectType.ACCOUNT);
        adapter.setSingleSelect(isSingleSelect);
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
        boolean isAcc = baseModel instanceof Account;
        if (!isAcc) {
            return;
        }

        mAccount = (Account) baseModel;

        if (isSingleSelect) {
            onOkClick();
        } else {
            adapter.selectItemByMode(position);
        }

    }


    private void onOkClick() {
        if (mAccount == null) {
            Toasts.show(R.string.choose_an_account);
            return;
        }

        Intent intent = new Intent();
        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            intent.putExtras(bundle);
        }

        intent.putExtra(ObjKey.ACCOUNT, mAccount);
        setResult(RESULT_OK, intent);
        finish();
    }

    private void loadData() {
        // update action bar
        ActionBar bar = getSupportActionBar();
        if (bar == null) {
            return;
        }
        bar.setTitle(R.string.choose_an_account);

        //
        getViewModel().loadAccount();
    }

    private void showEmptyTip() {
        showAdapterTip(R.string.no_account);
    }

    private void showAdapterTip(int textRes) {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(this);
        tipView.setText(textRes);
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    private void stepBack() {
        setResult(RESULT_CANCELED);
        finish();
    }


}
