package com.seafile.seadroid2.ui.selector;

import android.os.Bundle;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.github.panpf.recycler.sticky.StickyItemDecoration;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.FragmentRemoteLibraryFragmentBinding;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.RepoSelectType;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.ui.repo.RepoQuickAdapter;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;

/**
 * Choose account and library for camera upload
 */
public class ObjSelectorFragment extends BaseFragment {
    private static final int STEP_CHOOSE_ACCOUNT = 1;
    private static final int STEP_CHOOSE_REPO = 2;
    private static final int STEP_CHOOSE_DIR = 3;
    private int mStep = 1;

    private FragmentRemoteLibraryFragmentBinding binding;
    private RepoQuickAdapter adapter;
    private final NavContext mNavContext = new NavContext();
    private ObjSelectorViewModel viewModel;
    private Account mAccount;
    private boolean canChooseAccount;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        viewModel = new ViewModelProvider(this).get(ObjSelectorViewModel.class);
    }

    public static ObjSelectorFragment newInstance(Account account) {

        Bundle args = new Bundle();
        args.putParcelable(ObjSelectorActivity.DATA_ACCOUNT, account);

        ObjSelectorFragment fragment = new ObjSelectorFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        if (getArguments() == null) {
            canChooseAccount = true;
        } else {
            Account account = getArguments().getParcelable(ObjSelectorActivity.DATA_ACCOUNT);
            if (account == null) {
                canChooseAccount = true;
            } else {
                mAccount = account;
            }
        }


        binding = FragmentRemoteLibraryFragmentBinding.inflate(getLayoutInflater(), container, false);

        initView();
        initViewModel();
        initRv();

        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        if (canChooseAccount) {
            mStep = STEP_CHOOSE_ACCOUNT;
        } else {
            mStep = STEP_CHOOSE_REPO;
        }

        loadData();
    }

    private void initView() {
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);

        binding.cucMultiSelectionUpLayout.setOnClickListener(v -> {
            try {
                stepBack();
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    private void initViewModel() {
        viewModel.getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        viewModel.getObjsListLiveData().observe(getViewLifecycleOwner(), new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> baseModels) {
                notifyDataChanged(baseModels);
            }
        });
    }

    private void initRv() {
        StickyItemDecoration decoration = new StickyItemDecoration.Builder()
                .itemType(AbsLayoutItemType.GROUP_ITEM)
                .build();

        binding.rv.addItemDecoration(decoration);

        adapter = new RepoQuickAdapter();
        adapter.setSelectType(RepoSelectType.ONLY_REPO);
        adapter.setFileViewType(FileViewType.LIST);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            BaseModel baseModel = adapter.getItems().get(i);
            onItemClick(baseModel, i);
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

    private void onItemClick(BaseModel baseModel, int position) {
        if (baseModel instanceof Account) {

            mAccount = (Account) baseModel;
            mStep = STEP_CHOOSE_REPO;

            binding.title.setText(mAccount.getDisplayName());

            loadData();

        } else if (baseModel instanceof RepoModel) {

            //It cannot be backed up to an encrypted repo, so there is no need to verify the password
            boolean status = adapter.selectItemByMode(position);
            if (status) {//selected
                RepoModel model = (RepoModel) baseModel;
                mNavContext.push(model);
            } else {
                mNavContext.pop();
            }



        }
    }

    private void loadData() {
        if (mStep == STEP_CHOOSE_ACCOUNT) {
            viewModel.loadAccount();
        } else if (mStep == STEP_CHOOSE_REPO) {
            viewModel.loadReposFromNet(mAccount,true);
        } else if (mStep == STEP_CHOOSE_DIR) {
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
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(textRes);
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    private void stepBack() {
        switch (mStep) {
            case STEP_CHOOSE_ACCOUNT: {
            }
            break;
            case STEP_CHOOSE_REPO: {
                if (canChooseAccount) {
                    mStep = STEP_CHOOSE_ACCOUNT;

                    binding.title.setText(R.string.choose_a_library);

                    loadData();
                }
            }
            break;
        }
    }


    public Pair<Account, RepoModel> getBackupInfo() {
        return new Pair<>(mAccount, mNavContext.getRepoModel());
    }
}

