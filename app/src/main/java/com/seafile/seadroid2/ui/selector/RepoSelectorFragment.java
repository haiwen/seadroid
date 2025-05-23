package com.seafile.seadroid2.ui.selector;

import static android.view.View.GONE;

import android.os.Bundle;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.FragmentRemoteLibraryFragmentBinding;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.repo.RepoQuickAdapter;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;

/**
 * Choose library for camera upload
 */
public class RepoSelectorFragment extends BaseFragmentWithVM<ObjSelectorViewModel> {
    private FragmentRemoteLibraryFragmentBinding binding;
    private RepoQuickAdapter adapter;

    // temp context
    private final NavContext mNavContext = new NavContext();
    private Account mAccount;

    public static RepoSelectorFragment newInstance() {
        return new RepoSelectorFragment();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mAccount = SupportAccountManager.getInstance().getCurrentAccount();

    }


    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentRemoteLibraryFragmentBinding.inflate(getLayoutInflater(), container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);


        initView();
        initViewModel();
        initRv();


        loadData();
    }

    private void initView() {
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);

        binding.cucMultiSelectionUpLayout.setVisibility(GONE);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getObjsListLiveData().observe(getViewLifecycleOwner(), new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> baseModels) {
                notifyDataChanged(baseModels);
            }
        });
    }

    private void initRv() {
        adapter = new RepoQuickAdapter();
        adapter.setSelectType(ObjSelectType.REPO);
        adapter.setFileViewType(FileViewType.LIST);

        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.no_repo);

        adapter.setStateView(tipView);
        adapter.setStateViewEnable(false);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            BaseModel baseModel = adapter.getItems().get(i);
            onItemClicked(baseModel, i);
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());
    }

    private void notifyDataChanged(List<BaseModel> models) {
        if (CollectionUtils.isEmpty(models)) {
            adapter.setStateViewEnable(true);
            adapter.submitList(null);
        } else {
            adapter.notifyDataChanged(models);
        }
    }

    private void onItemClicked(BaseModel model, int position) {
        //It cannot be backed up to an encrypted repo, so there is no need to verify the password
        adapter.selectItemByMode(position);

        RepoModel repoModel = (RepoModel) model;
        mNavContext.push(repoModel);
    }

    private void loadData() {
        getViewModel().loadReposFromNet(mAccount, true);
    }


    public Pair<Account, RepoModel> getBackupInfo() {
        return new Pair<>(mAccount, mNavContext.getRepoModel());
    }

    public boolean isChoseData() {
        return mAccount != null && mNavContext.getRepoModel() != null;
    }
}

