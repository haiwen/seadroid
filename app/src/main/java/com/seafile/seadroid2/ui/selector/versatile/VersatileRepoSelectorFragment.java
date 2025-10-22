package com.seafile.seadroid2.ui.selector.versatile;

import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextClock;
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
import com.seafile.seadroid2.databinding.FragmentVersatileSelectorBinding;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.versatile.RecentlyUsedModel;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.repo.RepoQuickAdapter;
import com.seafile.seadroid2.ui.selector.obj.ObjSelectorViewModel;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

import io.reactivex.Completable;

public class VersatileRepoSelectorFragment extends BaseFragmentWithVM<ObjSelectorViewModel> {
    private FragmentVersatileSelectorBinding binding;
    private RepoQuickAdapter adapter;
    private String startRepoId;
    private String startPath;
    private RepoModel startRepoModel;

    // temp context
    private final NavContext localNavContext = new NavContext(false);
    private Account mAccount;

    public static VersatileRepoSelectorFragment newInstance() {
        return newInstance(null, null);
    }

    public static VersatileRepoSelectorFragment newInstance(String startRepoId, String startPath) {
        VersatileRepoSelectorFragment fragment = new VersatileRepoSelectorFragment();
        if (TextUtils.isEmpty(startRepoId)) {
            return fragment;
        }

        Bundle bundle = new Bundle();
        bundle.putString("startRepoId", startRepoId);
        bundle.putString("startPath", startPath);
        fragment.setArguments(bundle);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mAccount = SupportAccountManager.getInstance().getCurrentAccount();

        if (getArguments() != null) {
            startRepoId = getArguments().getString("startRepoId", "");
            startPath = getArguments().getString("startPath", "");
        }

    }


    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        binding = FragmentVersatileSelectorBinding.inflate(getLayoutInflater(), container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);


        initView();
        initViewModel();
        initRv();

        initLoad();
    }

    private void initLoad() {

        if (!TextUtils.isEmpty(startRepoId)) {
            CompletableFuture.supplyAsync(new Supplier<List<RepoModel>>() {
                        @Override
                        public List<RepoModel> get() {
                            return AppDatabase.getInstance().repoDao().getRepoByIdSync(startRepoId);
                        }
                    })
                    .whenComplete(new BiConsumer<List<RepoModel>, Throwable>() {
                        @Override
                        public void accept(List<RepoModel> repoModels, Throwable throwable) {

                            requireActivity().runOnUiThread(new Runnable() {
                                @Override
                                public void run() {

                                    if (!CollectionUtils.isEmpty(repoModels)) {
                                        startRepoModel = repoModels.get(0);
                                        localNavContext.switchToPath(startRepoModel, startPath);
                                    }

                                    setReturnToEnable();

                                    loadData();

                                }
                            });
                        }
                    });

        } else {
            setReturnToEnable();

            loadData();
        }
    }

    private void initView() {
        binding.returnTo.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                returnTo();
            }
        });
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);
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
        adapter.setSelectType(ObjSelectType.DIR);
        adapter.setFileViewType(FileViewType.LIST);

        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.empty_folder);

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
//        adapter.selectItemByMode(position);

        if (model instanceof RepoModel repoModel) {
            setReturnToEnable(true);
            localNavContext.push(repoModel);
            loadData();
        } else if (model instanceof DirentModel direntModel) {
            if (direntModel.isDir()) {
                setReturnToEnable(true);
                localNavContext.push(direntModel);
                loadData();
            } else {

            }
        }
    }

    private void returnTo() {
        localNavContext.pop();

        setReturnToEnable();

        loadData();
    }

    private void loadData() {
        if (localNavContext.inRepo()) {
            getViewModel().loadDirentsFromNet(mAccount, localNavContext);
        } else if (TextUtils.isEmpty(startRepoId)) {
            getViewModel().loadReposFromNet(mAccount, true, false);
        }
    }

    private void setReturnToEnable() {
        if (!TextUtils.isEmpty(startRepoId)) {
            if (localNavContext.inRepoRoot()) {
                setReturnToEnable(false);
            } else {
                setReturnToEnable(true);
            }
        } else if (!localNavContext.inRepo()) {
            setReturnToEnable(false);
        } else {
            setReturnToEnable(true);
        }
    }

    private void setReturnToEnable(boolean isEnable) {
        binding.returnTo.setEnabled(isEnable);
        if (isEnable) {
            binding.returnToIcon.setAlpha(1f);
            binding.returnToTitle.setAlpha(1f);
        } else {
            binding.returnToIcon.setAlpha(0.5f);
            binding.returnToTitle.setAlpha(0.5f);
        }
    }

    public Pair<Account, NavContext> getBackupInfo() {
        return new Pair<>(mAccount, localNavContext);
    }

    public RecentlyUsedModel genRecentUsedModel() {
        if (localNavContext.getRepoModel() == null) {
            return null;
        }

        RecentlyUsedModel r = new RecentlyUsedModel();
        r.repoId = localNavContext.getRepoModel().repo_id;
        r.repoName = localNavContext.getRepoModel().repo_name;
        r.path = localNavContext.getNavPath();
        r.account = mAccount.getSignature();
        return r;
    }
}

