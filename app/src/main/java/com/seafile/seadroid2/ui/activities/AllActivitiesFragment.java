package com.seafile.seadroid2.ui.activities;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.ui.base.adapter.CustomLoadMoreAdapter;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.data.model.activities.ActivityModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.play.exoplayer.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.view.TipsViews;

import io.reactivex.functions.Consumer;

public class AllActivitiesFragment extends BaseFragmentWithVM<ActivityViewModel> {
    private LayoutFrameSwipeRvBinding binding;
    private ActivityAdapter adapter;
    private QuickAdapterHelper helper;
    private int page = 0;

    public static AllActivitiesFragment newInstance() {
        Bundle args = new Bundle();
        AllActivitiesFragment fragment = new AllActivitiesFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.swipeRefreshLayout.setOnRefreshListener(this::reload);
        return binding.getRoot();
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initAdapter();

        initViewModel();
    }

    private boolean isFirstLoadData = true;

    @Override
    public void onResume() {
        super.onResume();
        d("load data：onResume");
        if (isFirstLoadData) {
            isFirstLoadData = false;
            d("load data：isFirstLoadData");
            loadNext();
        }
    }

    private void initAdapter() {
        adapter = new ActivityAdapter();
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.no_starred_file);
        tipView.setOnClickListener(v -> reload());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(false);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            ActivityModel activityModel = (ActivityModel) adapter.getItems().get(i);
            checkOpen(activityModel);
        });

        CustomLoadMoreAdapter loadMoreAdapter = new CustomLoadMoreAdapter();
        loadMoreAdapter.setOnLoadMoreListener(new TrailingLoadStateAdapter.OnTrailingListener() {
            @Override
            public void onLoad() {
                loadNext();
            }

            @Override
            public void onFailRetry() {
                page--;
                loadNext();
            }

            @Override
            public boolean isAllowLoading() {
                return !binding.swipeRefreshLayout.isRefreshing();
            }
        });

        helper = new QuickAdapterHelper.Builder(adapter)
                .setTrailingLoadStateAdapter(loadMoreAdapter)
                .build();
        binding.rv.setAdapter(helper.getAdapter());
    }

    private void showErrorTip() {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.error_when_load_activities);
        tipView.setOnClickListener(v -> reload());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), binding.swipeRefreshLayout::setRefreshing);

        getViewModel().getExceptionLiveData().observe(getViewLifecycleOwner(), exceptionPair -> showErrorTip());

        getViewModel().getListLiveData().observe(getViewLifecycleOwner(), activityModels -> {
            adapter.setStateViewEnable(true);

            if (page == 1) {
                adapter.submitList(activityModels);
            } else {
                adapter.addAll(activityModels);
            }

            if (activityModels.isEmpty()) {
                helper.setTrailingLoadState(new LoadState.NotLoading(true));
            } else {
                helper.setTrailingLoadState(new LoadState.NotLoading(false));
            }
        });
    }

    private void loadNext() {
        page++;

        getViewModel().loadAllData(page);
    }

    private void reload() {
        adapter.setStateViewEnable(false);
        page = 0;

        loadNext();
    }

    //https://dev.seafile.com/seahub/mobile-login/?next=https://dev.seafile.com/seahub/lib/92d156fd-22ca-485a-a867-390e3e9ce43c/file/在线文档开发计划.sdoc
    private void checkOpen(ActivityModel activityModel) {
        if (activityModel.isDir()) {
            return;
        }

        //TODO repo is encrypted
        getViewModel().getRepoModelFromLocal(activityModel.repo_id, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel == null) {
                    ToastUtils.showLong(R.string.repo_not_found);
                    return;
                }
                if (repoModel.encrypted) {
                    showPasswordDialog(repoModel, activityModel);
                } else {
                    open(activityModel);
                }
            }
        });
    }

    private void showPasswordDialog(RepoModel repoModel, ActivityModel activityModel) {
        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance();
        dialogFragment.initData(repoModel.repo_id, repoModel.repo_name);
        dialogFragment.setRefreshListener(isDone -> {
            if (isDone) {
                open(activityModel);
            }
        });
        dialogFragment.show(getChildFragmentManager(), PasswordDialogFragment.class.getSimpleName());
    }

    private void open(ActivityModel activityModel) {
        if (Utils.isViewableImage(activityModel.name)) {
            ToastUtils.showLong("TODO: 图片预览");
//            WidgetUtils.startGalleryActivity(requireActivity(), activityModel.repo_name, activityModel.repo_id, activityModel.path, activityModel.name,
//                    SupportAccountManager.getInstance().getCurrentAccount());
        } else if (Utils.isVideoFile(activityModel.name)) {
            startPlayActivity(activityModel.name, activityModel.repo_id, activityModel.path);
        } else if (activityModel.isFileOpenable()) {
            String host = IO.getSingleton().getServerUrl();
            String url = String.format("%slib/%s/file/%s", host, activityModel.repo_id, activityModel.path);
            SeaWebViewActivity.openUrl(requireContext(), url);
        }
    }

    private void startPlayActivity(String fileName, String repoID, String filePath) {
        Intent intent = new Intent(requireContext(), CustomExoVideoPlayerActivity.class);
        intent.putExtra("fileName", fileName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", SupportAccountManager.getInstance().getCurrentAccount());
        startActivity(intent);
    }
}
