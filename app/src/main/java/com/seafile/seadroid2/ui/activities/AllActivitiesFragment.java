package com.seafile.seadroid2.ui.activities;

import android.app.Activity;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.base.adapter.LogicLoadMoreAdapter;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.file.FileActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.markdown.MarkdownActivity;
import com.seafile.seadroid2.ui.media.image_preview2.CarouselImagePreviewActivity;
import com.seafile.seadroid2.ui.media.player.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.sdoc.SDocWebViewActivity;
import com.seafile.seadroid2.view.TipsViews;

import java.io.File;

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

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);

        SLogs.i(newConfig.uiMode);

    }

    @Override
    public void onFirstResume() {
        super.onFirstResume();
        loadNext();
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

        LogicLoadMoreAdapter loadMoreAdapter = getLogicLoadMoreAdapter();

        helper = new QuickAdapterHelper.Builder(adapter)
                .setTrailingLoadStateAdapter(loadMoreAdapter)
                .build();
        binding.rv.setAdapter(helper.getAdapter());
    }

    @NonNull
    private LogicLoadMoreAdapter getLogicLoadMoreAdapter() {
        LogicLoadMoreAdapter loadMoreAdapter = new LogicLoadMoreAdapter();
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
        return loadMoreAdapter;
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
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(getViewLifecycleOwner(), exceptionPair -> showErrorTip());

        getViewModel().getListLiveData().observe(getViewLifecycleOwner(), activityModels -> {
            adapter.setStateViewEnable(true);

            if (page == 1) {
                adapter.submitList(activityModels);
            } else {
                adapter.addAll(activityModels);
            }

            if (activityModels.isEmpty()) {
                helper.setTrailingLoadState(new LoadState.NotLoading(true));
                if (helper.getTrailingLoadStateAdapter() != null) {
                    helper.getTrailingLoadStateAdapter().checkDisableLoadMoreIfNotFullPage();
                }
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
    private void checkOpen(ActivityModel model) {
        if ("delete".equals(model.op_type)) {
            if ("repo".equals(model.obj_type)) {
                ToastUtils.showLong(getString(R.string.library_not_found));
            } else if ("dir".equals(model.obj_type)) {
                ToastUtils.showLong(getString(R.string.op_exception_folder_deleted, model.name));
            } else if ("file".equals(model.obj_type)) {
                ToastUtils.showLong(getString(R.string.file_not_found, model.name));
            }
            return;
        }

        getViewModel().getRepoModelFromLocal(model.repo_id, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel == null) {
                    ToastUtils.showLong(R.string.repo_not_found);
                    return;
                }

                if (repoModel.encrypted) {
                    showPasswordDialog(repoModel, model);
                } else {
                    navTo(repoModel, model);
                }
            }
        });
    }

    private void navTo(RepoModel repoModel, ActivityModel model) {

        if ("repo".equals(model.obj_type)) {
            switch (model.op_type) {
                case "create":
                case "update":
                case "edit":
                    MainActivity.navToThis(requireContext(), model.repo_id, model.repo_name, "/", true);
                    break;
                default:
                    ToastUtils.showLong(R.string.not_supported);
                    break;
            }

        } else if ("dir".equals(model.obj_type)) {
            switch (model.op_type) {
                case "create":
                case "update":
                case "edit":
                    MainActivity.navToThis(requireContext(), model.repo_id, model.repo_name, model.path, true);
                    break;
                default:
                    ToastUtils.showLong(R.string.not_supported);
                    break;
            }
        } else if ("file".equals(model.obj_type)) {
            switch (model.op_type) {
                case "create":
                case "update":
                case "edit":
                    open(repoModel, model);
                    break;
                default:
                    ToastUtils.showLong(R.string.not_supported);
                    break;
            }
        }
    }

    private void open(RepoModel repoModel, ActivityModel activityModel) {
        if (repoModel.encrypted) {

            File file = getLocalDestinationFile(activityModel.repo_id, activityModel.repo_name, activityModel.path);
            if (file.exists()) {
                WidgetUtils.openWith(requireContext(), file);
            } else {
                Intent intent = FileActivity.startFromActivity(requireContext(), activityModel, "open_with");
                fileActivityLauncher.launch(intent);
            }

        } else if (Utils.isViewableImage(activityModel.name)) {

            Intent getIntent = CarouselImagePreviewActivity.startThisFromActivity(requireContext(), activityModel);
            imagePreviewActivityLauncher.launch(getIntent);

        } else if (activityModel.name.endsWith(Constants.Format.DOT_SDOC)) {

            SDocWebViewActivity.openSdoc(getContext(), activityModel.repo_name, activityModel.repo_id, activityModel.path);

        } else if (Utils.isVideoFile(activityModel.name)) {

            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
            builder.setItems(R.array.video_download_array, (dialog, which) -> {
                if (which == 0) {
                    CustomExoVideoPlayerActivity.startThis(getContext(), activityModel.name, activityModel.repo_id, activityModel.path);
                } else if (which == 1) {
                    Intent intent = FileActivity.startFromActivity(requireContext(), activityModel, "video_download");
                    fileActivityLauncher.launch(intent);
                }
            }).show();
        } else if (Utils.isTextMimeType(activityModel.name)) {

            File file = getLocalDestinationFile(activityModel.repo_id, activityModel.repo_name, activityModel.path);
            //check need to update
            if (file.exists()) {
                MarkdownActivity.start(requireContext(), file.getAbsolutePath(), activityModel.repo_id, activityModel.path);
            } else {
                Intent intent = FileActivity.startFromActivity(requireContext(), activityModel, "open_markdown");
                fileActivityLauncher.launch(intent);
            }
        } else {

            //Open with another app
            openWith(activityModel);
        }
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        return DataManager.getLocalRepoFile(account, repoId, repoName, fullPathInRepo);
    }


    private void showPasswordDialog(RepoModel repoModel, ActivityModel activityModel) {
        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance(repoModel.repo_id, repoModel.repo_name);
        dialogFragment.setRefreshListener(isDone -> {
            if (isDone) {
                navTo(repoModel, activityModel);
            }
        });
        dialogFragment.show(getChildFragmentManager(), PasswordDialogFragment.class.getSimpleName());
    }


    private void startPlayActivity(String fileName, String repoID, String filePath) {
        CustomExoVideoPlayerActivity.startThis(getContext(), fileName, repoID, filePath);
    }

    private void openWith(ActivityModel model) {
        File local = getLocalDestinationFile(model.repo_id, model.repo_name, model.path);
        if (local.exists()) {
            WidgetUtils.openWith(requireContext(), local);
        } else {
            Intent intent = FileActivity.startFromActivity(requireContext(), model, "open_with");
            fileActivityLauncher.launch(intent);
        }
    }

    private final ActivityResultLauncher<Intent> imagePreviewActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                return;
            }

            reload();
        }
    });

    private final ActivityResultLauncher<Intent> fileActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                return;
            }

            String action = o.getData().getStringExtra("action");
            String repoId = o.getData().getStringExtra("repo_id");
            String targetFile = o.getData().getStringExtra("target_file");
            String localFullPath = o.getData().getStringExtra("destination_path");
            boolean isUpdateWhenFileExists = o.getData().getBooleanExtra("is_update", false);

            if (TextUtils.isEmpty(localFullPath)) {
                return;
            }

            if (isUpdateWhenFileExists) {
                ToastUtils.showLong(R.string.download_finished);
            }


            File destinationFile = new File(localFullPath);
            if ("open_with".equals(action)) {
                WidgetUtils.openWith(requireContext(), destinationFile);
            } else if ("video_download".equals(action)) {
                //
            } else if ("open_markdown".equals(action)) {

                MarkdownActivity.start(requireContext(), localFullPath, repoId, targetFile);
            }
        }
    });
}
