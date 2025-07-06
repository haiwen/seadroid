package com.seafile.seadroid2.ui.activities;

import android.app.Activity;
import android.content.Intent;
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
import androidx.annotation.OptIn;
import androidx.lifecycle.Observer;
import androidx.media3.common.util.UnstableApi;

import com.chad.library.adapter4.QuickAdapterHelper;
import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.enums.FileReturnActionEnum;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.base.adapter.LogicLoadMoreAdapter;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.BottomSheetPasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.ui.file.FileActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.markdown.MarkdownActivity;
import com.seafile.seadroid2.ui.media.image.CarouselImagePreviewActivity;
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

    private ActivityResultLauncher<Intent> fileActivityLauncher;
    private ActivityResultLauncher<Intent> imagePreviewActivityLauncher;

    public static AllActivitiesFragment newInstance() {
        Bundle args = new Bundle();
        AllActivitiesFragment fragment = new AllActivitiesFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putInt("page", page);
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (savedInstanceState != null) {
            page = savedInstanceState.getInt("page");
        }
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.swipeRefreshLayout.setOnRefreshListener(this::reload);

        registerLauncher();

        initAdapter();

        initViewModel();

    }

    private void registerLauncher() {

        imagePreviewActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
            @Override
            public void onActivityResult(ActivityResult o) {
                if (o.getResultCode() != Activity.RESULT_OK) {
                    return;
                }

                reload();
            }
        });

        fileActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
            @Override
            public void onActivityResult(ActivityResult o) {
                if (o.getResultCode() != Activity.RESULT_OK) {
                    return;
                }

                Intent data = o.getData();
                if (o.getData() == null) {
                    return;
                }

                String action = data.getStringExtra("action");
                String repoId = data.getStringExtra("repo_id");
                String targetFile = data.getStringExtra("target_file");
                String localFullPath = data.getStringExtra("destination_path");
                boolean isUpdateWhenFileExists = data.getBooleanExtra("is_update", false);

                if (TextUtils.isEmpty(localFullPath)) {
                    return;
                }

                if (isUpdateWhenFileExists) {
                    Toasts.show(R.string.download_finished);
                }


                File destinationFile = new File(localFullPath);

                if (TextUtils.equals(FileReturnActionEnum.EXPORT.name(), action)) {
                    //nothing to do
                } else if (TextUtils.equals(FileReturnActionEnum.SHARE.name(), action)) {
                    //nothing to do
                } else if (TextUtils.equals(FileReturnActionEnum.DOWNLOAD_VIDEO.name(), action)) {
                    //nothing to do
                } else if (TextUtils.equals(FileReturnActionEnum.OPEN_WITH.name(), action)) {

                    WidgetUtils.openWith(requireContext(), destinationFile);
                } else if (TextUtils.equals(FileReturnActionEnum.OPEN_TEXT_MIME.name(), action)) {

                    MarkdownActivity.start(requireContext(), localFullPath, repoId, targetFile);
                }
            }
        });
    }


    @Override
    public void onFirstResume() {
        super.onFirstResume();
        loadNext();
    }


    private void initAdapter() {
        adapter = new ActivityAdapter();

        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.no_more_activities);
        tipView.setOnClickListener(v -> reload());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(false);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            ActivityModel activityModel = (ActivityModel) adapter.getItems().get(i);
            checkAndOpen(activityModel);
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
        getViewModel().getSecondRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoadingDialog(aBoolean);
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

    private void checkAndOpen(ActivityModel model) {
        if ("delete".equals(model.op_type)) {
            if ("repo".equals(model.obj_type)) {
                Toasts.show(getString(R.string.library_not_found));
            } else if ("dir".equals(model.obj_type)) {
                Toasts.show(getString(R.string.op_exception_folder_deleted, model.name));
            } else if ("file".equals(model.obj_type)) {
                Toasts.show(getString(R.string.file_not_found, model.name));
            }
            return;
        }

        getViewModel().getRepoModelFromLocal(model.repo_id, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel == null) {
                    Toasts.show(R.string.repo_not_found);
                    return;
                }

                navTo(repoModel, model);
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
                    Toasts.show(R.string.not_supported);
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
                    Toasts.show(R.string.not_supported);
                    break;
            }
        } else if ("file".equals(model.obj_type)) {
            switch (model.op_type) {
                case "create":
                case "update":
                case "edit":
                    decryptRepo(repoModel, model);
                    break;
                default:
                    Toasts.show(R.string.not_supported);
                    break;
            }
        }
    }

    private void showPasswordDialogCallback(String repo_id, String repo_name, OnResultListener<RepoModel> resultListener) {
        BottomSheetPasswordDialogFragment dialogFragment = BottomSheetPasswordDialogFragment.newInstance(repo_id, repo_name);
        dialogFragment.setResultListener(resultListener);
        dialogFragment.show(getChildFragmentManager(), BottomSheetPasswordDialogFragment.class.getSimpleName());
    }

    private void decryptRepo(RepoModel repoModel, ActivityModel model) {
        if (repoModel.encrypted) {
            getViewModel().decryptRepo(model.repo_id, new Consumer<String>() {
                @Override
                public void accept(String i) throws Exception {
                    if (TextUtils.equals(i, "need-to-re-enter-password")) {
                        showPasswordDialogCallback(model.repo_id, model.repo_name, new OnResultListener<RepoModel>() {
                            @Override
                            public void onResultData(RepoModel repoModel) {
                                if (repoModel != null) {
                                    open(model);
                                }
                            }
                        });
                    } else if (TextUtils.equals(i, "done")) {
                        open(model);
                    } else {
                        getViewModel().remoteVerify(model.repo_id, i, new Consumer<ResultModel>() {
                            @Override
                            public void accept(ResultModel r) throws Exception {
                                if (r.success) {
                                    open(model);
                                } else {
                                    Toasts.show(r.error_msg);
                                    showPasswordDialogCallback(model.repo_id, model.repo_name, new OnResultListener<RepoModel>() {
                                        @Override
                                        public void onResultData(RepoModel repoModel) {
                                            if (repoModel != null) {
                                                open(model);
                                            }
                                        }
                                    });
                                }
                            }
                        });
                    }
                }
            });
        } else {
            open(model);
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void open(ActivityModel activityModel) {
        if (Utils.isViewableImage(activityModel.name)) {

            Intent getIntent = CarouselImagePreviewActivity.startThisFromActivities(requireContext(), activityModel);
            imagePreviewActivityLauncher.launch(getIntent);

        } else if (activityModel.name.endsWith(Constants.Format.DOT_SDOC)) {
            SDocWebViewActivity.openSdoc(getContext(), activityModel.repo_name, activityModel.repo_id, activityModel.path, activityModel.name);

        } else if (activityModel.name.endsWith(Constants.Format.DOT_DRAW) || activityModel.name.endsWith(Constants.Format.DOT_EXDRAW)) {
            SDocWebViewActivity.openDraw(getContext(), activityModel.repo_name, activityModel.repo_id, activityModel.path, activityModel.name);

        } else if (Utils.isVideoFile(activityModel.name)) {

            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
            builder.setItems(R.array.video_download_array, (dialog, which) -> {
                if (which == 0) {
                    CustomExoVideoPlayerActivity.startThis(getContext(), activityModel.name, activityModel.repo_id, activityModel.path, null);
                } else if (which == 1) {
                    Intent intent = FileActivity.startFromActivity(requireContext(), activityModel, FileReturnActionEnum.DOWNLOAD_VIDEO);
                    fileActivityLauncher.launch(intent);
                }
            }).show();
        } else if (Utils.isTextMimeType(activityModel.name)) {
            openWith(activityModel, FileReturnActionEnum.OPEN_TEXT_MIME);
        } else {
            //Open with another app
            openWith(activityModel, FileReturnActionEnum.OPEN_WITH);
        }
    }


    private void openWith(ActivityModel model, FileReturnActionEnum actionEnum) {
        getViewModel().checkRemoteAndOpen(model.repo_id, model.path, new Consumer<String>() {
            @Override
            public void accept(String fileId) {
                File local = getLocalDestinationFile(model.repo_id, model.repo_name, model.path);
                if (!TextUtils.isEmpty(fileId) && local.exists()) {

                    if (TextUtils.equals(FileReturnActionEnum.OPEN_WITH.name(), actionEnum.name())) {

                        WidgetUtils.openWith(requireContext(), local);
                    } else if (TextUtils.equals(FileReturnActionEnum.OPEN_TEXT_MIME.name(), actionEnum.name())) {
                        MarkdownActivity.start(requireContext(), local.getAbsolutePath(), model.repo_id, model.path);

                    }
                } else {
                    Intent intent = FileActivity.startFromActivity(requireContext(), model, FileReturnActionEnum.OPEN_WITH);
                    fileActivityLauncher.launch(intent);
                }
            }
        });
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        return DataManager.getLocalFileCachePath(account, repoId, repoName, fullPathInRepo);
    }


}
