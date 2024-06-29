package com.seafile.seadroid2.ui.star;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.MenuItem;
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
import androidx.lifecycle.ViewModelProvider;

import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetMenuFragment;
import com.seafile.seadroid2.bottomsheetmenu.OnMenuClickListener;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.ui.file.FileActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.main.MainViewModel;
import com.seafile.seadroid2.ui.markdown.MarkdownActivity;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewActivity;
import com.seafile.seadroid2.ui.media.player.exoplayer.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.view.TipsViews;

import java.io.File;
import java.util.List;

import kotlin.Pair;

public class StarredQuickFragment extends BaseFragmentWithVM<StarredViewModel> {
    private MainViewModel mainViewModel;
    private LayoutFrameSwipeRvBinding binding;
    private StarredAdapter adapter;

    public static StarredQuickFragment newInstance() {
        Bundle args = new Bundle();
        StarredQuickFragment fragment = new StarredQuickFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (getViewModel() != null) {
            getViewModel().disposeAll();
        }
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mainViewModel = new ViewModelProvider(requireActivity()).get(MainViewModel.class);
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

    private boolean isForce = false;

    @Override
    public void onFirstResume() {
        super.onFirstResume();
        reload();
    }

    @Override
    public void onResume() {
        super.onResume();
        if (isForce) {
            reload();
        }
    }

    private void initAdapter() {
        adapter = new StarredAdapter();
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.no_starred_file);
        tipView.setOnClickListener(v -> reload());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(false);

        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {

            StarredModel starredModel = adapter.getItems().get(i);
            navTo(starredModel);

        });

        adapter.addOnItemChildClickListener(R.id.item_action, new BaseQuickAdapter.OnItemChildClickListener<StarredModel>() {
            @Override
            public void onItemClick(@NonNull BaseQuickAdapter<StarredModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                showBottomSheet(adapter.getItems().get(i));
            }
        });

        binding.rv.setAdapter(createAdapterHelper(adapter).getAdapter());
    }

    private void showErrorTip(SeafException seafException) {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.error_when_load_starred);
        tipView.setOnClickListener(v -> reload());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);
    }

    private void initViewModel() {
        mainViewModel.getOnForceRefreshStarredListLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                isForce = aBoolean;
            }
        });

        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException seafException) {
                showErrorTip(seafException);
            }
        });

        getViewModel().getListLiveData().observe(getViewLifecycleOwner(), new Observer<List<StarredModel>>() {
            @Override
            public void onChanged(List<StarredModel> starredModels) {
                adapter.setStateViewEnable(true);

                adapter.notifyDataChanged(starredModels);
            }
        });

        getViewModel().getUnStarredResultLiveData().observe(getViewLifecycleOwner(), new Observer<Pair<String, ResultModel>>() {
            @Override
            public void onChanged(Pair<String, ResultModel> pair) {
                if (pair.getSecond().success) {
                    ToastUtils.showLong(R.string.success);

                    mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);

                    reload();
                }
            }
        });
    }

    private void reload() {
        adapter.setStateViewEnable(false);
        getViewModel().loadData();
    }

    private void showBottomSheet(StarredModel model) {
        BottomSheetMenuFragment.Builder builder = BottomSheetHelper.buildSheet(requireActivity(), R.menu.bottom_sheet_unstarred, new OnMenuClickListener() {
            @Override
            public void onMenuClick(MenuItem menuItem) {
                if (menuItem.getItemId() == R.id.nav_to) {
                    MainActivity.navToThis(requireContext(), model.repo_id, model.repo_name, model.path, model.is_dir);
                } else if (menuItem.getItemId() == R.id.unstar) {
                    getViewModel().unStarItem(model.repo_id, model.path);
                }
            }
        });

        if (model.deleted) {
            builder.removeMenu(R.id.nav_to);
        }

        builder.show(getChildFragmentManager());
    }

    private void navTo(StarredModel starredModel) {
        if (!starredModel.deleted) {
            open(starredModel);
        } else if (starredModel.isRepo()) {
            ToastUtils.showLong(getString(R.string.library_not_found));
        } else if (starredModel.is_dir) {
            ToastUtils.showLong(getString(R.string.op_exception_folder_deleted, starredModel.obj_name));
        } else {
            ToastUtils.showLong(getString(R.string.file_not_found, starredModel.obj_name));
        }
    }

    private void open(StarredModel model) {
        if (model.is_dir) {
            MainActivity.navToThis(requireContext(), model.repo_id, model.repo_name, model.path, model.is_dir);
        } else if (model.repo_encrypted) {

            File file = getLocalDestinationFile(model.repo_id, model.repo_name, model.path);
            if (file.exists()) {
                WidgetUtils.openWith(requireContext(), file);
            } else {
                Intent intent = FileActivity.startFromStarred(requireContext(), model, "open_with");
                fileActivityLauncher.launch(intent);
            }

        } else if (Utils.isViewableImage(model.obj_name)) {

            Intent getIntent = ImagePreviewActivity.startThisFromStarred(requireContext(), model);
            imagePreviewActivityLauncher.launch(getIntent);

        } else if (model.obj_name.endsWith(Constants.Format.DOT_SDOC)) {

            SeaWebViewActivity.openSdoc(getContext(), model.repo_name, model.repo_id, model.path);

        } else if (Utils.isVideoFile(model.obj_name)) {

            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
            builder.setItems(R.array.video_download_array, (dialog, which) -> {
                if (which == 0) {
                    CustomExoVideoPlayerActivity.startThis(getContext(), model.obj_name, model.repo_id, model.path);
                } else if (which == 1) {
                    Intent intent = FileActivity.startFromStarred(requireContext(), model, "video_download");
                    fileActivityLauncher.launch(intent);
                }
            }).show();
        } else if (Utils.isTextMimeType(model.obj_name)) {

            File file = getLocalDestinationFile(model.repo_id, model.repo_name, model.path);
            //check need to update
            if (file.exists()) {
                MarkdownActivity.start(requireContext(), file.getAbsolutePath(), model.repo_id, model.path);
            } else {
                Intent intent = FileActivity.startFromStarred(requireContext(), model, "open_markdown");
                fileActivityLauncher.launch(intent);
            }
        } else {

            //Open with another app
            openWith(model);
        }
    }

    private File getLocalDestinationFile(String repoId, String repoName, String fullPathInRepo) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        return DataManager.getLocalRepoFile(account, repoId, repoName, fullPathInRepo);
    }

    private void openWith(StarredModel model) {
        File local = getLocalDestinationFile(model.repo_id, model.repo_name, model.path);
        if (local.exists()) {
            WidgetUtils.openWith(requireContext(), local);
        } else {
            Intent intent = FileActivity.startFromStarred(requireContext(), model, "open_with");
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

