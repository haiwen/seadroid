package com.seafile.seadroid2.ui.star;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.model.star.StarredModel;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.main.MainViewModel;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewActivity;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.view.TipsViews;

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

    private boolean isFirstLoadData = true;

    @Override
    public void onResume() {
        super.onResume();
        d("load data：onResume");
        if (isFirstLoadData) {
            isFirstLoadData = false;
            d("load data：isFirstLoadData");
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
            if (!starredModel.deleted) {
                navTo(starredModel);
            } else if (starredModel.isRepo()) {
                ToastUtils.showLong(getString(R.string.library_not_found));
            } else if (starredModel.is_dir) {
                ToastUtils.showLong(getString(R.string.op_exception_folder_deleted, starredModel.obj_name));
            } else {
                ToastUtils.showLong(getString(R.string.file_not_found, starredModel.obj_name));
            }

        });

        adapter.addOnItemChildClickListener(R.id.item_action, new BaseQuickAdapter.OnItemChildClickListener<StarredModel>() {
            @Override
            public void onItemClick(@NonNull BaseQuickAdapter<StarredModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                showBottomSheet(adapter.getItems().get(i));
            }
        });

        binding.rv.setAdapter(createAdapterHelper(adapter).getAdapter());
    }

    private void showErrorTip() {
        adapter.submitList(null);
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.error_when_load_starred);
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

        getViewModel().getExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<Pair<Integer, SeafException>>() {
            @Override
            public void onChanged(Pair<Integer, SeafException> exceptionPair) {
                showErrorTip();
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
        int rid = R.menu.bottom_sheet_unstarred;
        BottomSheetHelper.showSheet(getActivity(), rid, menuItem -> {
            if (menuItem.getItemId() == R.id.unstar) {
                getViewModel().unStarItem(model.repo_id, model.path);
            }
        });
    }

    private void navTo(StarredModel model) {
        if (model.is_dir) {
            MainActivity.navToThis(requireContext(), model.repo_id, model.repo_name, model.path, model.is_dir);
        } else if (Utils.isViewableImage(model.obj_name)) {
            ImagePreviewActivity.startThis(requireContext(), model.repo_id, model.path);
        } else {
            String host = IO.getSingleton().getServerUrl();
            String url = String.format("%slib/%s/file%s", host, model.repo_id, model.path);
            SeaWebViewActivity.openUrl(requireContext(),url);
        }
    }
}

