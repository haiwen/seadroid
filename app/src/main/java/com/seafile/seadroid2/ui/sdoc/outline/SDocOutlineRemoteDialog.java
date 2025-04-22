package com.seafile.seadroid2.ui.sdoc.outline;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.DialogSdocDirectoryBinding;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.ui.sdoc.SDocViewModel;

import java.util.List;

public class SDocOutlineRemoteDialog extends BottomSheetDialogFragment {
    private SDocViewModel viewModel;

    private SDocPageOptionsModel pageOptionsModel;

    private DialogSdocDirectoryBinding binding;
    private SDocOutlineAdapter adapter;

    public static SDocOutlineRemoteDialog newInstance(SDocPageOptionsModel pageOptionsModel) {

        Bundle args = new Bundle();
        args.putParcelable("pageOption", pageOptionsModel);
        SDocOutlineRemoteDialog fragment = new SDocOutlineRemoteDialog();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() == null || !getArguments().containsKey("pageOption")) {
            throw new IllegalArgumentException("pageOption is null");
        }

        pageOptionsModel = getArguments().getParcelable("pageOption");
        viewModel = new ViewModelProvider(this).get(SDocViewModel.class);
    }


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = DialogSdocDirectoryBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @SuppressLint("RestrictedApi")
    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        return new BottomSheetDialog(requireContext());
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        binding.rv.setLayoutManager(new LinearLayoutManager(requireContext()));

        adapter = new SDocOutlineAdapter();
        adapter.setAnimationEnable(true);
        adapter.setStateViewLayout(requireContext(),R.layout.layout_empty);
        adapter.setStateViewEnable(false);
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<OutlineItemModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<OutlineItemModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                OutlineItemModel outlineItemModel = adapter.getItems().get(i);
                dismiss();
            }
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());

        initViewModel();

        load();
    }

    private void initViewModel() {
        viewModel.getSdocElementLiveData().observe(getViewLifecycleOwner(), new Observer<List<OutlineItemModel>>() {
            @Override
            public void onChanged(List<OutlineItemModel> outlineItemModels) {
                adapter.setStateViewEnable(true);
                adapter.submitList(outlineItemModels);
            }
        });
    }

    private void load() {
        viewModel.loadSdocElements(pageOptionsModel);
    }
}
