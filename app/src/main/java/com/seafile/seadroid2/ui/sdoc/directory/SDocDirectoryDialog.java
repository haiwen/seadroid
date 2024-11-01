package com.seafile.seadroid2.ui.sdoc.directory;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.google.android.material.internal.ViewUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.DialogSdocDirectoryBinding;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocWrapperModel;
import com.seafile.seadroid2.ui.sdoc.SDocViewModel;

import java.util.List;

public class SDocDirectoryDialog extends BottomSheetDialogFragment {
    private SDocViewModel viewModel;

    private SDocPageOptionsModel pageOptionsModel;

    private DialogSdocDirectoryBinding binding;
    private SDocDirectoryAdapter adapter;

    public static SDocDirectoryDialog newInstance(SDocPageOptionsModel pageOptionsModel) {

        Bundle args = new Bundle();
        args.putParcelable("pageOption", pageOptionsModel);
        SDocDirectoryDialog fragment = new SDocDirectoryDialog();
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
        BottomSheetDialog bottomSheetDialog = new BottomSheetDialog(requireContext());
//        bottomSheetDialog.setContentView(binding.getRoot());
//
//        View bottomSheetInternal = bottomSheetDialog.findViewById(R.id.design_bottom_sheet);
////        BottomSheetBehavior.from(bottomSheetInternal).setPeekHeight(800);
//
//        View bottomSheetContent = bottomSheetInternal.findViewById(R.id.bottom_drawer_2);
//        ViewUtils.doOnApplyWindowInsets(bottomSheetContent, new ViewUtils.OnApplyWindowInsetsListener() {
//            @Override
//            public WindowInsetsCompat onApplyWindowInsets(View view, WindowInsetsCompat insets, ViewUtils.RelativePadding initialPadding) {
//                // Add the inset in the inner NestedScrollView instead to make the edge-to-edge behavior
//                // consistent - i.e., the extra padding will only show at the bottom of all content, i.e.,
//                // only when you can no longer scroll down to show more content.
//                ViewCompat.setPaddingRelative(bottomSheetContent,
//                        initialPadding.start,
//                        initialPadding.top,
//                        initialPadding.end,
//                        initialPadding.bottom + insets.getInsets(WindowInsetsCompat.Type.systemBars()).bottom);
//                return insets;
//            }
//        });

        return bottomSheetDialog;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        binding.rv.setLayoutManager(new LinearLayoutManager(requireContext()));

        adapter = new SDocDirectoryAdapter();
        adapter.setAnimationEnable(true);
        adapter.setStateViewLayout(requireContext(),R.layout.layout_empty);
        adapter.setStateViewEnable(false);
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<SDocModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<SDocModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                SDocModel sDocModel = adapter.getItems().get(i);
                ToastUtils.showLong(sDocModel.text);
                dismiss();
            }
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());

        initViewModel();

        load();
    }

    private void initViewModel() {
        viewModel.getSdocElementLiveData().observe(getViewLifecycleOwner(), new Observer<List<SDocModel>>() {
            @Override
            public void onChanged(List<SDocModel> sDocModels) {
                adapter.setStateViewEnable(true);
                adapter.submitList(sDocModels);
            }
        });
    }

    private void load() {
        viewModel.getSDocElements(pageOptionsModel);
    }
}
