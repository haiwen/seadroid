package com.seafile.seadroid2.ui.dialog_fragment.related_users;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.DialogSdocDirectoryBinding;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.ui.docs_comment.DocsCommentViewModel;

import java.util.List;


public class RelatedUserBottomSheetDialogFragment extends BottomSheetDialogFragment {
    private DialogSdocDirectoryBinding binding;
    private DocsCommentViewModel docsCommentViewModel;
    private List<UserModel> relatedUserList;
    private RelatedUsersAdapter adapter;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        docsCommentViewModel = new ViewModelProvider(requireActivity()).get(DocsCommentViewModel.class);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = DialogSdocDirectoryBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initAdapter();

        relatedUserList = docsCommentViewModel.getRelatedUsersLiveData().getValue();
        adapter.submitList(relatedUserList);
    }

    private BaseQuickAdapter.OnItemClickListener<UserModel> onItemClickListener;

    public void setOnItemClickListener(BaseQuickAdapter.OnItemClickListener<UserModel> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    private void initAdapter() {
        adapter = new RelatedUsersAdapter();
        adapter.setAnimationEnable(true);
        adapter.setStateViewLayout(requireContext(), R.layout.layout_empty);
        adapter.setStateViewEnable(false);

        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<UserModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<UserModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                if (onItemClickListener != null) {
                    onItemClickListener.onClick(baseQuickAdapter, view, i);
                }

                dismiss();
            }
        });

        binding.rv.setLayoutManager(new LinearLayoutManager(requireContext()));

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());

    }
}
