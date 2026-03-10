package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.Glide;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.FragmentSelectorCollaboratorBinding;
import com.seafile.seadroid2.databinding.ItemAvatarUserOptionalBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorBinding;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.transport.TransportHolder;
import com.seafile.seadroid2.ui.adapter.CustomLoadMoreAdapter;
import com.seafile.seadroid2.ui.base.fragment.BaseBottomSheetDialogFragment;
import com.seafile.seadroid2.view.LeftMarginDividerItemDecoration;

import java.util.List;

public class CollaboratorSelectorFragment extends BaseBottomSheetDialogFragment {
    private FragmentSelectorCollaboratorBinding binding;
    private ToolbarActionbarForSelectorBinding toolbarBinding;

    private CollaboratorSelectorAdapter adapter;
    private QuickAdapterHelper helper;
    private List<UserModel> userList;
    private List<UserModel> checkedUserList;

    public static CollaboratorSelectorFragment newInstance(List<UserModel> userList, List<UserModel> checkedUserList) {
        TransportHolder.get().put("user_list", userList);
        TransportHolder.get().put("checked_user_list", userList);

        return new CollaboratorSelectorFragment();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        userList = TransportHolder.get().get("user_list");
        checkedUserList = TransportHolder.get().get("checked_user_list");

        if (CollectionUtils.isEmpty(userList)) {
            throw new IllegalArgumentException("userList is null");
        }

    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentSelectorCollaboratorBinding.inflate(inflater, container, false);
        toolbarBinding = ToolbarActionbarForSelectorBinding.bind(binding.toolbar.getRoot());

        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initView();
        initAdapter();

        loadData();
    }

    protected void initView() {
        binding.rv.setLayoutManager(new LinearLayoutManager(requireContext()));
        binding.rv.setNestedScrollingEnabled(true);
        binding.rv.addItemDecoration(new LeftMarginDividerItemDecoration(
                SizeUtils.dp2px(16),
                ContextCompat.getColor(requireContext(), R.color.fancy_dark_gray)));

        toolbarBinding.done.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                onDone();
            }
        });
        toolbarBinding.cancel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                dismiss();
            }
        });

    }

    private void initAdapter() {
        adapter = new CollaboratorSelectorAdapter();
        adapter.setStateViewEnable(true);

        CustomLoadMoreAdapter customLoadMoreAdapter = new CustomLoadMoreAdapter();

        helper = new QuickAdapterHelper.Builder(adapter)
                .setTrailingLoadStateAdapter(customLoadMoreAdapter)
                .build();

        binding.rv.setAdapter(helper.getAdapter());

    }

    private void loadData() {
        adapter.submitList(userList);
    }

    private void onDone() {
        dismiss();
    }

    public static class CollaboratorSelectorAdapter extends BaseQuickAdapter<UserModel, CollaboratorSelectorViewHolder> {

        @Override
        protected void onBindViewHolder(@NonNull CollaboratorSelectorViewHolder holder, int i, @Nullable UserModel model) {
            final int p = i;
            holder.binding.userName.setText(model.getName());

            Glide.with(getContext())
                    .load(GlideLoadConfig.getGlideUrl(model.getAvatarUrl()))
                    .apply(GlideLoadConfig.getOptions())
                    .into(holder.binding.userAvatar);

            holder.binding.userSelected.setVisibility(model.isSelected() ? View.VISIBLE : View.GONE);

            holder.binding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    model.setSelected(!model.isSelected());
                    notifyItemChanged(p);
                }
            });

        }

        @NonNull
        @Override
        protected CollaboratorSelectorViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
            ItemAvatarUserOptionalBinding binding = ItemAvatarUserOptionalBinding.inflate(LayoutInflater.from(viewGroup.getContext()), viewGroup, false);
            return new CollaboratorSelectorViewHolder(binding);
        }
    }

    public static class CollaboratorSelectorViewHolder extends RecyclerView.ViewHolder {
        public ItemAvatarUserOptionalBinding binding;

        public CollaboratorSelectorViewHolder(@NonNull ItemAvatarUserOptionalBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
