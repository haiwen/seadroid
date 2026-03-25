package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.os.Bundle;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.ViewModelProvider;
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
import com.seafile.seadroid2.databinding.ItemUserAvatarBinding;
import com.seafile.seadroid2.databinding.ItemUserSelectorBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorWithDragBinding;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.transport.TransportHolder;
import com.seafile.seadroid2.ui.adapter.CustomLoadMoreAdapter;
import com.seafile.seadroid2.ui.base.fragment.BaseBottomSheetDialogFragment;
import com.seafile.seadroid2.ui.media.image.ImagePreviewViewModel;
import com.seafile.seadroid2.ui.sdoc.SDocViewModel;
import com.seafile.seadroid2.view.LeftMarginDividerItemDecoration;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.stream.Collectors;

public class CollaboratorSelectorFragment extends BaseBottomSheetDialogFragment {
    private FragmentSelectorCollaboratorBinding binding;
    private ToolbarActionbarForSelectorWithDragBinding toolbarBinding;

    private CollaboratorSelectorAdapter adapter;
    private List<UserModel> userList;
    private List<UserModel> checkedUserList;
    private String columnKey;

    private SDocViewModel sDocViewModel;

    public static CollaboratorSelectorFragment newInstance(String columnKey,List<UserModel> userList, List<UserModel> checkedUserList) {
        TransportHolder.get().put("columnKey", columnKey);
        TransportHolder.get().put("user_list", userList);
        TransportHolder.get().put("checked_user_list", checkedUserList);

        return new CollaboratorSelectorFragment();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        columnKey = TransportHolder.get().get("columnKey");
        userList = TransportHolder.get().get("user_list");
        checkedUserList = TransportHolder.get().get("checked_user_list");

        TransportHolder.get().remove("columnKey");
        TransportHolder.get().remove("user_list");
        TransportHolder.get().remove("checked_user_list");

        sDocViewModel = new ViewModelProvider(requireActivity()).get(SDocViewModel.class);

        if (CollectionUtils.isEmpty(userList)) {
            throw new IllegalArgumentException("userList is null");
        }

    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentSelectorCollaboratorBinding.inflate(inflater, container, false);
        toolbarBinding = ToolbarActionbarForSelectorWithDragBinding.bind(binding.toolbar.getRoot());

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
                ContextCompat.getColor(requireContext(), R.color.divider_color)));

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
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<UserModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<UserModel, ?> baseQuickAdapter, @NonNull View view, int i) {

                adapter.getItems().get(i).setSelected(!adapter.getItems().get(i).isSelected());
                baseQuickAdapter.notifyItemChanged(i);
            }
        });

        binding.rv.setAdapter(adapter);
    }

    private void loadData() {

        if (CollectionUtils.isNotEmpty(checkedUserList)){
            for (UserModel cModel : checkedUserList) {
                for (UserModel uModel : userList) {
                    if (StringUtils.equals(cModel.getEmail(),uModel.getEmail())){
                        uModel.setSelected(true);
                        break;
                    }
                }
            }
        }

        adapter.submitList(userList);
    }

    private List<UserModel> getSelectedList() {
        return adapter.getItems()
                .stream()
                .filter(UserModel::isSelected)
                .collect(Collectors.toList());
    }

    private void onDone() {

        Pair<String,List<UserModel>> pair = new Pair<>(columnKey,getSelectedList());
        sDocViewModel.getOnUserSelectedLiveData().setValue(pair);

        dismiss();
    }

    public static class CollaboratorSelectorAdapter extends BaseQuickAdapter<UserModel, CollaboratorSelectorViewHolder> {

        @Override
        protected void onBindViewHolder(@NonNull CollaboratorSelectorViewHolder holder, int i, @Nullable UserModel model) {
            if (model == null){
                return;
            }

            holder.binding.userName.setText(model.getName());

            Glide.with(getContext())
                    .load(GlideLoadConfig.getGlideUrl(model.getAvatarUrl()))
                    .apply(GlideLoadConfig.getOptions())
                    .into(holder.binding.userAvatar);

            holder.binding.userSelected.setVisibility(model.isSelected() ? View.VISIBLE : View.GONE);
        }

        @NonNull
        @Override
        protected CollaboratorSelectorViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
            ItemUserSelectorBinding binding = ItemUserSelectorBinding.inflate(LayoutInflater.from(viewGroup.getContext()), viewGroup, false);
            return new CollaboratorSelectorViewHolder(binding);
        }
    }

    public static class CollaboratorSelectorViewHolder extends RecyclerView.ViewHolder {
        public ItemUserSelectorBinding binding;

        public CollaboratorSelectorViewHolder(@NonNull ItemUserSelectorBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
