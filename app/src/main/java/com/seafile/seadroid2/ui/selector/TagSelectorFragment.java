package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.graphics.Color;
import android.os.Bundle;
import android.text.TextUtils;
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
import com.chad.library.adapter4.BaseQuickAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.FragmentSelectorCollaboratorBinding;
import com.seafile.seadroid2.databinding.ItemTagSelectorBinding;
import com.seafile.seadroid2.databinding.ItemTextRoundOptionalBinding;
import com.seafile.seadroid2.databinding.LayoutDetailTagBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorWithDragBinding;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.framework.transport.TransportHolder;
import com.seafile.seadroid2.ui.base.fragment.BaseBottomSheetDialogFragment;
import com.seafile.seadroid2.ui.sdoc.SDocViewModel;
import com.seafile.seadroid2.view.LeftMarginDividerItemDecoration;

import java.util.List;
import java.util.stream.Collectors;

public class TagSelectorFragment extends BaseBottomSheetDialogFragment {
    private FragmentSelectorCollaboratorBinding binding;
    private ToolbarActionbarForSelectorWithDragBinding toolbarBinding;

    private TagSelectorAdapter adapter;
    private String columnKey;
    private List<OptionTagModel> optionsModels;
    private List<OptionTagModel> selectedOptionsModels;

    private SDocViewModel sDocViewModel;

    public static TagSelectorFragment newInstance(String columnKey, String title, boolean isSingleSelect, List<OptionTagModel> tags, List<OptionTagModel> selectedTags) {
        TransportHolder.get().put("columnKey", columnKey);
        TransportHolder.get().put("tags", tags);
        TransportHolder.get().put("selectedTags", selectedTags);
        TransportHolder.get().put("isSingleSelect", isSingleSelect);

        return new TagSelectorFragment();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        columnKey = TransportHolder.get().get("columnKey");
        optionsModels = TransportHolder.get().get("tags");
        selectedOptionsModels = TransportHolder.get().get("selectedTags");

        TransportHolder.get().remove("columnKey");
        TransportHolder.get().remove("tags");
        TransportHolder.get().remove("selectedTags");
        TransportHolder.get().remove("isSingleSelect");

        sDocViewModel = new ViewModelProvider(requireActivity()).get(SDocViewModel.class);

        if (TextUtils.isEmpty(columnKey) || CollectionUtils.isEmpty(optionsModels)) {
            throw new IllegalArgumentException("no columnKey/tags param");
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
        adapter = new TagSelectorAdapter();
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<OptionTagModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<OptionTagModel, ?> baseQuickAdapter, @NonNull View view, int i) {

                adapter.getItems().get(i).isSelected = !adapter.getItems().get(i).isSelected;
                baseQuickAdapter.notifyItemChanged(i);
            }
        });

        binding.rv.setAdapter(adapter);
    }

    private void loadData() {
        if (CollectionUtils.isEmpty(optionsModels)) {
            adapter.submitList(optionsModels);
            return;
        }

        if (!CollectionUtils.isEmpty(selectedOptionsModels)) {
            for (OptionTagModel optionsModel : optionsModels) {
                for (OptionTagModel selectedOptionsModel : selectedOptionsModels) {
                    if (optionsModel.id.equals(selectedOptionsModel.id)) {
                        optionsModel.isSelected = true;
                        break;
                    }
                }
            }
        }

        adapter.submitList(optionsModels);
    }

    private void onDone() {
        List<OptionTagModel> ls = adapter.getItems()
                .stream()
                .filter(f -> f.isSelected)
                .collect(Collectors.toList());

        sDocViewModel.getOnTagSelectedLiveData().postValue(new Pair<>(columnKey, ls));

        dismiss();
    }

    public static class TagSelectorAdapter extends BaseQuickAdapter<OptionTagModel, TagSelectorViewHolder> {

        @Override
        protected void onBindViewHolder(@NonNull TagSelectorViewHolder holder, int i, @Nullable OptionTagModel model) {
            if (model == null){
                return;
            }

            holder.binding.text.setText(model.name);
            holder.binding.indicator.setCardBackgroundColor(Color.parseColor(model.getColor()));

            holder.binding.userSelected.setVisibility(model.isSelected ? View.VISIBLE : View.GONE);
        }

        @NonNull
        @Override
        protected TagSelectorViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
            ItemTagSelectorBinding binding = ItemTagSelectorBinding.inflate(LayoutInflater.from(viewGroup.getContext()), viewGroup, false);
            return new TagSelectorViewHolder(binding);
        }
    }

    public static class TagSelectorViewHolder extends RecyclerView.ViewHolder {
        public ItemTagSelectorBinding binding;

        public TagSelectorViewHolder(@NonNull ItemTagSelectorBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
