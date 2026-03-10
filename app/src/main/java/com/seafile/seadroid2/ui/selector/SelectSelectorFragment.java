package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.graphics.Color;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.FragmentSelectorCollaboratorBinding;
import com.seafile.seadroid2.databinding.ItemTextRoundOptionalBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorBinding;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.ui.adapter.CustomLoadMoreAdapter;
import com.seafile.seadroid2.ui.base.fragment.BaseBottomSheetDialogFragment;
import com.seafile.seadroid2.view.LeftMarginDividerItemDecoration;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class SelectSelectorFragment extends BaseBottomSheetDialogFragment {
    private FragmentSelectorCollaboratorBinding binding;
    private ToolbarActionbarForSelectorBinding toolbarBinding;

    private SelectSelectorAdapter adapter;
    private QuickAdapterHelper helper;
    private String columnKey;
    private String title;
    private boolean isSingleSelect;
    private List<OptionTagModel> optionsModels;
    private List<OptionTagModel> selectedOptionsModels;

    public static SelectSelectorFragment newInstance(String columnKey, String title, boolean isSingleSelect, List<OptionTagModel> optionsModels, List<OptionTagModel> selectedOptionsModels) {

        Bundle args = new Bundle();

        args.putString("columnKey", columnKey);
        args.putString("title", title);
        args.putBoolean("isSingleSelect", isSingleSelect);

        if (!CollectionUtils.isEmpty(optionsModels)) {
            args.putParcelableArrayList("optionsModels", new ArrayList<>(optionsModels));
        }

        if (!CollectionUtils.isEmpty(selectedOptionsModels)) {
            args.putParcelableArrayList("selectedOptionsModels", new ArrayList<>(selectedOptionsModels));
        }
        SelectSelectorFragment fragment = new SelectSelectorFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (getArguments() != null) {
            columnKey = getArguments().getString("columnKey");
            title = getArguments().getString("title");
            isSingleSelect = getArguments().getBoolean("isSingleSelect");

            if (getArguments().containsKey("optionsModels")) {
                List<OptionTagModel> tempOptionsModels = getArguments().getParcelableArrayList("optionsModels");

                //clone
                TypeToken<ArrayList<OptionTagModel>> token = new TypeToken<ArrayList<OptionTagModel>>() {};
                optionsModels = CloneUtils.deepClone(tempOptionsModels, token.getType());
            }

            if (getArguments().containsKey("selectedOptionsModels")) {
                selectedOptionsModels = getArguments().getParcelableArrayList("selectedOptionsModels");
            }
        }

        if (TextUtils.isEmpty(columnKey)) {
            throw new IllegalArgumentException("no columnKey param");
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

        toolbarBinding.title.setText(title);

        if (isSingleSelect) {
            toolbarBinding.done.setVisibility(View.GONE);
            toolbarBinding.cancel.setVisibility(View.GONE);
        } else {
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
    }

    private void initAdapter() {
        adapter = new SelectSelectorAdapter(isSingleSelect);
//        adapter.setStateViewEnable(true);

        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<OptionTagModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<OptionTagModel, ?> baseQuickAdapter, @NonNull View view, int i) {

                if (isSingleSelect) {
                    for (OptionTagModel item : adapter.getItems()) {
                        item.isSelected = false;
                    }
                }

                adapter.getItem(i).isSelected = !adapter.getItem(i).isSelected;
                if (isSingleSelect) {
                    onDone();
                } else {
                    baseQuickAdapter.notifyItemChanged(i);
                }
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
        List<OptionTagModel> ls = adapter.getItems().stream().filter(f -> f.isSelected).map(new Function<OptionTagModel, OptionTagModel>() {
            @Override
            public OptionTagModel apply(OptionTagModel model) {
                model.isSelected = false;
                return model;
            }
        }).collect(Collectors.toList());


        dismiss();
    }

    public static class SelectSelectorAdapter extends BaseQuickAdapter<OptionTagModel, SelectSelectorViewHolder> {
        private int lastSelectedPosition = -1;
        private boolean isSingleSelect;

        public SelectSelectorAdapter(boolean isSingleSelect) {
            this.isSingleSelect = isSingleSelect;
        }

        @Override
        protected void onBindViewHolder(@NonNull SelectSelectorViewHolder holder, int i, @Nullable OptionTagModel model) {
            holder.binding.text.setText(model.name);
            holder.binding.text.setTextColor(Color.parseColor(model.textColor));
            holder.binding.cardView.setCardBackgroundColor(Color.parseColor(model.color));

            holder.binding.userSelected.setVisibility(model.isSelected ? View.VISIBLE : View.GONE);
        }

        @NonNull
        @Override
        protected SelectSelectorViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
            ItemTextRoundOptionalBinding binding = ItemTextRoundOptionalBinding.inflate(LayoutInflater.from(viewGroup.getContext()), viewGroup, false);
            return new SelectSelectorViewHolder(binding);
        }
    }

    public static class SelectSelectorViewHolder extends RecyclerView.ViewHolder {
        public ItemTextRoundOptionalBinding binding;

        public SelectSelectorViewHolder(@NonNull ItemTextRoundOptionalBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
