package com.seafile.seadroid2.ui.sdoc.outline;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.DialogSdocDirectoryBinding;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.listener.OnItemClickListener;

import java.lang.reflect.Type;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class SDocOutlineDialog extends BottomSheetDialogFragment {

    private DialogSdocDirectoryBinding binding;
    private SDocOutlineAdapter adapter;
    private List<OutlineItemModel> outlineItemList;
    private OnItemClickListener<OutlineItemModel> onItemClickListener;

    public void setOnItemClickListener(OnItemClickListener<OutlineItemModel> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    public static SDocOutlineDialog newInstance(String outlineStr) {
        Bundle args = new Bundle();
        args.putString("outline_value", outlineStr);
        SDocOutlineDialog fragment = new SDocOutlineDialog();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() == null || !getArguments().containsKey("outline_value")) {
            throw new IllegalArgumentException("outline_value is null");
        }

        String value = getArguments().getString("outline_value");

        Type listType = new TypeToken<List<OutlineItemModel>>() {
        }.getType();

        outlineItemList = GsonUtils.fromJson(value, listType);
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
        adapter.setStateViewLayout(requireContext(), R.layout.layout_empty);
        adapter.setStateViewEnable(false);
        adapter.addOnItemChildClickListener(R.id.text_container, new BaseQuickAdapter.OnItemChildClickListener<OutlineItemModel>() {
            @Override
            public void onItemClick(@NonNull BaseQuickAdapter<OutlineItemModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                OutlineItemModel outlineItemModel = adapter.getItems().get(i);
                if (onItemClickListener != null)
                    onItemClickListener.onItemClick(outlineItemModel, i);

                dismiss();
            }
        });


        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());

        load();
    }

    public static final List<String> _AllowedElementTypes = List.of("header1", "header2", "header3");

    private void load() {
        if (CollectionUtils.isEmpty(outlineItemList)) {
            adapter.setStateViewEnable(true);
            return;
        }

        List<OutlineItemModel> newList = outlineItemList.stream().filter(new Predicate<OutlineItemModel>() {
            @Override
            public boolean test(OutlineItemModel sDocModel) {
                if (!_AllowedElementTypes.contains(sDocModel.type)) {
                    return false;
                }

                if (TextUtils.isEmpty(sDocModel.text) && CollectionUtils.isEmpty(sDocModel.children)) {
                    return false;
                }

                return true;
            }
        }).map(new Function<OutlineItemModel, OutlineItemModel>() {
            @Override
            public OutlineItemModel apply(OutlineItemModel sDocModel) {
                if (!TextUtils.isEmpty(sDocModel.text)) {
                    return sDocModel;
                }

                if (CollectionUtils.isEmpty(sDocModel.children)) {
                    return sDocModel;
                }

                String text = "";
                for (OutlineItemModel child : sDocModel.children) {
                    if (!TextUtils.isEmpty(child.text)) {
                        String nt = StringUtils.trim(child.text, "\n").trim();
                        text = text.concat(nt);
                    }
                }
                sDocModel.text = text;
                return sDocModel;
            }
        }).collect(Collectors.toList());

        adapter.setStateViewEnable(true);
        adapter.submitList(newList);
    }
}
