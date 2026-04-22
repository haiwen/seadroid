package com.seafile.seadroid2.ui.activities.others_dialog;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.DialogSdocDirectoryBinding;
import com.seafile.seadroid2.framework.model.activities.ActivityDetailModel;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.transport.TransportHolder;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.listener.OnItemClickListener2;

import java.util.List;

public class ActivityOtherListDialog extends BottomSheetDialogFragment {
    private final static String PARAMS_KEY = "activity_other_data_list";
    private DialogSdocDirectoryBinding binding;
    private ActivityOtherListAdapter adapter;
    private OnItemClickListener2<ActivityModel, ActivityDetailModel> onItemClickListener;
    private ActivityModel activityModel;

    public void setOnItemClickListener(OnItemClickListener2<ActivityModel, ActivityDetailModel> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    public static ActivityOtherListDialog newInstance(ActivityModel activityModel) {
        Bundle args = new Bundle();
        ActivityOtherListDialog fragment = new ActivityOtherListDialog();
        fragment.setArguments(args);

        TransportHolder.get().put(PARAMS_KEY, activityModel);

        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);


        activityModel = TransportHolder.get().get(PARAMS_KEY);
        TransportHolder.get().remove(PARAMS_KEY);

        if (activityModel == null) {
            throw new IllegalArgumentException("ActivityModel param must not be null");
        }
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

        adapter = new ActivityOtherListAdapter();
        adapter.setActivityModel(activityModel);

        adapter.setAnimationEnable(true);
        adapter.setStateViewLayout(requireContext(), R.layout.layout_empty);
        adapter.setStateViewEnable(false);
        adapter.addOnItemChildClickListener(R.id.text_container, new BaseQuickAdapter.OnItemChildClickListener<ActivityDetailModel>() {
            @Override
            public void onItemClick(@NonNull BaseQuickAdapter<ActivityDetailModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                if (onItemClickListener != null) {
                    onItemClickListener.onItemClick(activityModel, adapter.getItems().get(i));
                }
                dismiss();
            }
        });

        binding.rv.setAdapter(adapter);

        load();
    }

    private void load() {
        if (CollectionUtils.isEmpty(activityModel.details)) {
            adapter.setStateViewEnable(true);
        } else {
            adapter.submitList(activityModel.details);
        }
    }
}
