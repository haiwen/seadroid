package com.seafile.seadroid2.ui.file_profile;


import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.SizeUtils;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.databinding.DialogFileProfileBinding;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataModel;
import com.seafile.seadroid2.framework.transport.TransportHolder;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;

public class FileProfileDialog extends BottomSheetDialogFragment {
    private DialogFileProfileBinding profileBinding;

    private FileProfileConfigModel configModel;
    private String repoId;

    public static FileProfileDialog newInstance(FileProfileConfigModel configModel) {
        TransportHolder.get().put("config_model", configModel);
        return new FileProfileDialog();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        configModel = TransportHolder.get().get("config_model");
        TransportHolder.get().remove("config_model");

        if (configModel == null) {
            throw new IllegalArgumentException("configModel is null");
        }
        repoId = configModel.getRepoId();
    }


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        profileBinding = DialogFileProfileBinding.inflate(inflater, container, false);
        return profileBinding.getRoot();
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

//        profileBinding.title.setVisibility(View.VISIBLE);
//        profileBinding.title.setText(configModel.getDetail().getName());
        setData(profileBinding.detailsContainer);
    }

    private void setData(LinearLayout parent) {
        if (configModel == null) {
            return;
        }

        if (configModel.isMetadataEnabled()) {
            profileBinding.edit.setVisibility(View.VISIBLE);
            profileBinding.edit.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    dismiss();
                    Intent intent = ProfileEditorActivity.getIntent(requireContext(), repoId, configModel);
                    startActivity(intent);
                }
            });
        }else {
            profileBinding.edit.setVisibility(View.GONE);
        }

        HashMap<String, Boolean> detailsSettingsMap = configModel.getDetailsSettingsMap();
        LinkedHashMap<String, MetadataModel> recordMetaDataMap = configModel.getRecordMetaDataMap();
        Set<String> keys = recordMetaDataMap.keySet();
        for (String key : keys) {
            MetadataModel metadata = recordMetaDataMap.get(key);
            if (metadata == null) {
                continue;
            }

            Boolean isShown = detailsSettingsMap.get(key);
            if (isShown == null || !isShown) {
                continue;
            }

            if (key.startsWith("_")) {
                if (MetadataViewUtils.getSupportedFieldMap().containsKey(key)) {
                    addMetadataView(parent, metadata);
                }
            } else {
                addMetadataView(parent, metadata);
            }
        }
    }

    private void addMetadataView(LinearLayout parent, MetadataModel metadata) {
        parseViewByType(getContext(), parent, metadata);
    }


    public void parseViewByType(Context context, LinearLayout parent, MetadataModel metadata) {
        final String type = metadata.type;
        LinearLayout kvView = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.layout_details_keyview_valuecontainer, null);

        int resStrId = ColumnTypeUtils.getResNameByKey(metadata.key);
        if (resStrId != 0) {
            kvView.<TextView>findViewById(R.id.text_title).setText(resStrId);
        } else {
            kvView.<TextView>findViewById(R.id.text_title).setText(metadata.name);
        }

        kvView.<ImageView>findViewById(R.id.text_icon).setImageResource(ColumnTypeUtils.getIconByColumnType(metadata.type, metadata.key));

        LinearLayout.LayoutParams ll = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        ll.topMargin = SizeUtils.dp2px(8);
        kvView.setLayoutParams(ll);

        if (!TextUtils.equals(ColumnType.RATE, type) && metadata.value == null) {

            MetadataViewUtils.addEmptyText(requireContext(), kvView);

            parent.addView(kvView);
            return;
        }

        if (TextUtils.equals(ColumnType.TEXT, type)) {
            MetadataViewUtils.parseText(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.LONG_TEXT, type)) {
            MetadataViewUtils.parseLongText(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.NUMBER, type)) {
            MetadataViewUtils.parseNumber(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.DATE, type)) {
            MetadataViewUtils.parseDate(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.DURATION, type)) {
            MetadataViewUtils.parseDuration(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.COLLABORATOR, type)) {
            MetadataViewUtils.parseCollaborator(requireContext(), kvView, configModel, metadata);
        } else if (TextUtils.equals(ColumnType.SINGLE_SELECT, type)) {
            MetadataViewUtils.parseSingleSelect(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.MULTIPLE_SELECT, type)) {
            MetadataViewUtils.parseMultiSelect(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.EMAIL, type)) {
            MetadataViewUtils.parseText(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.URL, type)) {
            MetadataViewUtils.parseText(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.RATE, type)) {
            MetadataViewUtils.parseRate(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.GEOLOCATION, type)) {
            MetadataViewUtils.parseGeoLocation(requireContext(), kvView, metadata, configModel);
        } else if (TextUtils.equals(ColumnType.CHECKBOX, type)) {
            MetadataViewUtils.parseCheckbox(requireContext(), kvView, metadata);
        } else if (TextUtils.equals(ColumnType.LINK, type)) {

            //tag
            if (TextUtils.equals("_tags", metadata.key)) {
                MetadataViewUtils.parseTag(requireContext(), kvView, configModel, metadata);
            } else {
            }
        }
//        else if (TextUtils.equals(ColumnType.IMAGE, type)) {
//            parseImage(kvView, model);
//        } else if (TextUtils.equals(ColumnType.FILE, type)) {
//            parseFile(kvView, model);
//        }  else if (TextUtils.equals(ColumnType.DIGITAL_SIGN, type)) {
//            parseDigitalSign(kvView, workFlowModel, model);
//        }

        parent.addView(kvView);
    }
}
