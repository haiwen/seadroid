package com.seafile.seadroid2.ui.file_profile;


import static com.seafile.seadroid2.config.Constants.DP.DP_4;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.Context;
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

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.databinding.DialogFileProfileBinding;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class FileProfileDialog extends BottomSheetDialogFragment {
    private FileProfileConfigModel configModel;

    public static FileProfileDialog newInstance(FileProfileConfigModel configModel) {
        Bundle args = new Bundle();
        args.putParcelable("config_model", configModel);
        FileProfileDialog fragment = new FileProfileDialog();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (getArguments() == null || !getArguments().containsKey("config_model")) {
            throw new IllegalArgumentException("configModel is null");
        }

        configModel = getArguments().getParcelable("config_model");
        if (configModel == null) {
            throw new IllegalArgumentException("configModel is null");
        }
    }

    private DialogFileProfileBinding profileBinding;

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
        List<MetadataModel> metadataList = new ArrayList<>(configModel.getRecordMetaDataList());
        for (MetadataModel metadata : metadataList) {
            if ("_file_modifier".equals(metadata.key)) {
                metadata.type = "collaborator";
                metadata.value = CollectionUtils.newArrayList(getValueByKey(metadata.name));
            } else {
                Object v = getValueByKey(metadata.name);
                metadata.value = v;
            }
        }
        configModel.setRecordMetaDataList(metadataList);

        for (MetadataModel metadata : configModel.getRecordMetaDataList()) {
            if (metadata.key.startsWith("_")) {
                if (_supportedField.contains(metadata.key)) {
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

    //not support: _tags
    private final List<String> _supportedField = List.of("_size", "_file_modifier", "_file_mtime", "_owner", "_description", "_collaborators", "_reviewer", "_status", "_location", "_tags", "_rate");

    private Object getValueByKey(String key) {
        Map<String, Object> model = configModel.getRecordResultList().get(0);
        return model.get(key);
    }

    private FlexboxLayout.LayoutParams getFlexParams() {
        FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        flexLayoutParams.bottomMargin = DP_4;
        flexLayoutParams.rightMargin = DP_4;
        return flexLayoutParams;
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
            MetadataViewUtils.parseGeoLocation(requireContext(), kvView, metadata);
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
