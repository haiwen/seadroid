package com.seafile.seadroid2.view;

import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataModel;
import com.seafile.seadroid2.ui.file_profile.ColumnTypeUtils;
import com.seafile.seadroid2.ui.file_profile.MetadataViewUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class DocProfileView extends LinearLayout {
    public DocProfileView(Context context) {
        super(context);
        init();
    }

    public DocProfileView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public DocProfileView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public DocProfileView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init();
    }

    private void init() {
        setOrientation(LinearLayout.VERTICAL);
    }

    private FileProfileConfigModel configModel;

    public void parseData(FileProfileConfigModel configModel) {
        this.configModel = configModel;

        if (configModel == null) {
            throw new IllegalArgumentException("configModel is null");
        }

        if (configModel.getDetail() == null) {
            throw new IllegalArgumentException("detail is null");
        }

        convert();

        addView();
    }

    private void convert() {
        List<MetadataModel> metadataList = new ArrayList<>(configModel.getRecordMetaDataList());
        for (MetadataModel metadata : metadataList) {
            if ("_file_modifier".equals(metadata.key)) {
                metadata.type = "collaborator";
                metadata.value = CollectionUtils.newArrayList(getValueByKey(metadata.name));
            } else {
                metadata.value = getValueByKey(metadata.name);
            }
        }
        configModel.setRecordMetaDataList(metadataList);
    }

    private void addView() {
        for (MetadataModel metadata : configModel.getRecordMetaDataList()) {
            if (metadata.key.startsWith("_")) {
                if (_supportedField.contains(metadata.key)) {
                    addMetadataView(metadata);
                }
            } else {
                addMetadataView(metadata);
            }
        }
    }

    private void addMetadataView(MetadataModel metadata) {
        parseViewByType(metadata);
    }

    private final List<String> _supportedField = List.of("_size", "_file_modifier", "_file_mtime", "_description", "_collaborators", "_owner", "_reviewer", "_status", "_tags", "_location", "_rate");

    private Object getValueByKey(String key) {
        if (configModel.getRecordResultList().isEmpty()) {
            return null;
        }

        Map<String, Object> model = configModel.getRecordResultList().get(0);
        return model.get(key);
    }


    public void parseViewByType(MetadataModel metadata) {
        final String type = metadata.type;
        LinearLayout view = (LinearLayout) LayoutInflater.from(getContext()).inflate(R.layout.layout_details_keyview_valuecontainer, null);

        int resStrId = ColumnTypeUtils.getResNameByKey(metadata.key);
        if (resStrId != 0) {
            view.<TextView>findViewById(R.id.text_title).setText(resStrId);
        } else {
            view.<TextView>findViewById(R.id.text_title).setText(metadata.name);
        }

        view.<ImageView>findViewById(R.id.text_icon).setImageResource(ColumnTypeUtils.getIconByColumnType(metadata.type, metadata.key));

        if (metadata.value == null) {

            MetadataViewUtils.addEmptyText(getContext(), view);

            addViewToThis(view);
            return;
        }

        if (TextUtils.equals(ColumnType.TEXT, type)) {
            MetadataViewUtils.parseText(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.LONG_TEXT, type)) {
            MetadataViewUtils.parseLongText(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.NUMBER, type)) {
            MetadataViewUtils.parseNumber(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.DATE, type)) {
            MetadataViewUtils.parseDate(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.DURATION, type)) {
            MetadataViewUtils.parseDuration(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.COLLABORATOR, type)) {
            MetadataViewUtils.parseCollaborator(getContext(), view, configModel, metadata);
        } else if (TextUtils.equals(ColumnType.SINGLE_SELECT, type)) {
            MetadataViewUtils.parseSingleSelect(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.MULTIPLE_SELECT, type)) {
            MetadataViewUtils.parseMultiSelect(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.EMAIL, type)) {
            MetadataViewUtils.parseText(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.URL, type)) {
            MetadataViewUtils.parseText(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.RATE, type)) {
            MetadataViewUtils.parseRate(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.GEOLOCATION, type)) {
            MetadataViewUtils.parseGeoLocation(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.LINK, type)) {

            //tag
            if (TextUtils.equals("_tags", metadata.key)) {
                MetadataViewUtils.parseTag(getContext(), view, configModel, metadata);
            } else {
                MetadataViewUtils.parseLink(getContext(), view, metadata);
            }
        } else if (TextUtils.equals(ColumnType.IMAGE, type)) {
            MetadataViewUtils.parseImage(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.FILE, type)) {
            MetadataViewUtils.parseFile(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.CHECKBOX, type)) {
            MetadataViewUtils.parseCheckbox(getContext(), view, metadata);
        } else if (TextUtils.equals(ColumnType.DIGITAL_SIGN, type)) {
            MetadataViewUtils.parseDigitalSign(getContext(), view, metadata);
        }

        addViewToThis(view);
    }

    private void addViewToThis(View view) {
        LinearLayout.LayoutParams ll = new LinearLayout.LayoutParams(-1, -2);
        ll.topMargin = DP_8;
        this.addView(view, ll);
    }


}
