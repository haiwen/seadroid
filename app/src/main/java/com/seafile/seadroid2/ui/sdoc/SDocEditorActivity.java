package com.seafile.seadroid2.ui.sdoc;

import static com.seafile.seadroid2.config.Constants.DP.DP_16;
import static com.seafile.seadroid2.config.Constants.DP.DP_4;
import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.CompoundButton;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.flexbox.FlexboxLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.ActivitySdocEditorBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarBinding;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.GeoLocationModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataModel;
import com.seafile.seadroid2.framework.model.sdoc.OptionsTagModel;
import com.seafile.seadroid2.framework.transport.TransportHolder;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.listener.OnSingleSelectChangedListener;
import com.seafile.seadroid2.listener.OnTaskViewOptionsChangedListener;
import com.seafile.seadroid2.listener.OnTextChangedListener;
import com.seafile.seadroid2.listener.OnTextViewClickListener;
import com.seafile.seadroid2.listener.OnViewClickListener;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.file_profile.ColumnTypeUtils;
import com.seafile.seadroid2.ui.file_profile.MetadataViewUtils;
import com.seafile.seadroid2.view.ratingbar.OnRatingChangedListener;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class SDocEditorActivity extends BaseActivityWithVM<SDocViewModel> {
    private static final String TAG = "SDocEditorActivity";
    private ActivitySdocEditorBinding binding;
    private ToolbarActionbarBinding bindingOfToolbar;

    private String repoId, path;
    private FileProfileConfigModel configModel;
    private LinkedHashMap<String, Object> linkedHashMap = new LinkedHashMap<>();
    private Context context;

    // value: can editable?
    private final HashMap<String, Boolean> _supportedField = new HashMap<>();
    private final String TEXT_VIEW_TITLE_TAG_PREFIX = "Text:Title:";
    private final String CHILD_CONTAINER_TAG_PREFIX = "Container:";

//    public static Intent getIntent(Context context, String repoId, String path) {
//        Intent intent = new Intent(context, SDocEditorActivity.class);
//        intent.putExtra("repoId", repoId);
//        intent.putExtra("path", path);
//        return intent;
//    }

    public static Intent getIntent(Context context, FileProfileConfigModel configModel) {
        TransportHolder.get().put("config_model", configModel);

        Intent intent = new Intent(context, SDocEditorActivity.class);
        return intent;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySdocEditorBinding.inflate(getLayoutInflater());
        bindingOfToolbar = ToolbarActionbarBinding.bind(binding.toolbar.getRoot());

        getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE);

        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());

        context = this;

        initVM();
        initData();
        init();

        if (!StringUtils.isEmpty(repoId) && !StringUtils.isEmpty(path)) {
            loadFileDetail(repoId, path);
        } else if (configModel != null) {
            parseData();
        }
    }

    private void init() {
        if (getIntent() == null) {
            throw new IllegalArgumentException("Intent is null");
        }

        configModel = TransportHolder.get().get("config_model");
        TransportHolder.get().remove("config_model");

        if (configModel == null && StringUtils.isEmpty(repoId)) {
            throw new IllegalArgumentException("intent params is null");
        }

        binding.swipeRefreshLayout.setEnabled(false);

        Toolbar toolbar = bindingOfToolbar.toolbarActionbar;

        toolbar.setTitle("");
        setSupportActionBar(toolbar);
        toolbar.setTitle(R.string.edit);

        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

    }

    private void initData() {
        _supportedField.put("_size", false);
        _supportedField.put("_file_modifier", false);
        _supportedField.put("_file_mtime", false);
        _supportedField.put("_owner", true);
        _supportedField.put("_description", true);
        _supportedField.put("_collaborators", true);
        _supportedField.put("_reviewer", true);
        _supportedField.put("_status", true);
        _supportedField.put("_location", true);
        _supportedField.put("_tags", true);
        _supportedField.put("_rate", true);
    }

    private void initVM() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {

            }
        });

        getViewModel().getFileDetailLiveData().observe(this, new Observer<FileProfileConfigModel>() {
            @Override
            public void onChanged(FileProfileConfigModel fileProfileConfigModel) {
                configModel = fileProfileConfigModel;
                if (configModel == null) {
                    Toasts.show("null");
                } else {
                    parseData();
                }
            }
        });
    }

    private void loadFileDetail(String repoId, String path) {
        getViewModel().loadFileDetail(repoId, path);
    }

    private void parseData() {
        if (configModel == null) {
            return;
        }

        List<MetadataModel> metadataList = new ArrayList<>(configModel.getRecordMetaDataList());
        for (MetadataModel metadata : metadataList) {
            if ("_file_modifier".equals(metadata.key)) {
                metadata.type = "collaborator";
                metadata.value = CollectionUtils.newArrayList(getValueByKey(metadata.name));
            } else {
                metadata.value = getValueByKey(metadata.name);
            }

            linkedHashMap.put(metadata.name, metadata);
        }
        configModel.setRecordMetaDataList(metadataList);

        setData(binding.container);
    }

    private void setData(LinearLayout parent) {
        for (MetadataModel metadata : configModel.getRecordMetaDataList()) {
            if (metadata.key.startsWith("_")) {
                if (_supportedField.containsKey(metadata.key)) {
                    addMetadataView(parent, metadata);
                }
            } else {
                addMetadataView(parent, metadata);
            }
        }
    }


    private Object getValueByKey(String key) {
        Map<String, Object> model = configModel.getRecordResultList().get(0);
        return model.get(key);
    }

    private void addMetadataView(LinearLayout parent, MetadataModel metadata) {

        LinearLayout ll = getLinearLayout(metadata.key);

        // add title view first
        TextView textView = genTitleTextView(metadata.name, metadata.key);
        ll.addView(textView);

        parseViewByType(context, ll, metadata);

        parent.addView(ll);
    }

    private boolean isEditableByKey(String key) {
        boolean isEditable = true;
        if (_supportedField.containsKey(key)) {
            return Boolean.TRUE.equals(_supportedField.get(key));
        }
        return isEditable;
    }

    public void parseViewByType(Context context, LinearLayout parent, MetadataModel metadata) {
        String type = metadata.type;
        boolean isEditable = isEditableByKey(metadata.key);

        if (TextUtils.equals(ColumnType.TEXT, type)) {
            MetadataViewUtils.buildEditableText(context, isEditable, parent, metadata, new OnTextChangedListener() {
                @Override
                public void onChanged(String text) {
                    linkedHashMap.put(metadata.key, text);
                }
            });
        } else if (TextUtils.equals(ColumnType.LONG_TEXT, type)) {
            MetadataViewUtils.buildEditableLongText(context, isEditable, parent, metadata, new OnViewClickListener() {
                @Override
                public void onClick(View view, String tag) {
//                    linkedHashMap.put(metadata.key, tag);
                }
            });
        } else if (TextUtils.equals(ColumnType.NUMBER, type)) {
            MetadataViewUtils.buildEditableNumber(context, isEditable, parent, metadata, new OnTextChangedListener() {
                @Override
                public void onChanged(String text) {
//                    linkedHashMap.put(metadata.key, text);
                }
            });
        } else if (TextUtils.equals(ColumnType.DATE, type)) {
            MetadataViewUtils.buildEditableDate(context, isEditable, parent, metadata, new OnTextViewClickListener() {
                @Override
                public void onClick(TextView textView, String tag) {

                }
            });
        } else if (TextUtils.equals(ColumnType.COLLABORATOR, type)) {
            MetadataViewUtils.buildEditableCollaborator(context, isEditable, parent, metadata, configModel, new OnViewClickListener() {
                @Override
                public void onClick(View view, String tag) {

                }
            });
        } else if (TextUtils.equals(ColumnType.SINGLE_SELECT, type)) {
            MetadataViewUtils.buildEditableSingleSelect(context, isEditable, parent, metadata, new OnSingleSelectChangedListener() {
                @Override
                public void onChanged(OptionsTagModel optionsTagModel) {

                }
            });
        } else if (TextUtils.equals(ColumnType.MULTIPLE_SELECT, type)) {
            MetadataViewUtils.buildEditableMultiSelect(context, isEditable, parent, metadata, new OnTaskViewOptionsChangedListener() {
                @Override
                public void onChanged(List<OptionsTagModel> optionsModels) {

                }
            });
        } else if (TextUtils.equals(ColumnType.RATE, type)) {
            MetadataViewUtils.buildEditableRate(context, isEditable, parent, metadata, 0f, new OnRatingChangedListener() {
                @Override
                public void onRatingChanged(double rating) {

                }
            });
        } else if (TextUtils.equals(ColumnType.GEOLOCATION, type)) {
            GeoLocationModel locationModel = new GeoLocationModel();
            MetadataViewUtils.buildEditableGeoLocation(context, isEditable, parent, metadata, locationModel, new OnViewClickListener() {
                @Override
                public void onClick(View view, String tag) {

                }
            });
        } else if (TextUtils.equals(ColumnType.CHECKBOX, type)) {
            MetadataViewUtils.buildEditableCheckbox(context, isEditable, parent, metadata, true, new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {

                }
            });
        } else if (TextUtils.equals(ColumnType.LINK, type)) {
            //tag
            if (TextUtils.equals("_tags", metadata.key)) {
                MetadataViewUtils.buildEditableTag(context, isEditable, parent, metadata, configModel);
            }
        }
    }

    private LinearLayout getLinearLayout(String tag) {
        LinearLayout ll = new LinearLayout(context);
        ll.setOrientation(LinearLayout.VERTICAL);
        LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(-1, -2);
        llp.topMargin = DP_16;
        llp.bottomMargin = DP_8;
        ll.setLayoutParams(llp);
        ll.setPadding(DP_8, 0, DP_8, 0);
        ll.setTag(CHILD_CONTAINER_TAG_PREFIX + tag);
        return ll;
    }

    private TextView genTitleTextView(String name, String key) {
        TextView textView = new TextView(context);
        textView.setTextSize(16);
        textView.setTextColor(context.getColor(R.color.black));
        textView.setMaxLines(1);
        textView.setEllipsize(TextUtils.TruncateAt.END);
        int resStrId = ColumnTypeUtils.getResNameByKey(key);
        if (resStrId != 0) {
            textView.setText(resStrId);
        } else {
            textView.setText(name);
        }
        textView.setGravity(Gravity.CENTER_VERTICAL);
        LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(-1, -2);
        textView.setLayoutParams(llp);
        textView.setPadding(0, Constants.DP.DP_2, Constants.DP.DP_2, Constants.DP.DP_2);
        textView.setTag(TEXT_VIEW_TITLE_TAG_PREFIX + key);
        return textView;
    }
}
