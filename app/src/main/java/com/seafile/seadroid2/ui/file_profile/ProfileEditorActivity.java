package com.seafile.seadroid2.ui.file_profile;

import static com.seafile.seadroid2.config.Constants.DP.DP_16;
import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.SpannableString;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.util.Pair;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.WindowManager;
import android.widget.CompoundButton;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.ActivitySdocEditorBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarBinding;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataModel;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.transport.TransportHolder;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.listener.OnSingleOptionChangedListener;
import com.seafile.seadroid2.listener.OnMultiOptionsChangedListener;
import com.seafile.seadroid2.listener.OnTextChangedListener;
import com.seafile.seadroid2.listener.OnTextViewClickListener;
import com.seafile.seadroid2.listener.OnViewClickListener;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.sdoc.SDocViewModel;
import com.seafile.seadroid2.ui.selector.CollaboratorSelectorFragment;
import com.seafile.seadroid2.ui.selector.DateSelectorActivity;
import com.seafile.seadroid2.ui.selector.LongTextSelectorActivity;
import com.seafile.seadroid2.ui.selector.SelectSelectorFragment;
import com.seafile.seadroid2.view.ratingbar.OnRatingChangedListener;

import org.apache.commons.lang3.StringUtils;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

public class ProfileEditorActivity extends BaseActivityWithVM<SDocViewModel> {
    private static final String TAG = "ProfileEditorActivity";
    private ActivitySdocEditorBinding binding;
    private ToolbarActionbarBinding bindingOfToolbar;

    private String repoId, path;
    private FileProfileConfigModel configModel;
    private Context context;
    // Content modified by the user
    private final LinkedHashMap<String, Object> contentMap = new LinkedHashMap<>();

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

        Intent intent = new Intent(context, ProfileEditorActivity.class);
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

        registerResultLauncher();

        init();

        if (!StringUtils.isEmpty(repoId) && !StringUtils.isEmpty(path)) {
            loadFileDetail(repoId, path);
        } else if (configModel != null) {
            checkData();
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

    private void initVM() {
        getViewModel().getSecondRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoadingDialog(aBoolean);
            }
        });
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
                    checkData();
                }
            }
        });

        getViewModel().getOnUserSelectedLiveData().observe(this, new Observer<Pair<String, List<UserModel>>>() {
            @Override
            public void onChanged(Pair<String, List<UserModel>> pair) {

                String key = pair.first;
                List<String> value = pair.second.stream().map(new Function<UserModel, String>() {
                    @Override
                    public String apply(UserModel userModel) {
                        return userModel.getEmail();
                    }
                }).collect(Collectors.toList());
                contentMap.put(key, value);

                updateConfigMapMetadata(key, value);
            }
        });

        getViewModel().getOnTagSelectedLiveData().observe(this, new Observer<Pair<String, List<OptionTagModel>>>() {
            @Override
            public void onChanged(Pair<String, List<OptionTagModel>> pair) {
                String key = pair.first;
                List<OptionTagModel> value = pair.second;
                contentMap.put(key, value);

                updateConfigMapMetadata(key, value);
            }
        });
    }

    private MenuItem editMenuItem;

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_sdoc_preview, menu);
        editMenuItem = menu.findItem(R.id.sdoc_edit);
        editMenuItem.setVisible(true);

        String title = editMenuItem.getTitle().toString();
        setToolMenuTitle(title);
        return true;
    }

    private void setToolMenuTitle(String title) {
        SpannableString spanString = new SpannableString(title);
        int blue = getColor(R.color.blue_900);
        spanString.setSpan(new ForegroundColorSpan(blue), 0, spanString.length(), 0);
        editMenuItem.setTitle(spanString);
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == R.id.sdoc_edit) {
            save();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void save() {
        Object object = configModel.getRecordResultMap().get("_id");
        if (object == null) {
            return;
        }
        String recordId = object.toString();
//        getViewModel().postRecord(repoId, recordId, contentMap);
    }

    private void loadFileDetail(String repoId, String path) {
        getViewModel().loadFileDetail(repoId, path);
    }

    private void checkData() {
        if (configModel == null) {
            return;
        }

        if (configModel.getRecordMetaDataMap().isEmpty()) {
            // todo 空布局
            return;
        }

        setData(binding.container);
    }

    private void setData(LinearLayout rootContainer) {
        LinkedHashMap<String, MetadataModel> recordMetaDataMap = configModel.getRecordMetaDataMap();
        Set<String> keys = recordMetaDataMap.keySet();
        for (String key : keys) {
            MetadataModel metadata = recordMetaDataMap.get(key);
            if (metadata == null) {
                continue;
            }

            if (key.startsWith("_")) {
                if (MetadataViewUtils.getSupportedFieldMap().containsKey(key)) {
                    addMetadataView(rootContainer, metadata);
                }
            } else {
                addMetadataView(rootContainer, metadata);
            }

            //
//            contentMap.put(key, metadata.value);
        }
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
        if (MetadataViewUtils.getSupportedFieldMap().containsKey(key)) {
            return Boolean.TRUE.equals(MetadataViewUtils.getSupportedFieldMap().get(key));
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
                    contentMap.put(metadata.key, text);
                }
            });
        } else if (TextUtils.equals(ColumnType.LONG_TEXT, type)) {
            MetadataViewUtils.buildEditableLongText(context, isEditable, parent, metadata, new OnViewClickListener() {
                @Override
                public void onClick(View view, String tag) {
                    String inputValue = null;
                    if (metadata.value instanceof String) {
                        inputValue = (String) metadata.value;
                    }
                    Intent intent = LongTextSelectorActivity.getIntent(context, metadata.key, inputValue, metadata.name);
                    pickLongTextLauncher.launch(intent);
                }
            });
        } else if (TextUtils.equals(ColumnType.NUMBER, type)) {
            MetadataViewUtils.buildEditableNumber(context, isEditable, parent, metadata, new OnTextChangedListener() {
                @Override
                public void onChanged(String text) {
                    contentMap.put(metadata.key, text);
                }
            });
        } else if (TextUtils.equals(ColumnType.DATE, type)) {
            MetadataViewUtils.buildEditableDate(context, isEditable, parent, metadata, new OnTextViewClickListener() {
                @Override
                public void onClick(TextView textView, String tag) {
                    String format = null;
                    if (CollectionUtils.isNotEmpty(metadata.config)) {
                        format = metadata.config.get(0).format;
                    }
                    Intent intent = DateSelectorActivity.getIntent(context, metadata.key, format, metadata.name);
                    pickDateLauncher.launch(intent);
                }
            });
        } else if (TextUtils.equals(ColumnType.COLLABORATOR, type)) {
            MetadataViewUtils.buildEditableCollaborator(context, isEditable, parent, metadata, configModel, new OnViewClickListener() {
                @Override
                public void onClick(View view, String tag) {
                    List<UserModel> selectedUserList = MetadataViewUtils.getUserList(metadata, configModel);
                    CollaboratorSelectorFragment sheetFragment = CollaboratorSelectorFragment.newInstance(metadata.key, configModel.getRelatedUserList(), selectedUserList);
                    sheetFragment.show(getSupportFragmentManager(), CollaboratorSelectorFragment.class.getSimpleName());
                }
            });
        } else if (TextUtils.equals(ColumnType.SINGLE_SELECT, type)) {
            MetadataViewUtils.buildEditableSingleSelect(context, isEditable, parent, metadata, new OnSingleOptionChangedListener() {
                @Override
                public void onChanged(OptionTagModel optionTagModel) {
                    contentMap.put(metadata.key, optionTagModel);
                }
            });
        } else if (TextUtils.equals(ColumnType.MULTIPLE_SELECT, type)) {
            MetadataViewUtils.buildEditableMultiSelect(context, isEditable, parent, metadata, new OnMultiOptionsChangedListener() {
                @Override
                public void onChanged(List<OptionTagModel> optionsModels) {
                    contentMap.put(metadata.key, optionsModels);
                }
            });
        } else if (TextUtils.equals(ColumnType.RATE, type)) {
            MetadataViewUtils.buildEditableRate(context, isEditable, parent, metadata, 0f, new OnRatingChangedListener() {
                @Override
                public void onRatingChanged(double rating) {
                    contentMap.put(metadata.key, rating);
                }
            });
        } else if (TextUtils.equals(ColumnType.GEOLOCATION, type)) {
            MetadataViewUtils.buildEditableGeoLocation(context, isEditable, parent, metadata, configModel, new OnViewClickListener() {
                @Override
                public void onClick(View view, String tag) {
                    // can not edit
                }
            });
        } else if (TextUtils.equals(ColumnType.CHECKBOX, type)) {
            MetadataViewUtils.buildEditableCheckbox(context, isEditable, parent, metadata, true, new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                    contentMap.put(metadata.key, isChecked);
                }
            });
        } else if (TextUtils.equals(ColumnType.LINK, type)) {
            //tag
            if (TextUtils.equals("_tags", metadata.key)) {

                MetadataViewUtils.buildEditableTag(context, isEditable, parent, metadata, configModel, new OnViewClickListener() {
                    @Override
                    public void onClick(View view, String tag) {

                        List<OptionTagModel> tagModels = MetadataViewUtils.convertLinkedTagToTagList(metadata, configModel);

                        String title = null;
                        int titleRes = ColumnTypeUtils.getResNameByKey(metadata.name);
                        if (titleRes != 0) {
                            title = getString(titleRes);
                        }

                        SelectSelectorFragment sheetFragment = SelectSelectorFragment.newInstance(
                                metadata.key,
                                title,
                                false,
                                configModel.getTagList(),
                                tagModels);
                        sheetFragment.show(getSupportFragmentManager(), SelectSelectorFragment.class.getSimpleName());
                    }
                });
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


    private ActivityResultLauncher<Intent> pickDateLauncher;
    private ActivityResultLauncher<Intent> pickLongTextLauncher;

    private void registerResultLauncher() {

        //
        pickLongTextLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
            @Override
            public void onActivityResult(ActivityResult result) {
                if (result == null || result.getData() == null) {
                    return;
                }

                String key = result.getData().getStringExtra("columnKey");
                String value = result.getData().getStringExtra("longtext");
                contentMap.put(key, value);

                updateConfigMapMetadata(key, value);
            }
        });


        //
        pickDateLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
            @Override
            public void onActivityResult(ActivityResult result) {
                if (result == null || result.getData() == null) {
                    return;
                }

                String key = result.getData().getStringExtra("columnKey");
                String value = result.getData().getStringExtra("date");
                contentMap.put(key, value);

                updateConfigMapMetadata(key, value);
            }
        });

    }

    private void updateConfigMapMetadata(String key, Object value) {
        MetadataModel metadataModel = configModel.getRecordMetaDataMap().get(key);
        if (metadataModel == null) {
            return;
        }

        metadataModel.value = value;
        configModel.getRecordMetaDataMap().put(key, metadataModel);

        LinearLayout ll = binding.container.findViewWithTag(CHILD_CONTAINER_TAG_PREFIX + key);
        parseViewByType(context, ll, metadataModel);
    }
}
