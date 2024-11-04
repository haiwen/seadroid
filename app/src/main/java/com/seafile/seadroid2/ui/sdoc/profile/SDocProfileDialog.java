package com.seafile.seadroid2.ui.sdoc.profile;


import static com.seafile.seadroid2.config.Constants.DP.DP_4;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.Resources;
import android.graphics.Color;
import android.os.Build;
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
import androidx.core.content.ContextCompat;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.Glide;
import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.google.android.material.card.MaterialCardView;
import com.google.android.material.imageview.ShapeableImageView;
import com.google.android.material.internal.ViewUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.DialogSdocProfileBinding;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigDataModel;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataModel;
import com.seafile.seadroid2.framework.data.model.sdoc.OptionsTagModel;
import com.seafile.seadroid2.framework.data.model.sdoc.RecordResultModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocDetailModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocRecordWrapperModel;
import com.seafile.seadroid2.framework.data.model.user.UserModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;

import java.lang.reflect.Field;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;


public class SDocProfileDialog extends BottomSheetDialogFragment {

    private SDocDetailModel docDetailModel;
    private SDocRecordWrapperModel recordWrapperModel;
    private ArrayList<UserModel> relatedUsers;

    public static SDocProfileDialog newInstance(SDocDetailModel docDetailModel, SDocRecordWrapperModel recordWrapperModel, List<UserModel> relatedUsers) {
        Bundle args = new Bundle();
        args.putParcelable("detailModel", docDetailModel);

        if (!CollectionUtils.isEmpty(relatedUsers)) {
            args.putParcelable("recordModel", recordWrapperModel);
        }

        if (!CollectionUtils.isEmpty(relatedUsers)) {
            args.putParcelableArrayList("relatedUsers", new ArrayList<>(relatedUsers));

        }
        SDocProfileDialog fragment = new SDocProfileDialog();
        fragment.setArguments(args);
        return fragment;
    }

    public static SDocProfileDialog newInstance(SDocDetailModel docDetailModel, List<UserModel> relatedUsers) {
        return newInstance(docDetailModel, null, relatedUsers);
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (getArguments() == null || !getArguments().containsKey("detailModel")) {
            throw new IllegalArgumentException("detailModel is null");
        }

        docDetailModel = getArguments().getParcelable("detailModel");
        recordWrapperModel = getArguments().getParcelable("recordModel");
        relatedUsers = getArguments().getParcelableArrayList("relatedUsers");

        if (docDetailModel == null) {
            throw new IllegalArgumentException("detail is null");
        }

        initFixedValueIfMetadataNotEnable();
    }

    DialogSdocProfileBinding profileBinding;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        profileBinding = DialogSdocProfileBinding.inflate(inflater, container, false);
        return profileBinding.getRoot();
    }

    @SuppressLint("RestrictedApi")
    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        BottomSheetDialog bottomSheetDialog = new BottomSheetDialog(requireContext());
//        View bottomSheetInternal = bottomSheetDialog.findViewById(R.id.design_bottom_sheet);
//        BottomSheetBehavior.from(bottomSheetInternal).setPeekHeight(800);

//        View bottomSheetContent = bottomSheetInternal.findViewById(R.id.bottom_drawer_2);
//        ViewUtils.doOnApplyWindowInsets(bottomSheetContent, new ViewUtils.OnApplyWindowInsetsListener() {
//            @Override
//            public WindowInsetsCompat onApplyWindowInsets(View view, WindowInsetsCompat insets, ViewUtils.RelativePadding initialPadding) {
//                // Add the inset in the inner NestedScrollView instead to make the edge-to-edge behavior
//                // consistent - i.e., the extra padding will only show at the bottom of all content, i.e.,
//                // only when you can no longer scroll down to show more content.
//                ViewCompat.setPaddingRelative(bottomSheetContent,
//                        initialPadding.start,
//                        initialPadding.top,
//                        initialPadding.end,
//                        initialPadding.bottom + insets.getInsets(WindowInsetsCompat.Type.systemBars()).bottom);
//                return insets;
//            }
//        });

        return bottomSheetDialog;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        setData(profileBinding.detailsContainer);
    }

    private void initFixedValueIfMetadataNotEnable() {
        if (recordWrapperModel != null) {
            return;
        }
        recordWrapperModel = new SDocRecordWrapperModel();

        RecordResultModel sizeModel = new RecordResultModel();
        sizeModel._size = docDetailModel.getSize();
        sizeModel._file_modifier = docDetailModel.getLastModifierEmail();
        sizeModel._file_mtime = docDetailModel.getLastModified();
        recordWrapperModel.results = new ArrayList<>();
        recordWrapperModel.results.add(sizeModel);

        recordWrapperModel.metadata = new ArrayList<>();

        MetadataModel sizeMetadataModel = new MetadataModel();
        sizeMetadataModel.key = "_size";
        sizeMetadataModel.name = "_size";
        sizeMetadataModel.type = ColumnType.NUMBER;
        recordWrapperModel.metadata.add(sizeMetadataModel);

        MetadataModel modifierMetadataModel = new MetadataModel();
        modifierMetadataModel.key = "_file_modifier";
        modifierMetadataModel.name = "_file_modifier";
        modifierMetadataModel.type = ColumnType.TEXT;
        recordWrapperModel.metadata.add(modifierMetadataModel);

        MetadataModel mTimeMetadataModel = new MetadataModel();
        mTimeMetadataModel.key = "_file_mtime";
        mTimeMetadataModel.name = "_file_mtime";
        mTimeMetadataModel.type = ColumnType.DATE;
        recordWrapperModel.metadata.add(mTimeMetadataModel);

    }

    private void setData(LinearLayout parent) {
        for (MetadataModel metadata : recordWrapperModel.metadata) {
            if ("_file_modifier".equals(metadata.key)) {
                metadata.type = "collaborator";
                metadata.value = CollectionUtils.newArrayList(getValueByKey(metadata.name));
            } else {
                metadata.value = getValueByKey(metadata.name);
            }
        }

        for (MetadataModel metadata : recordWrapperModel.metadata) {
            if (metadata.key.startsWith("_")) {
                if (_fixedField.contains(metadata.key)) {
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

    private final List<String> _fixedField = List.of("_size", "_file_modifier", "_file_mtime", "_description", "_collaborators", "_reviewer", "_status");

    private Object getValueByKey(String key) {
        RecordResultModel model = recordWrapperModel.results.get(0);
        try {
            Field field = RecordResultModel.class.getDeclaredField(key);
            field.setAccessible(true);
            return field.get(model);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            SLogs.e(e);
            return null;
        }
    }

    private FlexboxLayout.LayoutParams getFlexParams() {
        FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        flexLayoutParams.bottomMargin = DP_4;
        flexLayoutParams.rightMargin = DP_4;
        return flexLayoutParams;
    }

    private int getResNameByKey(String key) {
        switch (key) {
            case "_description":
                return R.string.description;
            case "_file_modifier":
                return R.string._last_modifier;
            case "_file_mtime":
                return R.string._last_modified_time;
            case "_status":
                return R.string._file_status;
            case "_collaborators":
                return R.string._file_collaborators;
            case "_size":
                return R.string._size;
            case "_reviewer":
                return R.string._reviewer;
            case "_in_progress":
                return R.string._in_progress;
            case "_in_review":
                return R.string._in_review;
            case "_done":
                return R.string._done;
            case "_outdated":
                return R.string._outdated;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            return Resources.ID_NULL;
        }
        return 0;
    }

    public void parseViewByType(Context context, LinearLayout parent, MetadataModel metadata) {
        final String type = metadata.type;
        LinearLayout view = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.layout_details_keyview_valuecontainer, null);

        int resStrId = getResNameByKey(metadata.key);
        if (resStrId != 0) {
            view.<TextView>findViewById(R.id.text_title).setText(resStrId);
        } else {
            view.<TextView>findViewById(R.id.text_title).setText(metadata.name);
        }

        view.<ImageView>findViewById(R.id.text_icon).setImageResource(getIconByColumnType(metadata.type));

        LinearLayout.LayoutParams ll = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        ll.topMargin = SizeUtils.dp2px(8);
        view.setLayoutParams(ll);
        parent.addView(view);

        if (!TextUtils.equals(ColumnType.RATE, type) && metadata.value == null) {

            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(R.string.empty);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
            return;
        }

        if (TextUtils.equals(ColumnType.TEXT, type)) {
            parseText(view, metadata);
        } else if (TextUtils.equals(ColumnType.LONG_TEXT, type)) {
            parseLongText(view, metadata);
        } else if (TextUtils.equals(ColumnType.NUMBER, type)) {
            parseNumber(view, metadata);
        } else if (TextUtils.equals(ColumnType.DATE, type)) {
            parseDate(view, metadata);
        } else if (TextUtils.equals(ColumnType.DURATION, type)) {
            parseDuration(view, metadata);
        } else if (TextUtils.equals(ColumnType.COLLABORATOR, type)) {
            parseCollaborator(view, metadata);
        } else if (TextUtils.equals(ColumnType.SINGLE_SELECT, type)) {
            parseSingleSelect(view, metadata);
        } else if (TextUtils.equals(ColumnType.MULTIPLE_SELECT, type)) {
            parseMultiSelect(view, metadata);
        } else if (TextUtils.equals(ColumnType.EMAIL, type)) {
            parseText(view, metadata);
        } else if (TextUtils.equals(ColumnType.URL, type)) {
            parseText(view, metadata);
        } else if (TextUtils.equals(ColumnType.RATE, type)) {
            parseRate(view, metadata);
        } else if (TextUtils.equals(ColumnType.GEOLOCATION, type)) {
//            parseGeoLocation(view, model);
        }
//        else if (TextUtils.equals(ColumnType.IMAGE, type)) {
//            parseImage(view, model);
//        } else if (TextUtils.equals(ColumnType.FILE, type)) {
//            parseFile(view, model);
//        } else if (TextUtils.equals(ColumnType.CHECKBOX, type)) {
//            parseCheckbox(view, model);
//        }else if (TextUtils.equals(ColumnType.LINK, type)) {
//            parseLink(view, model);
//        } else if (TextUtils.equals(ColumnType.DIGITAL_SIGN, type)) {
//            parseDigitalSign(view, workFlowModel, model);
//        }
    }

    private int getIconByColumnType(String type) {
        switch (type) {
            case ColumnType.TEXT:
                return R.drawable.ic_single_line_text;
            case ColumnType.COLLABORATOR:
                return R.drawable.ic_user_collaborator;
            case ColumnType.IMAGE:
                return R.drawable.ic_picture;
            case ColumnType.FILE:
                return R.drawable.ic_file_alt_solid;
            case ColumnType.DATE:
                return R.drawable.ic_calendar_alt_solid;
            case ColumnType.SINGLE_SELECT:
                return R.drawable.ic_single_election;
            case ColumnType.DURATION:
                return R.drawable.ic_duration;
            case ColumnType.MULTIPLE_SELECT:
                return R.drawable.ic_multiple_selection;
            case ColumnType.CHECKBOX:
                return R.drawable.ic_check_square_solid;
            case ColumnType.GEOLOCATION:
                return R.drawable.ic_location;
            case ColumnType.EMAIL:
                return R.drawable.ic_email;
            case ColumnType.LONG_TEXT:
                return R.drawable.ic_long_text;
            case ColumnType.NUMBER:
                return R.drawable.ic_number;
            case ColumnType.RATE:
                return R.drawable.ic_star_32;
            case ColumnType.URL:
                return R.drawable.ic_url;
            case ColumnType.LINK:
                return R.drawable.ic_links;
        }

        return R.drawable.ic_single_line_text;
    }

    private void parseText(LinearLayout view, MetadataModel model) {
        if (model.value instanceof String) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(model.value.toString());

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    private void parseLongText(LinearLayout view, MetadataModel model) {
        if (model.value instanceof String) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(model.value.toString());

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }


    private void parseDuration(LinearLayout view, MetadataModel model) {
        if (model.value instanceof String) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(model.value.toString());

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    private void parseNumber(LinearLayout view, MetadataModel model) {
        if (model.value instanceof Number number) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);

            ltr.<TextView>findViewById(R.id.text_view).setText(Utils.readableFileSize(number.intValue()));

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    private void parseDate(LinearLayout view, MetadataModel model) {
        if (model.value instanceof OffsetDateTime date) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);

            String temp = date.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace("T", " ");
            ltr.<TextView>findViewById(R.id.text_view).setText(temp);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

//    private void parseGeoLocation(LinearLayout view, MetadataModel model) {
//        if (model.value instanceof LinkedTreeMap) {
//            LinkedTreeMap<String, Object> treeMap = (LinkedTreeMap<String, Object>) model.value;
//            ColumnDataModel columnDataModel = model.column_data;
//
//            String geo_format = columnDataModel.geo_format;
//
//            String content = null;
//            if (TextUtils.equals("lng_lat", geo_format)) {
//                String lat = treeMap.get("lat").toString();
//                String lng = treeMap.get("lng").toString();
//                content = lat + "," + lng;
//            } else if (TextUtils.equals("geolocation", geo_format)) {
//                String province = treeMap.get("province").toString();
//                String city = treeMap.get("city").toString();
//                String dis = treeMap.get("district").toString();
//                String detail = treeMap.get("detail").toString();
//                content = province + city + dis + detail;
//            } else if (TextUtils.equals("country_region", geo_format)) {
//                content = treeMap.get("country_region").toString();
//            } else if (TextUtils.equals("province", geo_format)) {
//                content = treeMap.get("province").toString();
//            } else if (TextUtils.equals("province_city", geo_format)) {
//                String province = treeMap.get("province").toString();
//                String city = treeMap.get("city").toString();
//                content = province + city;
//            } else if (TextUtils.equals("province_city_district", geo_format)) {
//                String province = treeMap.get("province").toString();
//                String city = treeMap.get("city").toString();
//                String dis = treeMap.get("district").toString();
//                content = province + city + dis;
//            }
//
//            view.<TextView>findViewById(R.id.text_view).setText(content);
//        }
//    }

    //container
    private void parseCollaborator(LinearLayout view, MetadataModel model) {
        if (model.value instanceof ArrayList) {
            ArrayList<String> arrayList = (ArrayList<String>) model.value;
            for (String userEmail : arrayList) {
                UserModel userModel = getRelatedUserByEmail(userEmail);
                if (userModel == null) {
                    continue;
                }

                View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_avatar_username_round, null);
                TextView user_name_text_view = ltr.findViewById(R.id.user_name);
                ShapeableImageView imageView = ltr.findViewById(R.id.user_avatar);
                Glide.with(imageView)
                        .load(userModel.getAvatarUrl())
                        .apply(GlideLoadConfig.getOptions())
                        .into(imageView);

                user_name_text_view.setText(userModel.getName());
                view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
            }
        }
    }

    private UserModel getRelatedUserByEmail(String email) {
        if (relatedUsers == null) {
            return null;
        }

        Optional<UserModel> op = relatedUsers.stream().filter(f -> f.getEmail().equals(email)).findFirst();
        return op.orElse(null);
    }

    private void parseSingleSelect(LinearLayout view, MetadataModel model) {
        if (model.configData == null || CollectionUtils.isEmpty(model.configData)) {
            return;
        }

        MetadataConfigDataModel configDataModel = model.configData.get(0);
        if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
            return;
        }

        if (model.value instanceof String && !TextUtils.isEmpty(model.value.toString())) {
            String value = (String) model.value;

            OptionsTagModel option = configDataModel.options.stream().filter(f -> f.id.equals(value)).findFirst().get();
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_text_round, null);
            TextView textView = ltr.findViewById(R.id.text);
            MaterialCardView cardView = ltr.findViewById(R.id.card_view);

            int resStrId = getResNameByKey(option.id);
            if (resStrId != 0) {
                textView.setText(resStrId);
            } else {
                textView.setText(option.name);
            }


//            if (!TextUtils.isEmpty(option.borderColor)) {
//            }

            if (!TextUtils.isEmpty(option.textColor)) {
                textView.setTextColor(Color.parseColor(option.textColor));
            }

            if (!TextUtils.isEmpty(option.color)) {
                cardView.setCardBackgroundColor(Color.parseColor(option.color));
            }

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    private void parseMultiSelect(LinearLayout view, MetadataModel model) {
        if (model.configData == null || CollectionUtils.isEmpty(model.configData)) {
            return;
        }

        MetadataConfigDataModel configDataModel = model.configData.get(0);
        if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
            return;
        }

        if (model.value instanceof ArrayList) {
            ArrayList<String> arrayList = (ArrayList<String>) model.value;
            for (String key : arrayList) {
                OptionsTagModel option = configDataModel.options.stream().filter(f -> f.id.equals(key)).findFirst().get();
                View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_text_round, null);
                TextView textView = ltr.findViewById(R.id.text);
                MaterialCardView cardView = ltr.findViewById(R.id.card_view);

                int resStrId = getResNameByKey(option.id);
                if (resStrId != 0) {
                    textView.setText(resStrId);
                } else {
                    textView.setText(option.name);
                }


//            if (!TextUtils.isEmpty(option.borderColor)) {
//            }

                if (!TextUtils.isEmpty(option.textColor)) {
                    textView.setTextColor(Color.parseColor(option.textColor));
                }

                if (!TextUtils.isEmpty(option.color)) {
                    cardView.setCardBackgroundColor(Color.parseColor(option.color));
                }

                view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
            }
        }
    }


//    private void parseImage(LinearLayout view, MetadataModel model) {
//        if (model.value instanceof ArrayList) {
//            FlexboxLayout flexboxLayout = view.findViewById(R.id.flex_box);
//
//            int wh = SizeUtils.dp2px(24);
//            FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(wh, wh);
//            flexLayoutParams.bottomMargin = DP_4;
//            flexLayoutParams.rightMargin = DP_4;
//
//            ArrayList<String> imgList = (ArrayList<String>) model.value;
//            for (int i = 0; i < imgList.size(); i++) {
//                String url = imgList.get(i);
//                ImageView imageView = new ImageView(flexboxLayout.getContext());
//                Glide.with(imageView)
//                        .load(GlideLoadConfig.getGlideUrl(url))
//                        .apply(GlideLoadConfig.getOptions())
//                        .into(imageView);
//                flexboxLayout.addView(imageView, flexLayoutParams);
//            }
//        }
//    }

//    private void parseFile(LinearLayout view, MetadataModel model) {
//        if (model.value instanceof ArrayList) {
//            FlexboxLayout flexboxLayout = view.findViewById(R.id.flex_box);
//
//            int wh = SizeUtils.dp2px(24);
//            FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(wh, wh);
//            flexLayoutParams.bottomMargin = DP_4;
//            flexLayoutParams.rightMargin = DP_4;
//
//            ArrayList<LinkedTreeMap<String, String>> fileList = (ArrayList<LinkedTreeMap<String, String>>) model.value;
//            for (LinkedTreeMap<String, String> treeMap : fileList) {
//                ImageView imageView = new ImageView(flexboxLayout.getContext());
//                String url = treeMap.get("url").toString();
//
//                int rid = MimeTypes.getFileIconFromExtension(url);
//                imageView.setImageResource(rid);
//                flexboxLayout.addView(imageView, flexLayoutParams);
//            }
//        }
//    }

    private void parseRate(LinearLayout view, MetadataModel model) {

        if (model.configData == null || CollectionUtils.isEmpty(model.configData)) {
            return;
        }

        MetadataConfigDataModel configDataModel = model.configData.get(0);

        int wh = SizeUtils.dp2px(16);
        FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(wh, wh);
        flexLayoutParams.rightMargin = DP_4;

        int selectCount = 0;
        if (model.value != null) {
            selectCount = ((Number) model.value).intValue();
        }

        for (int i = 0; i < configDataModel.rate_max_number; i++) {
            ImageView ltr = new ImageView(view.getContext());

            int t;
            if (i < selectCount) {
                if (TextUtils.isEmpty(configDataModel.rate_style_color)) {
                    t = ContextCompat.getColor(requireContext(), R.color.grey);
                } else {
                    t = Color.parseColor(configDataModel.rate_style_color);
                }
            } else {
                t = ContextCompat.getColor(requireContext(), R.color.light_gray);
            }

            ColorStateList stateList = ColorStateList.valueOf(t);

            ltr.setImageTintList(stateList);
            ltr.setImageResource(R.drawable.ic_star_32);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, flexLayoutParams);
        }
    }

    private void getRateColor() {

    }
//
//    private void parseCheckbox(LinearLayout view, MetadataModel model) {
//        FlexboxLayout flexboxLayout = view.findViewById(R.id.flex_box);
//        FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
//        flexLayoutParams.bottomMargin = DP_4;
//        flexLayoutParams.rightMargin = DP_4;
//        flexLayoutParams.height = SizeUtils.dp2px(30);
//
//        AppCompatCheckBox checkBox = new AppCompatCheckBox(view.getContext());
//        checkBox.setText("");
//        checkBox.setClickable(false);
//        if (model.value instanceof Boolean) {
//            checkBox.setChecked((Boolean) model.value);
//        }
//        flexboxLayout.addView(checkBox, flexLayoutParams);
//    }

//    private void parseLink(LinearLayout view, MetadataModel model) {
//
//        if (model.value instanceof ArrayList) {
//
//            FlexboxLayout flexboxLayout = view.findViewById(R.id.flex_box);
//            FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
//            flexLayoutParams.bottomMargin = DP_4;
//            flexLayoutParams.rightMargin = DP_4;
//
//            ArrayList<LinkedTreeMap<String, String>> arrayList = (ArrayList<LinkedTreeMap<String, String>>) model.value;
//            if (!CollectionUtils.isEmpty(arrayList)) {
//                LinkedTreeMap<String, String> hashMap = arrayList.get(0);
//                View ltr = LayoutInflater.from(flexboxLayout.getContext()).inflate(R.layout.layout_text_round, null);
//                TextView textView = ltr.findViewById(R.id.text);
//                textView.setMaxLines(1);
//                textView.setEllipsize(TextUtils.TruncateAt.END);
//                textView.setText(hashMap.get("display_value"));
//                flexboxLayout.addView(ltr, flexLayoutParams);
//
//                //
//                flexboxLayout.addView(TaskViews.getInstance().getSingleLineTextView(context, "..."));
//            }
//        }
//    }
//
//    private void parseDigitalSign(LinearLayout view, WorkFlowSimplifyModel workFlowModel, MetadataModel model) {
//        if (model.value instanceof LinkedTreeMap) {
//            FlexboxLayout flexboxLayout = view.findViewById(R.id.flex_box);
//            FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
//            flexLayoutParams.bottomMargin = DP_4;
//            flexLayoutParams.rightMargin = DP_4;
//
//            LinkedTreeMap<String, Object> treeMap = (LinkedTreeMap<String, Object>) model.value;
//
//            ///digital-signs/2023-05/chaohui.wang%40seafile.com-1684141281136.png
//            String url = treeMap.get("sign_image_url").toString();
//
//            //https://dev.seatable.cn/thumbnail/workspace/246/asset/cf317040-a299-4311-a565-27d8b3dde319/digital-signs/2023-05/chaohui.wang%40seafile.com-1684141281136.png?size=256
//            url = IO.getSingleton().getHostUrl() + "thumbnail/workspace/" + workFlowModel.id + "/asset/" + workFlowModel.dtable_uuid + url;
//
//            ImageView imageView = Views.TableActivityViews.getImageViewWithWhAndUrl(flexboxLayout.getContext(), 24, url);
//            flexboxLayout.addView(imageView, flexLayoutParams);
//        }
//    }
}
