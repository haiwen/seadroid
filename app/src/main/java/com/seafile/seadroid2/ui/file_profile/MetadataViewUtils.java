package com.seafile.seadroid2.ui.file_profile;

import static com.seafile.seadroid2.config.Constants.DP.DP_4;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.Resources;
import android.graphics.Color;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.appcompat.widget.AppCompatCheckBox;
import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.bumptech.glide.Glide;
import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.card.MaterialCardView;
import com.google.android.material.imageview.ShapeableImageView;
import com.google.gson.internal.LinkedTreeMap;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.DateFormatType;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataConfigDataModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataModel;
import com.seafile.seadroid2.framework.model.sdoc.OptionsTagModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocTagModel;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.util.Utils;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

public class MetadataViewUtils {
    private static FlexboxLayout.LayoutParams getFlexParams() {
        FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        flexLayoutParams.bottomMargin = DP_4;
        flexLayoutParams.rightMargin = DP_4;
        return flexLayoutParams;
    }

    public static void addEmptyText(Context context, LinearLayout view) {
        View ltr = LayoutInflater.from(context).inflate(R.layout.layout_textview, null);
        TextView textView = ltr.<TextView>findViewById(R.id.text_view);
        textView.setText(R.string.empty);
        textView.setTextColor(ContextCompat.getColor(context, R.color.grey));

        view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
    }

    //default
    public static void addNotSupportedLayoutView(Context context, LinearLayout view) {

        View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
        ltr.<TextView>findViewById(R.id.text_view).setText(R.string.not_supported);
        ltr.<TextView>findViewById(R.id.text_view).setTextColor(ContextCompat.getColor(context, R.color.grey));

        view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
    }

    public static void parseText(Context context, LinearLayout view, MetadataModel model) {
        if (model.value instanceof String) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(model.value.toString());

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    public static void parseLongText(Context context, LinearLayout view, MetadataModel model) {
        if (model.value instanceof String) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(model.value.toString());

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }


    public static void parseDuration(Context context, LinearLayout view, MetadataModel model) {
        if (model.value instanceof String) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(model.value.toString());

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    public static void parseNumber(Context context, LinearLayout view, MetadataModel model) {
        if (model.value instanceof Number number) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);

            if (TextUtils.equals(model.key, "_size")) {
                ltr.<TextView>findViewById(R.id.text_view).setText(Utils.readableFileSize(number.intValue()));
            } else {
                boolean isInteger = (number.doubleValue() % 1 == 0);
                String r = isInteger ? Integer.toString(number.intValue()) : Double.toString(number.doubleValue());
                ltr.<TextView>findViewById(R.id.text_view).setText(r);
            }

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    public static void parseDate(Context context, LinearLayout view, MetadataModel model) {
        if (model.value instanceof OffsetDateTime date) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);

            String d = date.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace("T", " ");
            ltr.<TextView>findViewById(R.id.text_view).setText(d);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        } else if (model.value instanceof String date) {

            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);

            Date date1 = TimeUtils.string2Date(date, DateFormatType.DATE_XXX);
            String d = TimeUtils.date2String(date1, DateFormatType.DATE_YMD_HMS);

            ltr.<TextView>findViewById(R.id.text_view).setText(d);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }


    //container
    public static void parseCollaborator(Context context, LinearLayout view, FileProfileConfigModel configModel, MetadataModel model) {
        if (model.value instanceof ArrayList) {
            ArrayList<String> arrayList = (ArrayList<String>) model.value;
            for (String userEmail : arrayList) {
                UserModel userModel = getRelatedUserByEmail(configModel, userEmail);
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

    private static UserModel getRelatedUserByEmail(FileProfileConfigModel configModel, String email) {
        Optional<UserModel> op = configModel.getRelatedUserList().stream().filter(f -> f.getEmail().equals(email)).findFirst();
        return op.orElse(null);
    }


    public static void parseSingleSelect(Context context, LinearLayout view, MetadataModel model) {
        if (model.configData == null || CollectionUtils.isEmpty(model.configData)) {
            return;
        }

        MetadataConfigDataModel configDataModel = model.configData.get(0);
        if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
            return;
        }

        if (model.value instanceof String value && !TextUtils.isEmpty(model.value.toString())) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_text_round, null);
            TextView textView = ltr.findViewById(R.id.text);
            MaterialCardView cardView = ltr.findViewById(R.id.card_view);

            Optional<OptionsTagModel> option = configDataModel.options.stream().filter(f -> f.name.equals(value)).findFirst();
            if (option.isPresent()) {
                OptionsTagModel t = option.get();
                int r = ColumnTypeUtils.getResNameByKey(t.name);
                if (r == Resources.ID_NULL) {
                    textView.setText(t.name);
                } else {
                    textView.setText(r);
                }

                textView.setTextColor(Color.parseColor(t.getTextColor()));
                cardView.setCardBackgroundColor(Color.parseColor(t.getColor()));
            } else {
                textView.setText(value);
            }

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    public static void parseMultiSelect(Context context, LinearLayout view, MetadataModel model) {
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
                View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_text_round, null);
                TextView textView = ltr.findViewById(R.id.text);
                MaterialCardView cardView = ltr.findViewById(R.id.card_view);

                Optional<OptionsTagModel> option = configDataModel.options.stream().filter(f -> f.name.equals(key)).findFirst();
                if (option.isPresent()) {
                    OptionsTagModel t = option.get();
                    textView.setText(t.name);
                    textView.setTextColor(Color.parseColor(t.textColor));
                    cardView.setCardBackgroundColor(Color.parseColor(t.color));
                } else {
                    textView.setText(key);
                }

                view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
            }
        }
    }

    //location
    public static void parseGeoLocation(Context context, LinearLayout view, MetadataModel model) {
        if (model.value instanceof LinkedTreeMap) {
            LinkedTreeMap<String, Object> treeMap = (LinkedTreeMap<String, Object>) model.value;

            List<MetadataConfigDataModel> configDataModelList = model.configData;
            if (CollectionUtils.isEmpty(configDataModelList)) {
                return;
            }
            MetadataConfigDataModel columnDataModel = configDataModelList.get(0);
            String geo_format = columnDataModel.geo_format;

            String content = "";
            if (TextUtils.equals("lng_lat", geo_format)) {
                String lat = treeMap.get("lat").toString();
                String lng = treeMap.get("lng").toString();
                String formatLat = Utils.convertLatitude(lat);
                String formatLng = Utils.convertLongitude(lng);
                content = formatLat + ", " + formatLng;
            } else if (TextUtils.equals("geolocation", geo_format)) {
                String province = treeMap.get("province").toString();
                String city = treeMap.get("city").toString();
                String dis = treeMap.get("district").toString();
                String detail = treeMap.get("detail").toString();
                content = province + city + dis + detail;
            } else if (TextUtils.equals("country_region", geo_format)) {
                content = treeMap.get("country_region").toString();
            } else if (TextUtils.equals("province", geo_format)) {
                content = treeMap.get("province").toString();
            } else if (TextUtils.equals("province_city", geo_format)) {
                String province = treeMap.get("province").toString();
                String city = treeMap.get("city").toString();
                content = province + city;
            } else if (TextUtils.equals("province_city_district", geo_format)) {
                String province = treeMap.get("province").toString();
                String city = treeMap.get("city").toString();
                String dis = treeMap.get("district").toString();
                content = province + city + dis;
            }

            content = content.trim();
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            if (TextUtils.isEmpty(content) || TextUtils.equals(",", content)) {
                ltr.<TextView>findViewById(R.id.text_view).setText(context.getResources().getString(R.string.empty));
                ltr.<TextView>findViewById(R.id.text_view).setTextColor(ContextCompat.getColor(context, R.color.grey));
            } else {
                ltr.<TextView>findViewById(R.id.text_view).setTextColor(ContextCompat.getColor(context, R.color.item_title_color));
                ltr.<TextView>findViewById(R.id.text_view).setText(content);
            }

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }

    //tag
    public static void parseTag(Context context, LinearLayout view, FileProfileConfigModel configModel, MetadataModel model) {
        if (model.value instanceof ArrayList) {
            if (configModel.getTagMap().isEmpty()) {
                return;
            }

            FlexboxLayout flexboxLayout = view.findViewById(R.id.flex_box);
            FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
            flexLayoutParams.bottomMargin = DP_4;
            flexLayoutParams.rightMargin = DP_4;

            ArrayList<LinkedTreeMap<String, String>> arrayList = (ArrayList<LinkedTreeMap<String, String>>) model.value;
            if (!CollectionUtils.isEmpty(arrayList)) {
                for (LinkedTreeMap<String, String> map : arrayList) {
                    String rowId = map.get("row_id");
                    SDocTagModel tagModel = configModel.getTagMap().get(rowId);
                    if (tagModel == null) {
                        continue;
                    }

                    View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_tag, null);

                    MaterialCardView cardView = ltr.findViewById(R.id.indicator);

                    if (!TextUtils.isEmpty(tagModel.color)) {
                        cardView.setCardBackgroundColor(Color.parseColor(tagModel.color));
                    }

                    TextView textView = ltr.findViewById(R.id.text);
                    textView.setMaxLines(1);
                    textView.setEllipsize(TextUtils.TruncateAt.END);
                    textView.setText(tagModel.name);
                    flexboxLayout.addView(ltr, flexLayoutParams);
                }

            }
        }
    }

    public static void parseImage(Context context, LinearLayout view, MetadataModel model) {
        addNotSupportedLayoutView(context, view);
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
    }

    public static void parseFile(Context context, LinearLayout view, MetadataModel model) {
        addNotSupportedLayoutView(context, view);
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
    }

    public static void parseRate(Context context, LinearLayout view, MetadataModel model) {

        if (model.value == null) {

            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(R.string.empty);
            ltr.<TextView>findViewById(R.id.text_view).setTextColor(ContextCompat.getColor(context, R.color.grey));

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
            return;
        }

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
                    t = ContextCompat.getColor(context, R.color.grey);
                } else {
                    t = Color.parseColor(configDataModel.rate_style_color);
                }
            } else {
                t = ContextCompat.getColor(context, R.color.light_gray);
            }

            ColorStateList stateList = ColorStateList.valueOf(t);

            ltr.setImageTintList(stateList);
            ltr.setImageResource(R.drawable.ic_star_32);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, flexLayoutParams);
        }
    }


    public static void parseCheckbox(Context context, LinearLayout view, MetadataModel model) {
        if (model.value instanceof Boolean booleanValue) {

            AppCompatCheckBox checkBox = new AppCompatCheckBox(view.getContext());
            checkBox.setText("");
            checkBox.setClickable(false);
            checkBox.setChecked(booleanValue);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(checkBox, getFlexParams());
        }
    }


    public static void parseLink(Context context, LinearLayout view, MetadataModel model) {
        addNotSupportedLayoutView(context, view);

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
    }

    //
    public static void parseDigitalSign(Context context, LinearLayout view, MetadataModel model) {
        addNotSupportedLayoutView(context, view);

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
    }


}
