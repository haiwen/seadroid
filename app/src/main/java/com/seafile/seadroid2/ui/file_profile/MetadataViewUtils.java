package com.seafile.seadroid2.ui.file_profile;

import static com.seafile.seadroid2.config.Constants.DP.DP_16;
import static com.seafile.seadroid2.config.Constants.DP.DP_4;
import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.text.Editable;
import android.text.InputFilter;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.text.method.LinkMovementMethod;
import android.util.Pair;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.appcompat.widget.AppCompatCheckBox;
import androidx.core.content.ContextCompat;
import androidx.core.graphics.drawable.DrawableCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NumberUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.bumptech.glide.Glide;
import com.bumptech.glide.RequestBuilder;
import com.bumptech.glide.request.target.Target;
import com.google.android.flexbox.FlexWrap;
import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.card.MaterialCardView;
import com.google.android.material.imageview.ShapeableImageView;
import com.google.gson.internal.LinkedTreeMap;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.DateFormatType;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.filter.CharacterNoRepeatSpecialCountInputFilter;
import com.seafile.seadroid2.filter.NumberDotOnlyInputFilter;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.GeoLocationModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataConfigDataModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataModel;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.listener.OnItemRemoveListener;
import com.seafile.seadroid2.listener.OnSingleOptionChangedListener;
import com.seafile.seadroid2.listener.OnMultiOptionsChangedListener;
import com.seafile.seadroid2.listener.OnTextChangedListener;
import com.seafile.seadroid2.listener.OnTextViewClickListener;
import com.seafile.seadroid2.listener.OnViewClickListener;
import com.seafile.seadroid2.markdown.LinkClickSelfSpan;
import com.seafile.seadroid2.view.SupportMetadataCheckGroup;
import com.seafile.seadroid2.view.SupportMetadataRadioGroup;
import com.seafile.seadroid2.view.ratingbar.OnRatingChangedListener;
import com.seafile.seadroid2.view.ratingbar.RatingStatus;
import com.seafile.seadroid2.view.ratingbar.SeaRatingBar;

import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.Image;
import org.commonmark.node.Link;

import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;

import io.noties.markwon.AbstractMarkwonPlugin;
import io.noties.markwon.LinkResolver;
import io.noties.markwon.Markwon;
import io.noties.markwon.MarkwonPlugin;
import io.noties.markwon.MarkwonSpansFactory;
import io.noties.markwon.core.CorePlugin;
import io.noties.markwon.core.CoreProps;
import io.noties.markwon.core.spans.LinkSpan;
import io.noties.markwon.ext.latex.JLatexMathPlugin;
import io.noties.markwon.ext.strikethrough.StrikethroughPlugin;
import io.noties.markwon.ext.tables.TablePlugin;
import io.noties.markwon.ext.tables.TableTheme;
import io.noties.markwon.ext.tasklist.TaskListDrawable;
import io.noties.markwon.ext.tasklist.TaskListPlugin;
import io.noties.markwon.html.HtmlPlugin;
import io.noties.markwon.image.AsyncDrawable;
import io.noties.markwon.image.ImageProps;
import io.noties.markwon.image.glide.GlideImagesPlugin;
import io.noties.markwon.inlineparser.MarkwonInlineParserPlugin;
import io.noties.markwon.linkify.LinkifyPlugin;

public class MetadataViewUtils {

    private static final LinkedHashMap<String, Boolean> _supportedField = new LinkedHashMap<String, Boolean>();

    public static LinkedHashMap<String, Boolean> getSupportedFieldMap() {
        if (_supportedField.isEmpty()) {
            _supportedField.put("_size", false);
            _supportedField.put("_file_modifier", false);
            _supportedField.put("_file_mtime", false);
            _supportedField.put("_owner", true);
            _supportedField.put("_description", true);
            _supportedField.put("_collaborators", true);
            _supportedField.put("_reviewer", true);
            _supportedField.put("_status", true);
            _supportedField.put("_location", false);
            _supportedField.put("_tags", true);
            _supportedField.put("_rate", true);
            _supportedField.put("_expire_time", true);
        }
        return _supportedField;
    }

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
        if (model.value instanceof String str) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            String parsedStr = null;
            if (StringUtils.isNotEmpty(str)) {
                parsedStr = str.replace("\n", "");
            }
            ltr.<TextView>findViewById(R.id.text_view).setText(parsedStr);

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

                String text = getFormattedNumber(number, model.getConfigData());
//                String r = isInteger ? Integer.toString(number.intValue()) : Double.toString(number.doubleValue());
                ltr.<TextView>findViewById(R.id.text_view).setText(text);
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

            String format = null;
            if (model.getConfigData() != null) {
                format = model.getConfigData().getFormat();
            }
            if (StringUtils.isEmpty(format)) {
                format = DateFormatType.DATE_YMD_HMS;
            }
            String d = TimeUtils.date2String(date1, format);

            ltr.<TextView>findViewById(R.id.text_view).setText(d);

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
        }
    }


    //container
    public static void parseCollaborator(Context context, LinearLayout view, FileProfileConfigModel configModel, MetadataModel model) {
        if (model.value instanceof ArrayList<?> arrayList) {
            for (Object object : arrayList) {
                if (object instanceof String userEmail) {
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
    }

    private static UserModel getRelatedUserByEmail(FileProfileConfigModel configModel, String email) {
        Optional<UserModel> op = configModel.getRelatedUserList().stream().filter(f -> f.getEmail().equals(email)).findFirst();
        return op.orElse(null);
    }


    public static void parseSingleSelect(Context context, LinearLayout view, MetadataModel model) {
        if (model.config == null || CollectionUtils.isEmpty(model.config)) {
            return;
        }

        MetadataConfigDataModel configDataModel = model.config.get(0);
        if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
            return;
        }

        if (model.value instanceof String value && !TextUtils.isEmpty(model.value.toString())) {
            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_text_round, null);
            TextView textView = ltr.findViewById(R.id.text);
            MaterialCardView cardView = ltr.findViewById(R.id.card_view);

            Optional<OptionTagModel> option = configDataModel.options.stream().filter(f -> f.name.equals(value)).findFirst();
            if (option.isPresent()) {
                OptionTagModel t = option.get();
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
        if (model.config == null || CollectionUtils.isEmpty(model.config)) {
            return;
        }

        MetadataConfigDataModel configDataModel = model.config.get(0);
        if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
            return;
        }

        if (model.value instanceof ArrayList<?> arrayList) {
            for (Object object : arrayList) {
                if (object instanceof String strKey) {
                    View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_text_round, null);
                    TextView textView = ltr.findViewById(R.id.text);
                    MaterialCardView cardView = ltr.findViewById(R.id.card_view);

                    Optional<OptionTagModel> option = configDataModel.options.stream().filter(f -> f.name.equals(strKey)).findFirst();
                    if (option.isPresent()) {
                        OptionTagModel t = option.get();
                        textView.setText(t.name);
                        textView.setTextColor(Color.parseColor(t.textColor));
                        cardView.setCardBackgroundColor(Color.parseColor(t.color));
                    } else {
                        textView.setText(strKey);
                    }

                    view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
                }
            }
        }

    }

    //location
    public static void parseGeoLocation(Context context, LinearLayout view, MetadataModel metadataModel, FileProfileConfigModel configModel) {
        GeoLocationModel locationModel = parseGeoLocation(metadataModel, configModel);

        View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
        if (locationModel == null || TextUtils.isEmpty(locationModel.getText())) {
            ltr.<TextView>findViewById(R.id.text_view).setText(context.getResources().getString(R.string.empty));
            ltr.<TextView>findViewById(R.id.text_view).setTextColor(ContextCompat.getColor(context, R.color.grey));
        } else {
            ltr.<TextView>findViewById(R.id.text_view).setTextColor(ContextCompat.getColor(context, R.color.item_title_color));
            ltr.<TextView>findViewById(R.id.text_view).setText(locationModel.getText());
        }

        view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
    }

    private static String getLinkedMapStringValue(LinkedTreeMap<?, ?> tMap, String key) {
        Object o = tMap.getOrDefault(key, null);
        if (o == null) {
            return "";
        }

        if (o instanceof String) {
            return o.toString();
        }
        return "";
    }

    private static String joinStrings(String... strs) {
        if (strs == null || strs.length == 0) {
            return "";
        }
        return StringUtils.join(strs);
    }

    private static String joinWithStrings(String delimiter, String... strs) {
        if (strs == null || strs.length == 0) {
            return "";
        }
        return StringUtils.joinWith(delimiter, Arrays.stream(strs).toArray());
    }

    //tag
    public static void parseTag(Context context, LinearLayout view, FileProfileConfigModel configModel, MetadataModel model) {
        if (model.value instanceof ArrayList<?> arrayList) {
            if (configModel.getTagMap().isEmpty()) {
                return;
            }

            FlexboxLayout flexboxLayout = view.findViewById(R.id.flex_box);
            FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
            flexLayoutParams.bottomMargin = DP_4;
            flexLayoutParams.rightMargin = DP_4;

            if (!CollectionUtils.isEmpty(arrayList)) {
                for (Object o : arrayList) {
                    if (o instanceof LinkedTreeMap<?, ?> map) {
                        String rowId = getLinkedMapStringValue(map, "row_id");
                        OptionTagModel tagModel = configModel.getTagMap().get(rowId);
                        if (tagModel == null) {
                            continue;
                        }

                        View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_detail_tag, null);

                        MaterialCardView indicatorView = ltr.findViewById(R.id.indicator);
                        if (!TextUtils.isEmpty(tagModel.color)) {
                            indicatorView.setCardBackgroundColor(Color.parseColor(tagModel.color));
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
    }

    public static void parseRate(Context context, LinearLayout view, MetadataModel model) {

        if (model.value == null) {

            View ltr = LayoutInflater.from(view.getContext()).inflate(R.layout.layout_textview, null);
            ltr.<TextView>findViewById(R.id.text_view).setText(R.string.empty);
            ltr.<TextView>findViewById(R.id.text_view).setTextColor(ContextCompat.getColor(context, R.color.grey));

            view.<FlexboxLayout>findViewById(R.id.flex_box).addView(ltr, getFlexParams());
            return;
        }

        if (model.config == null || CollectionUtils.isEmpty(model.config)) {
            return;
        }

        MetadataConfigDataModel configDataModel = model.config.get(0);

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

    /// ///////////////////////////////build editable view///////////////////////////
    private static Pair<Boolean, FlexboxLayout> getFlexContainer(Context context, LinearLayout parentContainer, String key) {
        FlexboxLayout flexboxContainer = parentContainer.findViewWithTag(key);
        boolean isAdded = flexboxContainer != null;
        if (flexboxContainer != null) {
            flexboxContainer.removeAllViews();
        } else {
            flexboxContainer = new FlexboxLayout(context);
            LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(-1, -2);
            llp.topMargin = Constants.DP.DP_8;
            llp.bottomMargin = Constants.DP.DP_8;
            flexboxContainer.setLayoutParams(llp);
            flexboxContainer.setTag(key);
            flexboxContainer.setFlexWrap(FlexWrap.WRAP);
        }
        return new Pair<>(isAdded, flexboxContainer);
    }

    public static EditText getFlexEditView(Context context) {
        EditText editText = new EditText(context);
        editText.setTextSize(16);
        editText.setTextColor(context.getColor(R.color.bar_title_color));
        editText.setGravity(Gravity.CENTER_VERTICAL);
        editText.setBackground(AppCompatResources.getDrawable(context, R.drawable.shape_task_view_editable));
        editText.setPadding(Constants.DP.DP_16, Constants.DP.DP_8, Constants.DP.DP_16, Constants.DP.DP_8);

        editText.setLayoutParams(new FlexboxLayout.LayoutParams(-1, -2));
        return editText;
    }

    public static TextView getFlexTextView(Context context) {
        return getFlexTextView(context, null);
    }

    public static TextView getFlexTextView(Context context, String string) {
        TextView textView = new TextView(context);
        textView.setTextSize(16);
        textView.setTextColor(context.getColor(R.color.bar_title_color));
        textView.setText(string);
        textView.setGravity(Gravity.CENTER_VERTICAL);
        textView.setBackground(AppCompatResources.getDrawable(context, R.drawable.shape_stroke1_radius4));
        textView.setPadding(Constants.DP.DP_16, Constants.DP.DP_8, Constants.DP.DP_16, Constants.DP.DP_8);

        FlexboxLayout.LayoutParams flp = new FlexboxLayout.LayoutParams(-1, -2);
        textView.setLayoutParams(flp);
        return textView;
    }


    public static void buildEditableText(Context context,
                                         boolean editable,
                                         LinearLayout parentContainer,
                                         MetadataModel metadataModel,
                                         OnTextChangedListener onTextChangedListener) {
        if (metadataModel == null) {
            return;
        }

        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        EditText text = getFlexEditView(context);

        if (metadataModel.value instanceof String) {
            String initText = metadataModel.value.toString();
            text.setText(initText);
        }


        if (!editable) {
            text.setEnabled(false);
            text.setBackgroundResource(R.drawable.shape_task_view_no_editable);
        }

        if (onTextChangedListener != null) {
            text.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {

                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {

                }

                @Override
                public void afterTextChanged(Editable s) {
                    onTextChangedListener.onChanged(s.toString());
                }
            });
        }
        flexboxContainer.addView(text);

        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }


    public static void buildEditableLongText(Context context,
                                             boolean editable,
                                             LinearLayout parentContainer,
                                             MetadataModel metadataModel,
                                             OnViewClickListener clickListener) {
        if (metadataModel == null) {
            return;
        }

        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        //text view
        TextView textView = getFlexTextView(context);
        textView.setMinHeight(Constants.DP.DP_72);
        textView.setHint(R.string.empty);
        textView.setGravity(Gravity.START);

        textView.setPadding(Constants.DP.DP_16, Constants.DP.DP_8, Constants.DP.DP_16, Constants.DP.DP_8);
        textView.setMovementMethod(LinkMovementMethod.getInstance());

        if (editable) {
            textView.setBackgroundResource(R.drawable.shape_task_view_editable);
        } else {
            textView.setBackgroundResource(R.drawable.shape_task_view_no_editable);
        }

        if (editable && clickListener != null) {
            textView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    clickListener.onClick(flexboxContainer, metadataModel.key);
                }
            });
        }

        String initText = "";
        if (metadataModel.value instanceof String) {
            initText = metadataModel.value.toString();
            textView.setText(initText);
        }

        //set text
        getRenderedMarkdownText(context).setMarkdown(textView, initText);
        flexboxContainer.addView(textView);

        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }


    /**
     * get markdown text
     */
    protected static Markwon getRenderedMarkdownText(Context context) {
        final TaskListDrawable drawable = new TaskListDrawable(Color.GRAY, Color.GRAY, Color.WHITE);

        final TableTheme tableTheme = new TableTheme.Builder()
                .tableBorderColor(Color.GRAY)
                .tableBorderWidth(1)
                .tableCellPadding(2)
                .tableHeaderRowBackgroundColor(Color.GRAY)
                .tableEvenRowBackgroundColor(Color.LTGRAY)
                .tableOddRowBackgroundColor(Color.WHITE)
                .build();

        final MarkwonPlugin glidePlugin = GlideImagesPlugin.create(new GlideImagesPlugin.GlideStore() {
            @NonNull
            @Override
            public RequestBuilder<Drawable> load(@NonNull AsyncDrawable drawable) {
                final Drawable placeholder = ContextCompat.getDrawable(context, R.drawable.loading_anim);
                placeholder.setBounds(0, 0, 100, 100);
                final Drawable errorPlaceholder = ContextCompat.getDrawable(context, R.drawable.icon_image_error_filled);
                errorPlaceholder.setBounds(0, 0, 100, 100);
                return Glide.with(SeadroidApplication.getAppContext())
                        .load(GlideLoadConfig.getGlideUrl(drawable.getDestination()))
                        .placeholder(placeholder)
                        .error(errorPlaceholder);
            }

            @Override
            public void cancel(@NonNull Target<?> target) {
                Glide.with(SeadroidApplication.getAppContext()).clear(target);
            }
        });

        return Markwon.builder(context)
                .usePlugin(CorePlugin.create())
                .usePlugin(TablePlugin.create(context))
                .usePlugin(TaskListPlugin.create(drawable))
                .usePlugin(StrikethroughPlugin.create())
                .usePlugin(HtmlPlugin.create())
                .usePlugin(LinkifyPlugin.create())
                .usePlugin(MarkwonInlineParserPlugin.create())
                .usePlugin(glidePlugin)
                .usePlugin(JLatexMathPlugin.create(SizeUtils.px2sp(16), new JLatexMathPlugin.BuilderConfigure() {
                    @Override
                    public void configureBuilder(@NonNull JLatexMathPlugin.Builder builder) {
                        builder.inlinesEnabled(true);
                    }
                }))
                .usePlugin(new AbstractMarkwonPlugin() {
                    @Override
                    public void configureSpansFactory(@NonNull MarkwonSpansFactory.Builder builder) {
                        super.configureSpansFactory(builder);
                        builder.setFactory(Link.class, (configuration, props) ->
                                // create a subclass of markwon LinkSpan
                                new LinkClickSelfSpan(
                                        configuration.theme(),
                                        CoreProps.LINK_DESTINATION.require(props),
                                        configuration.linkResolver()
                                )
                        );

                        builder.appendFactory(Image.class, (configuration, props) -> {
                            String url = ImageProps.DESTINATION.require(props);
                            return new LinkSpan(
                                    configuration.theme(),
                                    url,
                                    new ImageLinkResolver(context, configuration.linkResolver()));
                        });
                    }
                })
                .build();
    }

    public static class ImageLinkResolver implements LinkResolver {
        Context context;
        LinkResolver original;

        public ImageLinkResolver(Context context, LinkResolver original) {
            this.context = context;
            this.original = original;
        }

        @Override
        public void resolve(@NonNull View view, @NonNull String link) {
            SafeLogs.d("ImageLinkResolver => " + link);
//            MediaPlayerActivity.startThisWithUrl(context, link);
        }
    }

    public static void buildEditableNumber(Context context,
                                           boolean editable,
                                           LinearLayout parentContainer,
                                           MetadataModel metadataModel,
                                           OnTextChangedListener onTextChangedListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        EditText text = getFlexEditView(context);

        text.setEnabled(editable);
        if (editable) {
            text.setBackgroundResource(R.drawable.shape_task_view_editable);
        } else {
            text.setBackgroundResource(R.drawable.shape_task_view_no_editable);
        }

        MetadataConfigDataModel configDataModel = metadataModel.getConfigData();

        text.setKeyListener(new NumberDotOnlyInputFilter());
        text.setFilters(new InputFilter[]{new CharacterNoRepeatSpecialCountInputFilter('.', 1)});
        if (metadataModel.value instanceof Number number) {
            if (TextUtils.equals("_size", metadataModel.key)) {
                String initText = Utils.readableFileSize(number.longValue());
                text.setText(initText);
            } else {
                String t = getFormattedNumber(number, configDataModel);
                text.setText(t);
            }
        }


        if (editable && onTextChangedListener != null) {
            text.setOnFocusChangeListener(new View.OnFocusChangeListener() {
                @Override
                public void onFocusChange(View v, boolean hasFocus) {
                    if (hasFocus) {
                        String r = getOriginalNumberByFormattedString(text.getText().toString(), configDataModel);
                        text.setText(r);
                    } else {
                        // Use BigDecimal directly to avoid floating-point precision issues
                        String inputStr = text.getText().toString();
                        if (inputStr.isEmpty()) {
                            return;
                        }

                        try {
                            BigDecimal decimalValue = new BigDecimal(inputStr);
                            String r = getFormattedNumber(decimalValue, configDataModel);
                            text.setText(r);
                        } catch (NumberFormatException e) {
                            SLogs.e("Failed to parse number: " + inputStr);
                        }
                    }
                }
            });

            TextWatcher tw = new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {

                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {

                }

                @Override
                public void afterTextChanged(Editable s) {
                    onTextChangedListener.onChanged(s.toString());
                }
            };

            text.addTextChangedListener(tw);
        }

        flexboxContainer.addView(text);


        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    /**
     * Modify thousands separator symbol, adding specified thousands separator to the number.
     * input: Input number string, could be "101.11%", or "~11.11", "1.01¥", etc.
     * Need to remove non-numeric characters before calculating thousands separator.
     * <p>
     * thousandSymbol: The symbol to replace with, three types: "no" means no input,
     * return directly. "space" use space to replace. "comma" use comma to replace.
     * <p>
     * decima: Current decimal point symbol in the number string, could be dot or comma.
     *
     */
    public static String modifyThousandsSymbol(String input, String thousandSymbol, String decima) {
        if (input == null || input.isEmpty()) {
            return input;
        }

        // If no thousands separator needed, return directly
        if ("no".equals(thousandSymbol)) {
            if ("comma".equals(decima)) {
                input = input.replace(".", ",");
            }
            return input;
        }

        // Determine the final decimal separator character based on decima parameter
        String finalDecimalSeparator = "comma".equals(decima) ? "," : ".";

        // Determine the thousands separator character
        String thousandsSeparator = "space".equals(thousandSymbol) ? " " : ",";

        // Extract prefix, integer part, decimal part, and suffix
        // Input decimal point is always "."
        StringBuilder prefix = new StringBuilder(); // Non-numeric prefix (like ~, ¥)
        StringBuilder integerPart = new StringBuilder(); // Integer part (digits before ".")
        StringBuilder decimalPart = new StringBuilder(); // Decimal part (after ".")
        StringBuilder suffix = new StringBuilder(); // Non-numeric suffix (like %)

        boolean foundNumeric = false;
        boolean foundDecimal = false;
        boolean foundNegative = false;

        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);

            if (c == '-' && !foundNumeric && !foundNegative) {
                foundNegative = true;
                integerPart.append(c);
                foundNumeric = true;
            } else if (Character.isDigit(c)) {
                if (!foundDecimal) {
                    integerPart.append(c);
                } else {
                    decimalPart.append(c);
                }
                foundNumeric = true;
            } else if (c == '.' && !foundDecimal) {
                // First "." is the decimal point
                foundDecimal = true;
                foundNumeric = true;
            } else if (foundNumeric) {
                if (!foundDecimal && c != ',' && c != ' ') {
                    // After integer started but before decimal, could be old thousands separator or suffix
                    // Check if it's a valid thousands separator (comma or space)
                    if (c == ',' || c == ' ') {
                        // Skip old thousands separator
                    } else {
                        // It's suffix
                        suffix.append(c);
                        foundNumeric = false; // Mark that we're now in suffix mode
                    }
                } else if (foundDecimal) {
                    // After decimal, it's suffix
                    suffix.append(c);
                }
            } else {
                // Before numeric part, it's prefix
                prefix.append(c);
            }
        }

        String integerStr = integerPart.toString();
        if (integerStr.isEmpty()) {
            return input;
        }

        // Remove negative sign temporarily for processing
        boolean isNegative = integerStr.startsWith("-");
        if (isNegative) {
            integerStr = integerStr.substring(1);
        }

        // Remove any existing thousands separators and non-digit characters
        StringBuilder cleanInteger = new StringBuilder();
        for (int i = 0; i < integerStr.length(); i++) {
            char c = integerStr.charAt(i);
            if (Character.isDigit(c)) {
                cleanInteger.append(c);
            }
        }

        // Add thousands separator to integer part
        StringBuilder formattedInteger = new StringBuilder();
        int count = 0;
        for (int i = cleanInteger.length() - 1; i >= 0; i--) {
            formattedInteger.insert(0, cleanInteger.charAt(i));
            count++;
            if (count == 3 && i > 0) {
                formattedInteger.insert(0, thousandsSeparator);
                count = 0;
            }
        }

        // Add negative sign back if needed
        if (isNegative) {
            formattedInteger.insert(0, "-");
        }

        // Combine prefix, formatted number, decimal part (with final decimal separator), and suffix
        StringBuilder result = new StringBuilder();
        result.append(prefix.toString());
        result.append(formattedInteger.toString());
        if (decimalPart.length() > 0) {
            result.append(finalDecimalSeparator);
            result.append(decimalPart.toString());
        }
        result.append(suffix.toString());

        return result.toString();
    }

    public static String getOriginalNumberByFormattedString(String formattedString, MetadataConfigDataModel configDataModel) {
        if (formattedString == null || formattedString.isEmpty()) {
            return "0";
        }

        if (configDataModel == null) {
            // default config
            configDataModel = new MetadataConfigDataModel();
            configDataModel.thousands = "no";
            configDataModel.decimal = "dot";
            configDataModel.enable_precision = false;
            configDataModel.precision = 2;
            configDataModel.format = "number";
        }

        // ～11.010000
        // 11,0101
        // 1 010 111,000000%
        // ￥101,110000
        // 101,110000~
        // 101,110,000: enable_precision = true, precision=3,最后一个 comma 为小数点标识
        // 101,110,00: 最后一个 comma 为小数点标识
        // 101,110,000000: 最后一个 comma 为小数点标识

        // Check if the input contains percentage sign
        boolean isPercentage = formattedString.contains("%");

        String thousands = configDataModel.thousands;
        String decima = configDataModel.decimal;
        boolean enable_precision = configDataModel.enable_precision;
        int precision = configDataModel.precision;

        // Step a: Remove non-numeric prefix and suffix (except negative sign)
        StringBuilder numericPart = new StringBuilder();
        boolean foundNumeric = false;
        boolean foundNegative = false;

        for (int i = 0; i < formattedString.length(); i++) {
            char c = formattedString.charAt(i);

            if (c == '-' && !foundNumeric && !foundNegative) {
                foundNegative = true;
                numericPart.append(c);
                foundNumeric = true;
            } else if (Character.isDigit(c)) {
                numericPart.append(c);
                foundNumeric = true;
            } else if (c == '.' || c == ',' || c == ' ') {
                // Keep potential separators
                numericPart.append(c);
                foundNumeric = true;
            }
            // Other characters (prefix/suffix) are ignored
        }

        String cleanedStr = numericPart.toString();
        if (cleanedStr.isEmpty()) {
            return "0";
        }

        // Determine the separator characters based on config
        String thousandsSeparator;
        if ("space".equals(thousands)) {
            thousandsSeparator = " ";
        } else if ("comma".equals(thousands)) {
            thousandsSeparator = ",";
        } else {
            thousandsSeparator = null; // "no" means no thousands separator
        }

        String decimalSeparator = "comma".equals(decima) ? "," : ".";

        // Step b: Handle special case when thousands is "comma" AND decima is "comma"
        if ("comma".equals(thousands) && "comma".equals(decima)) {
            int lastCommaIndex = cleanedStr.lastIndexOf(",");
            if (lastCommaIndex != -1) {
                String afterLastComma = cleanedStr.substring(lastCommaIndex + 1);
                int afterLastCommaLength = afterLastComma.length();

                // Rule b.1: If characters after last comma is 1, 2, 4, 5, or 6 digits, it's a decimal point
                if (afterLastCommaLength == 1 || afterLastCommaLength == 2 ||
                        afterLastCommaLength == 4 || afterLastCommaLength == 5 ||
                        afterLastCommaLength == 6) {
                    // Last comma is decimal point
                    cleanedStr = cleanedStr.substring(0, lastCommaIndex) + "." + afterLastComma;
                }
                // Rule b.2: If characters after last comma is 3 digits
                else if (afterLastCommaLength == 3) {
                    // Check if enable_precision is true AND precision == 3
                    if (enable_precision && precision == 3) {
                        // Last comma is decimal point
                        cleanedStr = cleanedStr.substring(0, lastCommaIndex) + "." + afterLastComma;
                    } else {
                        // Last comma is thousands separator, remove it
                        cleanedStr = cleanedStr.substring(0, lastCommaIndex) + afterLastComma;
                    }
                } else {
                    // Other cases: last comma is decimal point
                    cleanedStr = cleanedStr.substring(0, lastCommaIndex) + "." + afterLastComma;
                }
            }

            // Remove all remaining commas (thousands separators)
            cleanedStr = cleanedStr.replace(",", "");
        } else {
            // Normal case: different thousands and decimal separators

            // Remove thousands separator if exists
            if (thousandsSeparator != null) {
                cleanedStr = cleanedStr.replace(thousandsSeparator, "");
            }

            // Replace decimal separator with standard "."
            if (!".".equals(decimalSeparator)) {
                cleanedStr = cleanedStr.replace(decimalSeparator, ".");
            }
        }

        // Remove any remaining spaces
        cleanedStr = cleanedStr.replace(" ", "");

        // Validate and return
        if (cleanedStr.isEmpty() || cleanedStr.equals("-")) {
            return "0";
        }

        // Handle percentage: divide by 100 if the original string contains "%"
        if (isPercentage) {
            try {
                // Use BigDecimal to avoid floating-point precision issues
                BigDecimal number = new BigDecimal(cleanedStr);
                BigDecimal dividedValue = number.divide(BigDecimal.valueOf(100), 10, java.math.RoundingMode.HALF_UP);
                cleanedStr = dividedValue.stripTrailingZeros().toPlainString();
            } catch (NumberFormatException e) {
                // If parsing fails, return as is
                SLogs.e("Failed to parse percentage number: " + cleanedStr);
            }
        }

        return cleanedStr;
    }

    @Nullable
    public static String getFormattedNumber(Number t, MetadataConfigDataModel configDataModel) {
        if (t == null) {
            return null;
        }

        if (configDataModel == null) {
            // default config
            configDataModel = new MetadataConfigDataModel();
            configDataModel.thousands = "no";
            configDataModel.decimal = "dot";
            configDataModel.enable_precision = false;
            configDataModel.precision = 2;
            configDataModel.format = "number";
        }

        //set default value
        if ("number".equals(configDataModel.format)) {
            String f;
            if (configDataModel.enable_precision) {
                f = NumberUtils.format(t.floatValue(), configDataModel.precision);
            } else {
                f = t.toString();
            }

            return modifyThousandsSymbol(f, configDataModel.thousands, configDataModel.decimal);

        } else if ("percent".equals(configDataModel.format)) {
            // Use BigDecimal to avoid floating-point precision issues
            BigDecimal decimalValue;
            if (t instanceof BigDecimal) {
                decimalValue = (BigDecimal) t;
            } else if (t instanceof Float || t instanceof Double) {
                decimalValue = BigDecimal.valueOf(t.doubleValue());
            } else {
                decimalValue = new BigDecimal(t.toString());
            }

            BigDecimal percentValue = decimalValue.multiply(BigDecimal.valueOf(100));

            String f;
            if (configDataModel.enable_precision) {
                f = NumberUtils.format(percentValue.doubleValue(), configDataModel.precision);
            } else {
                // Use BigDecimal's plain string representation to avoid precision issues
                f = percentValue.stripTrailingZeros().toPlainString();
            }

            f = f + "%";
            return modifyThousandsSymbol(f, configDataModel.thousands, configDataModel.decimal);

        } else if ("yuan".equals(configDataModel.format) || "dollar".equals(configDataModel.format) || "euro".equals(configDataModel.format)) {
            float d = t.floatValue();
            String f;
            if (configDataModel.enable_precision) {
                f = NumberUtils.format(d, configDataModel.precision);
            } else {
                f = NumberUtils.format(d, 2);//default 2
            }

            if ("yuan".equals(configDataModel.format)) {
                f = "¥" + f;
            } else if ("dollar".equals(configDataModel.format)) {
                f = "$" + f;
            } else if ("euro".equals(configDataModel.format)) {
                f = "€" + f;
            }
            return modifyThousandsSymbol(f, configDataModel.thousands, configDataModel.decimal);

        } else if ("custom_currency".equals(configDataModel.format)) {
            String f;
            if (configDataModel.enable_precision) {
                float d = t.floatValue();
                f = NumberUtils.format(d, configDataModel.precision);
            } else {
                f = t.toString();
            }

            if ("before".equals(configDataModel.currency_symbol_position)) {
                f = configDataModel.currency_symbol + f;
            } else {
                f = f + configDataModel.currency_symbol;
            }

            return modifyThousandsSymbol(f, configDataModel.thousands, configDataModel.decimal);
        }
        return t.toString();
    }


    public static void buildEditableDate(Context context,
                                         boolean editable,
                                         LinearLayout parentContainer,
                                         MetadataModel metadataModel,
                                         OnTextViewClickListener clickListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        TextView textView = getFlexTextView(context);

        textView.setEnabled(editable);
        if (editable) {
            textView.setBackgroundResource(R.drawable.shape_task_view_editable);
        } else {
            textView.setBackgroundResource(R.drawable.shape_task_view_no_editable);
        }

        String format = "";
        MetadataConfigDataModel configDataModel = metadataModel.getConfigData();
        if (configDataModel == null || StringUtils.isEmpty(configDataModel.format)) {
            format = DateFormatType.DATE_YMD_HMS;
        } else {
            format = configDataModel.getFormat();
        }

        // set data
        if (metadataModel.value instanceof OffsetDateTime date) {
            String initText = date.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
                    .replace("T", " ");
            textView.setText(initText);
        } else if (metadataModel.value instanceof String date) {
            Date date1 = TimeUtils.string2Date(date, DateFormatType.DATE_XXX);
            String initText = TimeUtils.date2String(date1, format);

            textView.setText(initText);
        }

        if (editable && clickListener != null) {
            textView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    clickListener.onClick(textView, metadataModel.key);
                }
            });
        }

        flexboxContainer.addView(textView);

        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    @Nullable
    private static GeoLocationModel parseGeoLocation(MetadataModel metadataModel, FileProfileConfigModel configModel) {
        if (configModel.getRecordResultMap().isEmpty()) {
            return null;
        }

        MetadataConfigDataModel model = metadataModel.getConfigData();
        if (StringUtils.isEmpty(model.geo_format)) {
            return null;
        }

        if (StringUtils.equals("lng_lat", model.geo_format)) {
            GeoLocationModel translatedGeoModel = checkLocationTranslated(configModel);
            if (metadataModel.value instanceof LinkedTreeMap<?, ?> map) {
                Object latObj = map.getOrDefault("lat", null);
                Object lngObj = map.getOrDefault("lng", null);
                if (latObj instanceof Double lat && lngObj instanceof Double lng) {
                    if (translatedGeoModel != null) {
                        translatedGeoModel.geo_format = model.geo_format;
                        translatedGeoModel.lat = lat;
                        translatedGeoModel.lng = lng;
                        return translatedGeoModel;
                    } else {
                        GeoLocationModel geoLocationModel = new GeoLocationModel();
                        geoLocationModel.geo_format = model.geo_format;
                        geoLocationModel.lat = lat;
                        geoLocationModel.lng = lng;
                        return geoLocationModel;
                    }
                }
            }
        }

        return null;
    }

    @Nullable
    private static GeoLocationModel checkLocationTranslated(FileProfileConfigModel configModel) {
        LinkedHashMap<String, Object> resultMap = configModel.getRecordResultMap();

        if (resultMap == null || !resultMap.containsKey("_location_translated")) {
            return null;
        }

        Object ltObj = resultMap.get("_location_translated");
        if (ltObj instanceof LinkedTreeMap<?, ?> tMap) {
            String address = getLinkedMapStringValue(tMap, "address");
            String street = getLinkedMapStringValue(tMap, "street");
            String district = getLinkedMapStringValue(tMap, "district");
            String city = getLinkedMapStringValue(tMap, "city");
            String province = getLinkedMapStringValue(tMap, "province");
            String country = getLinkedMapStringValue(tMap, "country");

            GeoLocationModel geoLocationModel = new GeoLocationModel();
            geoLocationModel.address = address;
            geoLocationModel.street = street;
            geoLocationModel.district = district;
            geoLocationModel.city = city;
            geoLocationModel.province = province;
            geoLocationModel.country = country;
            return geoLocationModel;
        }
        return null;
    }

    public static void buildEditableGeoLocation(Context context,
                                                boolean editable,
                                                LinearLayout parentContainer,
                                                MetadataModel metadataModel,
                                                FileProfileConfigModel configModel,
                                                OnViewClickListener clickListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        GeoLocationModel locationModel = parseGeoLocation(metadataModel, configModel);

        TextView textView = getFlexTextView(context);

        textView.setEnabled(editable);
        if (editable) {
            textView.setBackgroundResource(R.drawable.shape_task_view_editable);
        } else {
            textView.setBackgroundResource(R.drawable.shape_task_view_no_editable);
        }
        if (editable && clickListener != null) {
            textView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    clickListener.onClick(flexboxContainer, metadataModel.key);
                }
            });
        }

        if (locationModel != null) {
            textView.setText(locationModel.getText());
        } else {
            textView.setText(R.string.empty);
        }

        flexboxContainer.addView(textView);


        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }


    public static void buildEditableCollaborator(Context context,
                                                 boolean editable,
                                                 LinearLayout parentContainer,
                                                 MetadataModel metadataModel,
                                                 FileProfileConfigModel configModel,
                                                 OnViewClickListener onViewClickListener, OnItemRemoveListener<UserModel> onUserRemoveListener) {

        if (onUserRemoveListener == null) {
            throw new IllegalArgumentException("onUserRemoveListener cannot be null");
        }

        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        List<UserModel> userList = getUserList(metadataModel, configModel);
        if (!CollectionUtils.isEmpty(userList)) {
            for (int i = 0; i < userList.size(); i++) {
                UserModel userModel = userList.get(i);
                View v = buildCollaboratorView(context, editable, userModel.getAvatarUrl(), userModel.getName(), i, new OnViewClickListener() {
                    @Override
                    public void onClick(View view, String tag) {
                        onUserRemoveListener.onRemove(userModel);
                        flexboxContainer.removeView(view);
                    }
                });
                flexboxContainer.addView(v);
            }
        }

        if (editable && onViewClickListener != null) {
            ImageView add_imageView = new ImageView(context);
            add_imageView.setImageResource(R.drawable.icon_plus_sign);
            FlexboxLayout.LayoutParams flp = new FlexboxLayout.LayoutParams(Constants.DP.DP_24, Constants.DP.DP_24);
            add_imageView.setLayoutParams(flp);
            add_imageView.setPadding(DP_4,DP_4,DP_4,DP_4);
            add_imageView.setImageTintList(ColorStateList.valueOf(context.getColor(R.color.fancy_gray)));
            add_imageView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onViewClickListener.onClick(flexboxContainer, metadataModel.key);
                }
            });
            flexboxContainer.addView(add_imageView);

            //
            flexboxContainer.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onViewClickListener.onClick(v, metadataModel.key);
                }
            });
        }

        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    public static List<String> removeSpecialUser(MetadataModel metadataModel, UserModel userModel) {
        if (userModel == null) {
            return null;
        }

        boolean isList = metadataModel.value instanceof ArrayList;
        if (!isList) {
            return null;
        }

        List<String> userList = new ArrayList<>();
        List<?> arrayList = (List<?>) metadataModel.value;
        for (int i = 0; i < arrayList.size(); i++) {
            Object obj = arrayList.get(i);
            if (obj instanceof String strObj) {
                if (!strObj.equals(userModel.getEmail())) {
                    userList.add(strObj);
                }
            }
        }
        return userList;
    }

    public static List<UserModel> getUserList(MetadataModel metadataModel, FileProfileConfigModel configModel) {
        boolean isList = metadataModel.value instanceof ArrayList;
        if (!isList) {
            return null;
        }

        List<UserModel> userList = new ArrayList<>();

        List<?> arrayList = (List<?>) metadataModel.value;
        for (int i = 0; i < arrayList.size(); i++) {
            Object obj = arrayList.get(i);
            if (obj instanceof String strObj) {
                Optional<UserModel> op = configModel.getRelatedUserList()
                        .stream()
                        .filter(f -> f.getEmail().equals(strObj))
                        .findFirst();

                op.ifPresent(userList::add);
            }
        }

        return userList;
    }

    private static View buildCollaboratorView(Context context, boolean editable, String avatarUrl, String name, int index, OnViewClickListener onRemoveClickListener) {
        FlexboxLayout.LayoutParams flexLayoutParams = new FlexboxLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        flexLayoutParams.bottomMargin = DP_4;
        flexLayoutParams.rightMargin = DP_8;

        View ltr = LayoutInflater.from(context).inflate(R.layout.layout_avatar_username_round, null);
        TextView user_name_text_view = ltr.findViewById(R.id.user_name);
        ShapeableImageView imageView = ltr.findViewById(R.id.user_avatar);
        ltr.setTag(index);
        Glide.with(imageView)
                .load(GlideLoadConfig.getGlideUrl(avatarUrl))
                .apply(GlideLoadConfig.getOptions())
                .into(imageView);

        user_name_text_view.setText(name);
        if (editable && onRemoveClickListener != null) {
            ImageView removeView = ltr.findViewById(R.id.remove);
            removeView.setVisibility(View.VISIBLE);
            removeView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onRemoveClickListener.onClick(ltr, null);
                }
            });
        }

        ltr.setLayoutParams(flexLayoutParams);
        return ltr;
    }


    public static void buildEditableSingleSelect(Context context,
                                                 boolean editable,
                                                 LinearLayout parentContainer,
                                                 MetadataModel metadataModel,
                                                 OnSingleOptionChangedListener optionsChangedListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        MetadataConfigDataModel metadataConfigDataModel = metadataModel.getConfigData();
        if (metadataConfigDataModel == null) {
            return;
        }

        if (CollectionUtils.isEmpty(metadataConfigDataModel.options)) {
            return;
        }

        List<OptionTagModel> optionsList = metadataConfigDataModel.options;

        SupportMetadataRadioGroup radioGroup = new SupportMetadataRadioGroup(context);
        radioGroup.setKey(metadataModel.key);
        radioGroup.setEditable(editable);

        for (OptionTagModel optionsModel : optionsList) {
            radioGroup.addRadioView(optionsModel);
        }

        if (metadataModel.value instanceof String) {
            String initText = metadataModel.value.toString();
            radioGroup.select(initText);
        }

        // listener
        radioGroup.setChangedListener(optionsChangedListener);

        flexboxContainer.addView(radioGroup);


        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    public static void buildEditableMultiSelect(Context context,
                                                boolean editable,
                                                LinearLayout parentContainer,
                                                MetadataModel metadataModel,
                                                OnMultiOptionsChangedListener optionsChangedListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        MetadataConfigDataModel metadataConfigDataModel = metadataModel.getConfigData();
        if (metadataConfigDataModel == null) {
            return;
        }
        if (CollectionUtils.isEmpty(metadataConfigDataModel.options)) {
            return;
        }

        SupportMetadataCheckGroup checkGroup = new SupportMetadataCheckGroup(context);
        checkGroup.setEditable(editable);

        for (int i = 0; i < metadataConfigDataModel.options.size(); i++) {
            OptionTagModel optionsModel = metadataConfigDataModel.options.get(i);
            checkGroup.addCheckView(optionsModel);
        }

        if (metadataModel.value instanceof ArrayList<?> initList) {
            for (Object object : initList) {
                if (object instanceof String str) {
                    checkGroup.select(str);
                }
            }
        }

        // listener
        checkGroup.setChangedListener(optionsChangedListener);

        flexboxContainer.addView(checkGroup);

        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    public static void buildEditableCheckbox(Context context,
                                             boolean editable,
                                             LinearLayout parentContainer,
                                             MetadataModel metadataModel,
                                             CompoundButton.OnCheckedChangeListener onCheckedChangeListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;


        LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(DP_16, DP_16);

        AppCompatCheckBox checkBox = new AppCompatCheckBox(context);
        checkBox.setText("");
        checkBox.setLayoutParams(llp);
        checkBox.setEnabled(editable);
        boolean isChecked = false;
        if (metadataModel.value instanceof Boolean b) {
            isChecked = b;
        }
        checkBox.setChecked(isChecked);

        if (onCheckedChangeListener != null) {
            checkBox.setOnCheckedChangeListener(onCheckedChangeListener);
        }

        flexboxContainer.addView(checkBox);
        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    public static void buildEditableRate(Context context,
                                         boolean editable,
                                         LinearLayout parentContainer,
                                         MetadataModel metadataModel,
                                         OnRatingChangedListener onRatingChangedListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        MetadataConfigDataModel configDataModel = metadataModel.getConfigData();
        if (configDataModel == null) {
            return;
        }

        int rateNum = 0;
        if (metadataModel.value instanceof Number n) {
            rateNum = n.intValue();
        }

        SeaRatingBar seaRatingBar = new SeaRatingBar(context);
        seaRatingBar.setEnabled(editable);
        seaRatingBar.setStarHeight(Constants.DP.DP_24);
        seaRatingBar.setStarWidth(Constants.DP.DP_24);
        seaRatingBar.setStarTotal(configDataModel.rate_max_number);
        seaRatingBar.setStarPadding(DP_8);
        seaRatingBar.setRatingStatus(RatingStatus.Enable);

        int drawRes = R.drawable.icon_starred;
        //set empty
        Drawable emptyDrawable = ContextCompat.getDrawable(context, drawRes);
        if (emptyDrawable != null) {
            DrawableCompat.setTint(emptyDrawable, Color.rgb(229, 229, 229));
            seaRatingBar.setStarEmptyDrawable(emptyDrawable);
        }

        //set full
        Drawable fullDrawable = ContextCompat.getDrawable(context, drawRes);
        if (fullDrawable != null) {
            DrawableCompat.setTint(fullDrawable, Color.parseColor(configDataModel.rate_style_color));
            seaRatingBar.setStarFullDrawable(fullDrawable);
        }

        seaRatingBar.build();

        if (rateNum >= 0) {
            seaRatingBar.setRating(rateNum);
        }

        if (onRatingChangedListener != null) {
            seaRatingBar.setOnRatingChangedListener(onRatingChangedListener);
        }

        LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(-1, -2);
        seaRatingBar.setLayoutParams(llp);

        flexboxContainer.addView(seaRatingBar);
        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    //tag
    public static void buildEditableTag(Context context,
                                        boolean editable,
                                        LinearLayout parentContainer,
                                        MetadataModel metadataModel,
                                        FileProfileConfigModel configModel,
                                        OnViewClickListener onViewClickListener) {
        Pair<Boolean, FlexboxLayout> pair = getFlexContainer(context, parentContainer, metadataModel.key);
        FlexboxLayout flexboxContainer = pair.second;
        boolean isAdded = pair.first;

        LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(-2, -2);
        llp.rightMargin = Constants.DP.DP_8;
        llp.bottomMargin = Constants.DP.DP_8;

        if (configModel == null || configModel.getTagMap().isEmpty()) {
            return;
        }

        List<OptionTagModel> tagModels = convertLinkedTagToTagList(metadataModel, configModel);
        if (!CollectionUtils.isEmpty(tagModels)) {
            for (OptionTagModel tagModel : tagModels) {
                View ltr = LayoutInflater.from(context).inflate(R.layout.layout_detail_tag, null);
                ltr.findViewById(R.id.remove).setVisibility(editable ? View.VISIBLE : View.GONE);

                TextView textView = ltr.findViewById(R.id.text);
                textView.setText(tagModel.name);
                MaterialCardView materialCardView = ltr.findViewById(R.id.indicator);
                materialCardView.setCardBackgroundColor(Color.parseColor(tagModel.getColor()));
                ltr.setLayoutParams(llp);

                flexboxContainer.addView(ltr);
            }
        }

        if (editable && onViewClickListener != null) {
            ImageView add_imageView = new ImageView(context);
            add_imageView.setImageResource(R.drawable.icon_plus_sign);
            FlexboxLayout.LayoutParams flp = new FlexboxLayout.LayoutParams(Constants.DP.DP_24, Constants.DP.DP_24);
            add_imageView.setLayoutParams(flp);
            add_imageView.setPadding(DP_4,DP_4,DP_4,DP_4);
            add_imageView.setImageTintList(ColorStateList.valueOf(context.getColor(R.color.fancy_gray)));
            add_imageView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onViewClickListener.onClick(flexboxContainer, metadataModel.key);
                }
            });
            flexboxContainer.addView(add_imageView);

            //
            flexboxContainer.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onViewClickListener.onClick(v, metadataModel.key);
                }
            });
        }

        if (!isAdded) {
            parentContainer.addView(flexboxContainer);
        }
    }

    public static List<OptionTagModel> convertLinkedTagToTagList(MetadataModel metadataModel, FileProfileConfigModel configModel) {
        if (metadataModel == null || configModel == null) {
            return null;
        }

        return convertLinkedTagToTagList(metadataModel.value, configModel);
    }

    /**
     * In Activity, after executing some logic, the value of metadata will be set to the ArrayList<OptionTagModel> type.
     *
     */
    public static List<OptionTagModel> convertLinkedTagToTagList(Object value, FileProfileConfigModel configModel) {
        if (value == null || configModel == null) {
            return null;
        }

        boolean isArrayList = value instanceof ArrayList<?>;
        if (!isArrayList) {
            return null;
        }

        ArrayList<?> list = (ArrayList<?>) value;
        List<OptionTagModel> tags = new ArrayList<>();

        for (Object item : list) {
            if (item instanceof LinkedTreeMap<?, ?> map) {
                String rowId = map.get("row_id") != null ? map.get("row_id").toString() : null;
                OptionTagModel tagModel = configModel.getTagMap().get(rowId);
                if (tagModel != null) {
                    tags.add(tagModel);
                }
            } else if (item instanceof OptionTagModel m) {
                tags.add(m);
            }
        }

        return tags;
    }
}
