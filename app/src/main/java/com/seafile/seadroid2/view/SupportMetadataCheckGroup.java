package com.seafile.seadroid2.view;

import android.content.Context;
import android.graphics.Color;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.card.MaterialCardView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.listener.OnTaskViewOptionsChangedListener;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class SupportMetadataCheckGroup extends LinearLayout {

    private boolean editable = true;
    private final List<OptionTagModel> options = new ArrayList<>();
    private OnTaskViewOptionsChangedListener changedListener;

    public SupportMetadataCheckGroup(Context context) {
        super(context);
        init();
    }

    public SupportMetadataCheckGroup(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public SupportMetadataCheckGroup(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public SupportMetadataCheckGroup(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init();
    }

    private void init() {
        setOrientation(LinearLayout.VERTICAL);
        setLayoutParams(new FlexboxLayout.LayoutParams(-1, -2));
    }

    public void setEditable(boolean editable) {
        this.editable = editable;
    }

    public void setChangedListener(OnTaskViewOptionsChangedListener changedListener) {
        this.changedListener = changedListener;
    }

    public void addCheckView(OptionTagModel optionsModel) {
        options.add(optionsModel);

        View view = getCheckBoxSelectLayoutView(getContext(), optionsModel);
        CheckBox checkBox = view.findViewById(R.id.checkbox);
        checkBox.setEnabled(editable);

        view.setTag(optionsModel.name);
        checkBox.setTag(optionsModel.name);

        if (editable) {
            view.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View v) {
                    checkBox.toggle();
                }
            });

            checkBox.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                    checkSelectedOptions();
                }
            });
        }

        addView(view);
    }

    private void checkSelectedOptions() {
        if (changedListener != null) {
            changedListener.onChanged(getSelectedOptions());
        }
    }

    public void select(String current_tag) {
        int c = getChildCount();
        for (int i = 0; i < c; i++) {
            CheckBox checkbox = getChildAt(i).findViewById(R.id.checkbox);
            if (current_tag.equals(checkbox.getTag().toString())) {
                checkbox.setChecked(true);
            }
        }
    }

    public List<OptionTagModel> getSelectedOptions() {
        List<OptionTagModel> selected_options = new ArrayList<>();
        int c = getChildCount();
        for (int i = 0; i < c; i++) {
            CheckBox checkBox = getChildAt(i).findViewById(R.id.checkbox);
            if (checkBox.isChecked()) {
                String name = checkBox.getTag().toString();
                Optional<OptionTagModel> op = options
                        .stream()
                        .filter(f -> f.name.equals(name))
                        .findFirst();
                op.ifPresent(selected_options::add);
            }
        }
        return selected_options;
    }

    private View getCheckBoxSelectLayoutView(Context context, OptionTagModel optionsModel) {
        View ltr = LayoutInflater.from(context).inflate(R.layout.layout_check_box_text_round, null);
        FlexboxLayout.LayoutParams flp = new FlexboxLayout.LayoutParams(-1, -2);
        flp.topMargin = Constants.DP.DP_8;
        flp.rightMargin = Constants.DP.DP_8;
        ltr.setLayoutParams(flp);
        TextView textView = ltr.findViewById(R.id.text);
        MaterialCardView cardView = ltr.findViewById(R.id.card_view);
        textView.setTextColor(Color.parseColor(optionsModel.textColor));
        textView.setMaxLines(1);
        textView.setEllipsize(TextUtils.TruncateAt.END);
        textView.setText(optionsModel.name);
        cardView.setCardBackgroundColor(Color.parseColor(optionsModel.color));
        return ltr;
    }

}
