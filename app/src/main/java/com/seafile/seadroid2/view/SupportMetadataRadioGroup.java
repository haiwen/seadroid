package com.seafile.seadroid2.view;

import android.content.Context;
import android.graphics.Color;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.CompoundButton;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.TextView;

import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.card.MaterialCardView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.listener.OnSingleOptionChangedListener;
import com.seafile.seadroid2.ui.file_profile.ColumnTypeUtils;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class SupportMetadataRadioGroup extends RadioGroup {

    private boolean editable = true;
    private String key;
    private final List<OptionTagModel> options = new ArrayList<>();
    private OnSingleOptionChangedListener changedListener;

    public void setEditable(boolean editable) {
        this.editable = editable;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public SupportMetadataRadioGroup(Context context) {
        super(context);
        init();
    }

    public SupportMetadataRadioGroup(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setLayoutParams(new FlexboxLayout.LayoutParams(-1, -2));
    }

    public void setChangedListener(OnSingleOptionChangedListener changedListener) {
        this.changedListener = changedListener;
    }

    public void addRadioView(OptionTagModel optionsModel) {
        options.add(optionsModel);

        View view = getCheckRadioSelectLayoutView(getContext(), optionsModel);
        RadioButton radioButton = view.findViewById(R.id.radio_button);
        radioButton.setEnabled(editable);

        view.setTag(optionsModel.name);
        radioButton.setTag(optionsModel.name);


        if (editable) {
            view.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View v) {
                    select(v.getTag().toString());
                }
            });

            radioButton.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View v) {
                    select(v.getTag().toString());
                }
            });

            radioButton.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                    if (isChecked) {
                        checkSelectedOptions();
                    }
                }
            });
        }

        addView(view);
    }

    private void checkSelectedOptions() {
        if (changedListener != null) {
            changedListener.onChanged(getSelectedOption());
        }
    }

    public OptionTagModel getSelectedOption() {
        int c = getChildCount();
        for (int i = 0; i < c; i++) {
            RadioButton radioButton = getChildAt(i).findViewById(R.id.radio_button);
            if (radioButton.isChecked()) {
                String name = radioButton.getTag().toString();
                Optional<OptionTagModel> op = options.stream()
                        .filter(f -> f.name.equals(name))
                        .findFirst();
                if (op.isPresent()) {
                    return op.get();
                }
            }
        }
        return null;
    }

    public void select(String current_tag) {
        int c = getChildCount();
        for (int i = 0; i < c; i++) {
            RadioButton radioButton = getChildAt(i).findViewById(R.id.radio_button);
            if (current_tag.equals(radioButton.getTag().toString())) {
                radioButton.toggle();
            } else {
                radioButton.setChecked(false);
            }
        }
    }

    private View getCheckRadioSelectLayoutView(Context context, OptionTagModel optionsModel) {
        View ltr = LayoutInflater.from(context).inflate(R.layout.layout_check_radio_text_round, null);
        LayoutParams flp = new LayoutParams(-1, -2);
        flp.topMargin = Constants.DP.DP_8;
        flp.rightMargin = Constants.DP.DP_8;
        ltr.setLayoutParams(flp);
        TextView textView = ltr.findViewById(R.id.text);
        MaterialCardView cardView = ltr.findViewById(R.id.card_view);
        textView.setTextColor(Color.parseColor(optionsModel.getTextColor()));
        textView.setMaxLines(1);
        textView.setEllipsize(TextUtils.TruncateAt.END);

        if (StringUtils.equals("_status", key)) {
            textView.setText(ColumnTypeUtils.getResNameByKey(optionsModel.name));
        }else{
            textView.setText(optionsModel.name);
        }

        cardView.setCardBackgroundColor(Color.parseColor(optionsModel.getColor()));
        return ltr;
    }
}
