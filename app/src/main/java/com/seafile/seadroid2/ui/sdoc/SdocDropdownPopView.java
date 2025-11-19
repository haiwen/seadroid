package com.seafile.seadroid2.ui.sdoc;

import static android.view.View.VISIBLE;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.PopupWindow;

import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.enums.TextTypeEnum;
import com.seafile.seadroid2.framework.model.sdoc.TextTypeModel;
import com.seafile.seadroid2.listener.Callback;

import java.util.MissingFormatArgumentException;

public class SdocDropdownPopView extends PopupWindow {

    private TextTypeModel selectedTextTypeModel = null;
    private final int w;

    public int getW() {
        return w;
    }

    public SdocDropdownPopView(Context context) {
        super(context);
        w = SizeUtils.dp2px(180);


        View popView = LayoutInflater.from(context).inflate(R.layout.layout_sdoc_dropdown_pop, null);
        ViewGroup.LayoutParams params = new ViewGroup.LayoutParams(w, ViewGroup.LayoutParams.WRAP_CONTENT);
        popView.setLayoutParams(params);

        setContentView(popView);


        this.setWidth(w);
        this.setHeight(ViewGroup.LayoutParams.WRAP_CONTENT);

        this.setFocusable(true);
        this.setElevation(8);
        this.setOutsideTouchable(true);
        this.setFocusable(true);
        this.setBackgroundDrawable(ContextCompat.getDrawable(getContentView().getContext(), R.color.white));

        //
        initView(popView);
    }

    public void setSelectedTextStyleModel(TextTypeModel selectedTextTypeModel) {
        this.selectedTextTypeModel = selectedTextTypeModel;

        // default
        if (this.selectedTextTypeModel == null) {
            this.selectedTextTypeModel = new TextTypeModel(TextTypeEnum.paragraph.name());
        }

        initViewData();
    }

    private Callback<TextTypeModel> callback = null;

    public void setCallback(Callback<TextTypeModel> callback) {
        this.callback = callback;
    }

    private LinearLayout paragraphLayout, titleLayout, subtitleLayout, h1Layout, h2Layout, h3Layout, h4Layout, h5Layout, h6Layout;

    private ImageView paragraphIcon, titleIcon, subtitleIcon, h1Icon, h2Icon, h3Icon, h4Icon, h5Icon, h6Icon;

    private void initView(View view) {
        paragraphLayout = view.findViewById(R.id.dropdown_paragraph_container);
        titleLayout = view.findViewById(R.id.dropdown_title_container);
        subtitleLayout = view.findViewById(R.id.dropdown_subtitle_container);
        h1Layout = view.findViewById(R.id.dropdown_heading_1_container);
        h2Layout = view.findViewById(R.id.dropdown_heading_2_container);
        h3Layout = view.findViewById(R.id.dropdown_heading_3_container);
        h4Layout = view.findViewById(R.id.dropdown_heading_4_container);
        h5Layout = view.findViewById(R.id.dropdown_heading_5_container);
        h6Layout = view.findViewById(R.id.dropdown_heading_6_container);

        paragraphIcon = view.findViewById(R.id.dropdown_paragraph_icon);
        titleIcon = view.findViewById(R.id.dropdown_title_icon);
        subtitleIcon = view.findViewById(R.id.dropdown_subtitle_icon);
        h1Icon = view.findViewById(R.id.dropdown_heading_1_icon);
        h2Icon = view.findViewById(R.id.dropdown_heading_2_icon);
        h3Icon = view.findViewById(R.id.dropdown_heading_3_icon);
        h4Icon = view.findViewById(R.id.dropdown_heading_4_icon);
        h5Icon = view.findViewById(R.id.dropdown_heading_5_icon);
        h6Icon = view.findViewById(R.id.dropdown_heading_6_icon);

        paragraphLayout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(paragraphLayout.getId());
                dismiss();
            }
        });
        titleLayout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(titleLayout.getId());
                dismiss();
            }
        });
        subtitleLayout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(subtitleLayout.getId());
                dismiss();
            }
        });
        h1Layout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(h1Layout.getId());
                dismiss();
            }
        });
        h2Layout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(h2Layout.getId());
                dismiss();
            }
        });
        h3Layout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(h3Layout.getId());
                dismiss();
            }
        });
        h4Layout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(h4Layout.getId());
                dismiss();
            }
        });
        h5Layout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(h5Layout.getId());
                dismiss();
            }
        });
        h6Layout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                goneAllIcon();
                onContainerClick(h6Layout.getId());
                dismiss();
            }
        });

    }

    private void goneAllIcon() {
        paragraphIcon.setVisibility(View.INVISIBLE);
        titleIcon.setVisibility(View.INVISIBLE);
        subtitleIcon.setVisibility(View.INVISIBLE);
        h1Icon.setVisibility(View.INVISIBLE);
        h2Icon.setVisibility(View.INVISIBLE);
        h3Icon.setVisibility(View.INVISIBLE);
        h4Icon.setVisibility(View.INVISIBLE);
        h5Icon.setVisibility(View.INVISIBLE);
        h6Icon.setVisibility(View.INVISIBLE);
    }

    private void onContainerClick(long clickedId) {
        TextTypeModel m = null;
        if (clickedId == paragraphLayout.getId()) {
            paragraphIcon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.paragraph.name());
        } else if (clickedId == titleLayout.getId()) {
            titleIcon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.title.name());
        } else if (clickedId == subtitleLayout.getId()) {
            subtitleIcon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.subtitle.name());
        } else if (clickedId == h1Layout.getId()) {
            h1Icon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.header1.name());
        } else if (clickedId == h2Layout.getId()) {
            h2Icon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.header2.name());
        } else if (clickedId == h3Layout.getId()) {
            h3Icon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.header3.name());
        } else if (clickedId == h4Layout.getId()) {
            h4Icon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.header4.name());
        } else if (clickedId == h5Layout.getId()) {
            h5Icon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.header5.name());
        } else if (clickedId == h6Layout.getId()) {
            h6Icon.setVisibility(VISIBLE);
            m = new TextTypeModel(TextTypeEnum.header6.name());
        }

        if (callback != null) {
            callback.callback(m);
        }
    }


    private void initViewData() {

        goneAllIcon();

        if (selectedTextTypeModel == null) {
            throw new MissingFormatArgumentException("selectedTextStyleModel is null");
        }

        if (TextUtils.equals("paragraph", selectedTextTypeModel.type)) {
            paragraphIcon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("title", selectedTextTypeModel.type)) {
            titleIcon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("subtitle", selectedTextTypeModel.type)) {
            subtitleIcon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("h1", selectedTextTypeModel.type)) {
            h1Icon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("h2", selectedTextTypeModel.type)) {
            h2Icon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("h3", selectedTextTypeModel.type)) {
            h3Icon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("h4", selectedTextTypeModel.type)) {
            h4Icon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("h5", selectedTextTypeModel.type)) {
            h5Icon.setVisibility(VISIBLE);
        } else if (TextUtils.equals("h6", selectedTextTypeModel.type)) {
            h6Icon.setVisibility(VISIBLE);
        }

    }
}
