package com.seafile.seadroid2.ui.bottomsheet;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.View;
import android.widget.TextView;

import com.seafile.seadroid2.R;

public class BottomSheetTextFragment extends BaseBottomSheetDialogFragment {

    private TextView textView;
    private String text;

    public static BottomSheetTextFragment newInstance(String text) {
        BottomSheetTextFragment fragment = new BottomSheetTextFragment();
        Bundle args = new Bundle();
        args.putString("text",text);
        fragment.setArguments(args);
        return fragment;
    }


    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (getArguments() != null) {
            text = getArguments().getString("text");
        }
    }

    @Override
    protected int getLayoutId() {
        return R.layout.dialog_bottom_sheet_text;
    }

    @Override
    protected int getCancelId() {
        return View.NO_ID;
    }

    @Override
    protected void initView() {
        textView = getRootView().findViewById(R.id.text);
    }
    
    public TextView getTextView() {
        return textView;
    }

    @Override
    protected void init() {
        this.textView.setText(text);
    }
}
