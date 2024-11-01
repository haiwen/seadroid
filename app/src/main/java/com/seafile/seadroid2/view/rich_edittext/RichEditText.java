package com.seafile.seadroid2.view.rich_edittext;

import android.content.Context;
import android.net.Uri;
import android.text.InputFilter;
import android.text.Spanned;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.LinearLayout;

import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.Glide;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.LayoutUploadFileBinding;
import com.seafile.seadroid2.view.MaxHeightScrollView;

import java.util.ArrayList;
import java.util.List;

public class RichEditText extends MaxHeightScrollView {
    public int DP_2 = Constants.DP.DP_2;
    public int DP_4 = Constants.DP.DP_4;
    public int DP_8 = Constants.DP.DP_8;
    public int DP_16 = Constants.DP.DP_16;
    public int DP_32 = Constants.DP.DP_32;


    private static final int VIEW_TAG_VALUE_IMAGE_URI = 0x2000002;
    private static final int VIEW_TAG_VALUE_IMAGE_URL = 0x2000003;

    private static final String VIEW_TAG_KV_INPUT = "input";
    private static final String VIEW_TAG_KV_IMAGE = "image";

    private LinearLayout container;
    private LayoutInflater inflater;
    private OnKeyListener keyListener;
    private OnRichAtListener onRichAtListener;

    private OnClickListener onCloseClickListener;

    private OnFocusChangeListener focusListener;

    private OnRichImageClickListener onRichImageStatusChangeListener;

    private EditText lastFocusEdit;

    private int removingImageIndex = 0;

    public RichEditText(Context context) {
        super(context);

        init(context);
    }

    public RichEditText(Context context, AttributeSet attrs) {
        super(context, attrs);

        init(context);
    }

    public RichEditText(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    private void init(Context context) {
        inflater = LayoutInflater.from(context);

        initLayoutView(context);

        initListener();

        initFirstEditText();
    }

    private void initLayoutView(Context context) {
        container = new LinearLayout(context);
        container.setOrientation(LinearLayout.VERTICAL);

        LayoutParams layoutParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
        addView(container, layoutParams);
    }

    private void initFirstEditText() {
        LinearLayout.LayoutParams firstEditParam = new LinearLayout.LayoutParams(-1, -2);
        EditText firstEdit = buildEditText();

        container.addView(firstEdit, firstEditParam);
        lastFocusEdit = firstEdit;
    }

    private void initListener() {
        keyListener = (v, keyCode, event) -> {
            if (event.getAction() == KeyEvent.ACTION_DOWN && event.getKeyCode() == KeyEvent.KEYCODE_DEL) {
                EditText edit = (EditText) v;
                onBackspacePress(edit);
            }
            return false;
        };

        //
        onCloseClickListener = v -> {
            if (v.getId() == R.id.remove) {
                FrameLayout parentView = (FrameLayout) v.getParent();
                remove(parentView);
            }
        };

        //
        focusListener = (v, hasFocus) -> {
            if (hasFocus) {
                lastFocusEdit = (EditText) v;
            }
        };
    }


    private void onBackspacePress(EditText editText) {
        if (editText == null) {
            return;
        }

        int startSelection = editText.getSelectionStart();
        if (startSelection != 0) {
            return;
        }

        int indexOfChild = container.indexOfChild(editText);
        if (indexOfChild <= 0) {
            return;
        }

        View preChildView = container.getChildAt(indexOfChild - 1);
        if (null == preChildView) {
            return;
        }

        if (preChildView instanceof FrameLayout) {
            remove(preChildView);
        } else if (preChildView instanceof EditText) {
            String str1 = editText.getText().toString();

            EditText preEditText = (EditText) preChildView;
            String str2 = preEditText.getText().toString();

            container.removeView(editText);

            preEditText.setText(String.format("%s%s", str2, str1));
            preEditText.requestFocus();
            preEditText.setSelection(str2.length(), str2.length());

            lastFocusEdit = preEditText;
        }
    }

    public void remove(View view) {
        removingImageIndex = container.indexOfChild(view);
        container.removeView(view);
        mergeEditText();
    }

    public void setOnRichImageStatusChangeListener(OnRichImageClickListener l) {
        this.onRichImageStatusChangeListener = l;
    }


    public void setOnRichAtListener(OnRichAtListener onRichAtListener) {
        this.onRichAtListener = onRichAtListener;
    }

    public void insertImage(Uri uri) {
        if (null == uri) {
            return;
        }

        int lastEditIndex = container.indexOfChild(lastFocusEdit);
        if (lastEditIndex + 1 < container.getChildCount()) {
            View v = container.getChildAt(lastEditIndex + 1);
            if (v instanceof FrameLayout) {
                addImageViewAtIndex(lastEditIndex + 1, uri);
                addEditTextAtIndex(lastEditIndex + 2, "");
            } else {
                addImageViewAtIndex(lastEditIndex + 1, uri);
            }
        } else {
            addImageViewAtIndex(-1, uri);
            addEditTextAtIndex(-1, "");
        }
    }

    private void addEditTextAtIndex(final int index, CharSequence editStr) {
        EditText editText = buildEditText();
        editText.setText(editStr);

        if (index == -1) {
            container.addView(editText);
        } else {
            container.addView(editText, index);
        }

        lastFocusEdit = editText;
        lastFocusEdit.requestFocus();
        lastFocusEdit.setSelection(editStr.length(), editStr.length());
    }

    private void addImageViewAtIndex(final int index, final Uri uri) {
        LayoutUploadFileBinding uploadFileBinding = LayoutUploadFileBinding.inflate(inflater);
        uploadFileBinding.getRoot().setTag(VIEW_TAG_KV_IMAGE);
        uploadFileBinding.getRoot().setTag(VIEW_TAG_VALUE_IMAGE_URI, uri.toString());
        uploadFileBinding.remove.setOnClickListener(onCloseClickListener);
        uploadFileBinding.uploadImage.setOnClickListener(v -> {
            if (onRichImageStatusChangeListener != null) {
                onRichImageStatusChangeListener.onClick(uploadFileBinding.uploadImage, uri.toString());
            }
        });


        LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(SizeUtils.dp2px(96), SizeUtils.dp2px(96));
        llp.topMargin = DP_2;
        llp.bottomMargin = DP_2;
        uploadFileBinding.getRoot().setLayoutParams(llp);

        Glide.with(this)
                .load(uri)
                .centerCrop()
                .into(uploadFileBinding.uploadImage);

        if (index == -1) {
            container.addView(uploadFileBinding.getRoot());
        } else {
            container.addView(uploadFileBinding.getRoot(), index);
        }
    }

    private void mergeEditText() {
        try {
            View preView = container.getChildAt(removingImageIndex - 1);
            View nextView = container.getChildAt(removingImageIndex);
            if (preView instanceof EditText && nextView instanceof EditText) {
                EditText preEdit = (EditText) preView;

                EditText nextEdit = (EditText) nextView;
                String str1 = preEdit.getText().toString();
                String str2 = nextEdit.getText().toString();
                String mergeText = "";
                if (str2.length() > 0) {
                    mergeText = str1 + "\n" + str2;
                } else {
                    mergeText = str1;
                }

                container.removeView(nextEdit);
                preEdit.setText(mergeText);
                //设置光标的定位
                preEdit.requestFocus();
                preEdit.setSelection(str1.length(), str1.length());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void updateUploadState(String uri, String url) {
        int c = container.getChildCount();
        if (c == 0) {
            return;
        }
        View frame = null;
        for (int i = 0; i < c; i++) {
            String key = container.getChildAt(i).getTag().toString();
            if (!VIEW_TAG_KV_IMAGE.equals(key)) {
                continue;
            }
            String uriKey = container.getChildAt(i).getTag(VIEW_TAG_VALUE_IMAGE_URI).toString();
            if (!uriKey.equals(uri)) {
                continue;
            }
            frame = container.getChildAt(i);
            break;
        }

        if (null == frame) {
            return;
        }
        frame.setTag(VIEW_TAG_VALUE_IMAGE_URL, url);
        frame.findViewById(R.id.upload_progress_bar).setVisibility(View.GONE);
        frame.findViewById(R.id.remove).setVisibility(View.VISIBLE);
    }


    private EditText buildEditText() {
        EditText editText = new DeletableEditText(getContext());
        LayoutParams layoutParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
        editText.setLayoutParams(layoutParams);
        editText.setTextSize(16);
        editText.setCursorVisible(true);
        editText.setBackground(null);
        editText.setOnKeyListener(keyListener);
        editText.setOnFocusChangeListener(focusListener);
        editText.setPadding(0, DP_4, 0, DP_4);
        editText.setTag(VIEW_TAG_KV_INPUT);
        editText.setFilters(new InputFilter[]{new InputFilter() {
            @Override
            public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
                if (TextUtils.equals("@", source)) {
                    if (onRichAtListener != null) {
                        onRichAtListener.onCall(editText);
                    }
                }
                return null;
            }
        }});
        return editText;
    }

    public List<RichContentModel> buildRichEditData() {
        List<RichContentModel> dataList = new ArrayList<>();
        int num = container.getChildCount();
        for (int index = 0; index < num; index++) {
            View itemView = container.getChildAt(index);
            RichContentModel richContentModel = new RichContentModel();
            if (itemView instanceof EditText) {
                EditText item = (EditText) itemView;
                richContentModel.content = item.getText().toString();
                richContentModel.type = 0;
            } else if (itemView instanceof FrameLayout) {
                Object obj = itemView.getTag(VIEW_TAG_VALUE_IMAGE_URL);
                if (null == obj) {
                    return null;
                }

                richContentModel.content = obj.toString();
                richContentModel.type = 1;
            }
            if (!TextUtils.isEmpty(richContentModel.content)) {
                dataList.add(richContentModel);
            }
        }
        return dataList;
    }

    public void removeAllViews() {
        if (container != null) {
            container.removeAllViews();

            initFirstEditText();
        }
    }

    public static class RichContentModel{
        public RichContentModel() {
        }

        public RichContentModel(int type, String content) {
            this.type = type;
            this.content = content;
        }

        /**
         * content type, 0 is text, 1 is image
         */
        public int type = 0;
        public String content;
    }
}
