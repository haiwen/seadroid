package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.ScreenUtils;
import com.seafile.seadroid2.databinding.FragmentSelectorLongTextBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorBinding;
import com.seafile.seadroid2.ui.base.BaseActivity;

public class LongTextSelectorActivity extends BaseActivity {

    private FragmentSelectorLongTextBinding binding;
    private ToolbarActionbarForSelectorBinding toolbarBinding;

    private String inputValue, columnKey;

    public static Intent getIntent(Context context, String columnKey, String inputValue, String title) {
        Intent args = new Intent(context, LongTextSelectorActivity.class);
        args.putExtra("inputValue", inputValue);
        args.putExtra("columnKey", columnKey);
        args.putExtra("title", title);
        return args;
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        binding = FragmentSelectorLongTextBinding.inflate(getLayoutInflater());
        toolbarBinding = ToolbarActionbarForSelectorBinding.bind(binding.toolbar.getRoot());

        super.onCreate(savedInstanceState);
        setContentView(binding.getRoot());
        applyEdgeToEdge(binding.getRoot());

        if (getIntent() != null && getIntent().hasExtra("inputValue")) {
            inputValue = getIntent().getStringExtra("inputValue");
        }

        if (getIntent() != null && getIntent().hasExtra("columnKey")) {
            columnKey = getIntent().getStringExtra("columnKey");
        }

//        if (TextUtils.isEmpty(columnKey)) {
//            throw new IllegalArgumentException("no columnKey param");
//        }


        binding.editText.setHeight(ScreenUtils.getScreenHeight() / 2);

        if (!TextUtils.isEmpty(inputValue)) {
            binding.editText.setText(inputValue);
        }

        toolbarBinding.cancel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                finish();
            }
        });

        toolbarBinding.done.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                onDone();
            }
        });
    }

    private void onDone() {
        Intent intent = new Intent();
        intent.putExtra("columnKey", columnKey);
        intent.putExtra("longtext", binding.editText.getText().toString());
        setResult(RESULT_OK, intent);
        finish();
    }
}
