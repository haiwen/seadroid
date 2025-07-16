package com.seafile.seadroid2.ui;

import android.graphics.Typeface;
import android.os.Build;
import android.os.Bundle;
import android.view.MenuItem;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.Toolbar;

import com.seafile.seadroid2.databinding.ActivityBugHandlerBinding;
import com.seafile.seadroid2.ui.base.BaseActivity;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;


public class BugHandlerActivity extends BaseActivity {
    private ActivityBugHandlerBinding binding;
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityBugHandlerBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        applyEdgeToEdge(binding.getRoot());
        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle("Crash log");
        }

        String exception = getIntent().getStringExtra("exception_message");
        String threadName = getIntent().getStringExtra("thread_name");

        StringBuilder combinedTextBuilder = getStringBuilder(threadName, exception);

        binding.error.setTypeface(Typeface.MONOSPACE);
        binding.error.setText(combinedTextBuilder.toString());

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }


    @NonNull
    private StringBuilder getStringBuilder(String threadName, String exception) {
        String deviceBrand = Build.BRAND;
        String deviceModel = Build.MODEL;
        int sdkLevel = Build.VERSION.SDK_INT;
        Date currentDateTime = Calendar.getInstance().getTime();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
        String formattedDateTime = formatter.format(currentDateTime);
        StringBuilder combinedTextBuilder = new StringBuilder();
        combinedTextBuilder
                .append("Brand").append(':').append("     ")
                .append(deviceBrand).append('\n')
                .append("Model").append(':').append("     ")
                .append(deviceModel).append('\n')
                .append("SDK Level").append(':').append(' ').append(sdkLevel)
                .append('\n')
                .append("Thread").append(':').append("    ").append(threadName)
                .append('\n').append('\n').append('\n')
                .append("Time").append(':').append(' ')
                .append(formattedDateTime).append('\n')
                .append("--------- beginning of crash").append('\n')
                .append(exception);
        return combinedTextBuilder;
    }
}
