package com.seafile.seadroid2.ui.selector;

import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.CalendarView;
import android.widget.TimePicker;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.databinding.FragmentSelectorDateBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarForSelectorBinding;
import com.seafile.seadroid2.ui.base.BaseActivity;

import java.util.Calendar;
import java.util.Locale;

public class DateSelectorActivity extends BaseActivity {

    private FragmentSelectorDateBinding binding;
    private ToolbarActionbarForSelectorBinding toolbarBinding;

    private String columnKey, format, title;

    private boolean isSelectHourMinute = false;
    private int y, mon, dofM, hofD, min;

    public static Intent getIntent(Context context, String columnKey, String format, String title) {
        Intent args = new Intent(context, DateSelectorActivity.class);
        args.putExtra("columnKey", columnKey);
        args.putExtra("format", format);
        args.putExtra("title", title);
        return args;
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        binding = FragmentSelectorDateBinding.inflate(getLayoutInflater());
        toolbarBinding = ToolbarActionbarForSelectorBinding.bind(binding.toolbar.getRoot());

        super.onCreate(savedInstanceState);

        setContentView(binding.getRoot());

        applyEdgeToEdge(binding.getRoot());

        if (getIntent() != null) {
            columnKey = getIntent().getStringExtra("columnKey");
            format = getIntent().getStringExtra("format");
            title = getIntent().getStringExtra("title");
        }

        if (TextUtils.isEmpty(format)) {
            format = "yyyy-MM-dd";
        }


        binding.clear.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                clear();
            }
        });

        toolbarBinding.title.setText(title);
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

        init();
    }

    private void init() {
        if (format.toLowerCase(Locale.getDefault()).contains("h:mm")) {
            isSelectHourMinute = true;
        }

        if (!isSelectHourMinute) {
            binding.datePickerTime.setVisibility(View.GONE);
            binding.pickerTime.setVisibility(View.GONE);
        } else if (format.contains("HH:") || format.contains("H:")) {
            binding.pickerTime.setIs24HourView(true);
        }


        Calendar calendar = Calendar.getInstance();
        binding.pickerCalendar.setDate(calendar.getTimeInMillis());

        if (isSelectHourMinute) {
            binding.pickerTime.setHour(calendar.get(binding.pickerTime.is24HourView() ? Calendar.HOUR_OF_DAY : Calendar.HOUR));
            binding.pickerTime.setMinute(calendar.get(Calendar.MINUTE));
        }

        y = calendar.get(Calendar.YEAR);
        mon = calendar.get(Calendar.MONTH);
        dofM = calendar.get(Calendar.DAY_OF_MONTH);
        hofD = calendar.get(Calendar.HOUR_OF_DAY);
        min = calendar.get(Calendar.MINUTE);

        updateText();

        binding.pickerCalendar.setOnDateChangeListener(new CalendarView.OnDateChangeListener() {
            @Override
            public void onSelectedDayChange(@NonNull CalendarView view, int year, int month, int dayOfMonth) {
                y = year;
                mon = month;
                dofM = dayOfMonth;

                updateText();
            }
        });

        binding.pickerTime.setOnTimeChangedListener(new TimePicker.OnTimeChangedListener() {
            @Override
            public void onTimeChanged(TimePicker view, int hourOfDay, int minute) {
                hofD = hourOfDay;
                min = minute;

                updateText();
            }
        });
    }

    private void updateText() {
        Calendar c = Calendar.getInstance();
        c.set(y, mon, dofM, hofD, min);
        long date = c.getTimeInMillis();
        if (isSelectHourMinute) {
            String s = TimeUtils.millis2String(date, format);

            String[] ss = s.split(" ");
            binding.datePickerDate.setText(ss[0]);
            binding.datePickerTime.setText(ss[1]);
        } else {
            String s = TimeUtils.millis2String(date, format);
            binding.datePickerDate.setText(s);
        }
    }

    private String getDate() {
        Calendar c = Calendar.getInstance();
        c.set(y, mon, dofM, hofD, min);
        long date = c.getTimeInMillis();
        return TimeUtils.millis2String(date, format);
    }

    private void clear() {
        binding.datePickerDate.setText("");
        binding.datePickerTime.setText("");
    }

    private void onDone() {
        Intent intent = new Intent();
        intent.putExtra("columnKey", columnKey);
        intent.putExtra("date", getDate());
        setResult(RESULT_OK, intent);
        finish();
    }
}
