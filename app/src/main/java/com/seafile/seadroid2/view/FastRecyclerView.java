package com.seafile.seadroid2.view;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.simplecityapps.recyclerview_fastscroll.views.FastScrollRecyclerView;

public class FastRecyclerView extends FastScrollRecyclerView {

    public FastRecyclerView(@NonNull Context context) {
        super(context);
        init();
    }

    public FastRecyclerView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public FastRecyclerView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    private void init() {
        setLayoutManager(new LinearLayoutManager(getContext()));
    }
}
