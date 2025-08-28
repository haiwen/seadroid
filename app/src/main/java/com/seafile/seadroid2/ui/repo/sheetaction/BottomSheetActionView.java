package com.seafile.seadroid2.ui.repo.sheetaction;

import android.app.Activity;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.PagerSnapHelper;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetMenuAdapter;

import java.util.List;

public class BottomSheetActionView {


    private View floatingView;
    private BottomSheetMenuAdapter adapter;
    private Activity context;
    private OnBottomSheetItemClickListener itemClickListener;

    public interface OnBottomSheetItemClickListener {
        void onItemClick(MenuItem item);
    }

    public BottomSheetActionView(Activity context) {
        this.context = context;
    }

    public void setOnItemClickListener(OnBottomSheetItemClickListener listener) {
        this.itemClickListener = listener;
    }

    private final int columnsPerPage = 5;  // 每页的列数
    private final int rowCount = 2;        // 行数
    private final int itemsPerPage = columnsPerPage * rowCount;  // 每页的项目总数

    public void show(List<MenuItem> menuItems) {
        if (CollectionUtils.isEmpty(menuItems)) {
            dismiss();
            return;
        }

        if (floatingView != null && floatingView.isAttachedToWindow()) {
            adapter.submitList(menuItems);
            return;
        }

        // 创建视图
        floatingView = LayoutInflater.from(context).inflate(R.layout.layout_bottom_sheet_menu_view, null, false);

        RecyclerView rv = floatingView.findViewById(R.id.rv);
        GridLayoutManager fixedManager = new GridLayoutManager(context, rowCount, RecyclerView.HORIZONTAL, false);
        rv.setLayoutManager(fixedManager);

        PagerSnapHelper snapHelper = new PagerSnapHelper();
        snapHelper.attachToRecyclerView(rv);

        adapter = new BottomSheetMenuAdapter(columnsPerPage, true);
        adapter.setOnItemClickListener((baseQuickAdapter, view, i) -> {
            MenuItem item = adapter.getItem(i);
            if (item == null || !item.isEnabled()) {
                return;
            }

            if (item.getItemId() == R.id.more) {
                fixedManager.scrollToPosition(adapter.getItemCount() - 1);
                return;
            }

            if (itemClickListener != null) {
                itemClickListener.onItemClick(item);
            }
        });

        rv.setAdapter(adapter);

        adapter.submitList(menuItems);

        FrameLayout.LayoutParams p = new FrameLayout.LayoutParams(-1, -2);
        p.gravity = Gravity.BOTTOM;
        floatingView.setLayoutParams(p);


        View decorView = context.getWindow().getDecorView();
        FrameLayout content = decorView.findViewById(android.R.id.content);
        content.addView(floatingView);

        ViewCompat.requestApplyInsets(floatingView);

        applyEdgeToEdge(floatingView);

    }


    public void dismiss() {
        if (floatingView == null || !floatingView.isAttachedToWindow()) {
            floatingView = null;
            adapter = null;
            return;
        }

        ViewParent parent = floatingView.getParent();
        if (parent instanceof ViewGroup) {
            ((ViewGroup) parent).removeView(floatingView);
        }

        floatingView = null;
        adapter = null;
    }

    private void applyEdgeToEdge(View view) {
        ViewCompat.setOnApplyWindowInsetsListener(view, (v, insets) -> {
            int bottomInset = insets.getInsets(WindowInsetsCompat.Type.navigationBars()).bottom;
            FrameLayout.LayoutParams lp = (FrameLayout.LayoutParams) view.getLayoutParams();
            lp.bottomMargin = bottomInset;
            v.setLayoutParams(lp);
            return insets;
        });
    }

}
