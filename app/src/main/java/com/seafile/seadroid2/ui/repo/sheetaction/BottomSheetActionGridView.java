package com.seafile.seadroid2.ui.repo.sheetaction;

import android.app.Activity;
import android.content.res.ColorStateList;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;
import android.widget.GridLayout;
import android.widget.HorizontalScrollView;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.listener.OnBottomSheetItemClickListener;

import java.util.List;

/** Two-row, continuously scrolling action menu backed by a GridLayout. */
public class BottomSheetActionGridView {
    private static final int MAX_ROW_COUNT = 2;
    private static final int MIN_THUMB_WIDTH_DP = 16;

    private final Activity context;
    private View floatingView;
    private HorizontalScrollView scrollView;
    private GridLayout menuGrid;
    private View indicatorContainer;
    private View indicatorThumb;
    private OnBottomSheetItemClickListener itemClickListener;

    public BottomSheetActionGridView(@NonNull Activity context) {
        this.context = context;
    }

    public void setOnItemClickListener(OnBottomSheetItemClickListener listener) {
        itemClickListener = listener;
    }

    public void show(List<MenuItem> menuItems) {
        if (CollectionUtils.isEmpty(menuItems)) {
            dismiss();
            return;
        }

        if (floatingView != null && floatingView.isAttachedToWindow()) {
            buildGrid(menuItems);
            scrollView.post(this::updateScrollIndicator);
            return;
        }

        floatingView = LayoutInflater.from(context).inflate(
                R.layout.layout_bottom_sheet_menu_view_grid, null, false);
        scrollView = floatingView.findViewById(R.id.menu_scroll_view);
        menuGrid = floatingView.findViewById(R.id.menu_grid);
        indicatorContainer = floatingView.findViewById(R.id.scroll_indicator_container);
        indicatorThumb = floatingView.findViewById(R.id.scroll_indicator_thumb);

        buildGrid(menuItems);
        scrollView.getViewTreeObserver().addOnScrollChangedListener(
                this::updateScrollIndicator);

        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        params.gravity = Gravity.BOTTOM;
        floatingView.setLayoutParams(params);

        FrameLayout content = context.getWindow().getDecorView()
                .findViewById(android.R.id.content);
        content.addView(floatingView);
        ViewCompat.requestApplyInsets(floatingView);
        applyEdgeToEdge(floatingView);
        scrollView.post(this::updateScrollIndicator);
    }

    private void buildGrid(List<MenuItem> menuItems) {
        int availableWidth = BottomSheetActionLayoutHelper.getAvailableWidth(context);
        int visibleColumnCount = BottomSheetActionLayoutHelper.getColumnCount(
                context, availableWidth);
        int itemWidth = BottomSheetActionLayoutHelper.getItemWidth(
                availableWidth, visibleColumnCount);
        int rowCount = menuItems.size() <= visibleColumnCount ? 1 : MAX_ROW_COUNT;
        int totalColumnCount = Math.max(visibleColumnCount,
                (menuItems.size() + rowCount - 1) / rowCount);

        menuGrid.removeAllViews();
        menuGrid.setRowCount(rowCount);
        menuGrid.setColumnCount(totalColumnCount);

        for (int index = 0; index < menuItems.size(); index++) {
            MenuItem menuItem = menuItems.get(index);
            View itemView = LayoutInflater.from(context).inflate(
                    R.layout.bottom_sheet_item_grid, menuGrid, false);
            bindMenuItem(itemView, menuItem);

            int column = index / rowCount;
            int row = index % rowCount;
            GridLayout.LayoutParams params = new GridLayout.LayoutParams(
                    GridLayout.spec(row), GridLayout.spec(column));
            params.width = itemWidth;
            params.height = itemWidth;
            menuGrid.addView(itemView, params);
        }
    }

    private void bindMenuItem(View itemView, MenuItem menuItem) {
        ImageView icon = itemView.findViewById(R.id.icon);
        TextView text = itemView.findViewById(R.id.text);
        icon.setImageDrawable(menuItem.getIcon());
        text.setText(menuItem.getTitle());

        int color = ContextCompat.getColor(context, menuItem.isEnabled()
                ? R.color.bottom_sheet_pop_enable_color
                : R.color.bottom_sheet_pop_disable_color);
        icon.setImageTintList(ColorStateList.valueOf(color));
        text.setTextColor(color);
        text.setEnabled(menuItem.isEnabled());
        itemView.setClickable(menuItem.isEnabled());
        itemView.setEnabled(menuItem.isEnabled());
        if (menuItem.isEnabled()) {
            itemView.setOnClickListener(v -> {
                if (itemClickListener != null) {
                    itemClickListener.onItemClick(menuItem);
                }
            });
        }
    }

    private void updateScrollIndicator() {
        if (scrollView == null || indicatorContainer == null || indicatorThumb == null) {
            return;
        }

        int viewportWidth = scrollView.getWidth();
        int contentWidth = menuGrid == null ? 0 : menuGrid.getWidth();
        int scrollableWidth = contentWidth - viewportWidth;
        if (viewportWidth <= 0 || scrollableWidth <= 0) {
            indicatorContainer.setVisibility(View.GONE);
            return;
        }

        indicatorContainer.setVisibility(View.VISIBLE);
        int trackWidth = indicatorContainer.getWidth();
        if (trackWidth <= 0) {
            return;
        }

        int thumbWidth = Math.max(dp(MIN_THUMB_WIDTH_DP), Math.round(
                trackWidth * viewportWidth / (float) contentWidth));
        int travel = Math.max(0, trackWidth - thumbWidth);
        int left = Math.round(travel * scrollView.getScrollX()
                / (float) scrollableWidth);

        ViewGroup.LayoutParams params = indicatorThumb.getLayoutParams();
        params.width = thumbWidth;
        indicatorThumb.setLayoutParams(params);
        indicatorThumb.setTranslationX(left);
    }

    private int dp(int value) {
        return Math.round(value * context.getResources().getDisplayMetrics().density);
    }

    public void dismiss() {
        if (floatingView != null && floatingView.isAttachedToWindow()) {
            ViewParent parent = floatingView.getParent();
            if (parent instanceof ViewGroup) {
                ((ViewGroup) parent).removeView(floatingView);
            }
        }
        floatingView = null;
        scrollView = null;
        menuGrid = null;
        indicatorContainer = null;
        indicatorThumb = null;
    }

    private void applyEdgeToEdge(View view) {
        ViewCompat.setOnApplyWindowInsetsListener(view, (v, insets) -> {
            int bottomInset = insets.getInsets(
                    WindowInsetsCompat.Type.navigationBars()).bottom;
            FrameLayout.LayoutParams params = (FrameLayout.LayoutParams) view.getLayoutParams();
            params.bottomMargin = bottomInset;
            view.setLayoutParams(params);
            return insets;
        });
    }
}
