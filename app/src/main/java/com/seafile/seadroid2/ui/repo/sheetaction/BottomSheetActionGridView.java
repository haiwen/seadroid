package com.seafile.seadroid2.ui.repo.sheetaction;

import android.app.Activity;
import android.content.res.ColorStateList;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.view.ViewTreeObserver;
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
    private static final int MIN_THUMB_WIDTH_DP = 8;
    private static final int MAX_ITEM_WIDTH_DP = 96;

    private final Activity context;
    private View floatingView;
    private HorizontalScrollView scrollView;
    private GridLayout menuGrid;
    private View indicatorContainer;
    private View indicatorThumb;
    private ViewTreeObserver scrollObserver;
    private ViewTreeObserver.OnScrollChangedListener scrollChangedListener;
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

        if (context.isFinishing() || context.isDestroyed()) {
            return;
        }

        if (floatingView != null && floatingView.isAttachedToWindow()) {
            int savedScrollX = scrollView == null ? 0 : scrollView.getScrollX();
            buildGrid(menuItems);
            if (scrollView != null) {
                scrollView.post(() -> {
                    scrollView.scrollTo(savedScrollX, 0);
                    updateScrollIndicator();
                });
            }
            return;
        }

        // A view that is no longer attached must be detached before a new one is
        // inflated, otherwise it stays parented and is never garbage collected.
        detachFloatingView();

        floatingView = LayoutInflater.from(context).inflate(
                R.layout.layout_bottom_sheet_menu_view_grid, null, false);
        scrollView = floatingView.findViewById(R.id.menu_scroll_view);
        menuGrid = floatingView.findViewById(R.id.menu_grid);
        indicatorContainer = floatingView.findViewById(R.id.scroll_indicator_container);
        indicatorThumb = floatingView.findViewById(R.id.scroll_indicator_thumb);

        buildGrid(menuItems);

        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        params.gravity = Gravity.BOTTOM;
        floatingView.setLayoutParams(params);

        FrameLayout content = context.getWindow().getDecorView()
                .findViewById(android.R.id.content);
        if (content == null) {
            detachFloatingView();
            return;
        }

        content.addView(floatingView);
        if (scrollChangedListener == null) {
            scrollChangedListener = this::updateScrollIndicator;
        }
        // Read the observer after attachment so this listener belongs to the
        // window observer that receives scroll changes.
        scrollObserver = scrollView.getViewTreeObserver();
        scrollObserver.addOnScrollChangedListener(scrollChangedListener);

        applyEdgeToEdge(floatingView);
        ViewCompat.requestApplyInsets(floatingView);
        // Do not rely on post() alone: it can run before the first layout pass, when
        // the grid still has no measured width.
        updateIndicatorAfterLayout();
        scrollView.post(this::updateScrollIndicator);
    }

    private void updateIndicatorAfterLayout() {
        if (menuGrid == null) {
            return;
        }
        menuGrid.addOnLayoutChangeListener(new View.OnLayoutChangeListener() {
            @Override
            public void onLayoutChange(View v, int left, int top, int right, int bottom,
                                       int oldLeft, int oldTop, int oldRight, int oldBottom) {
                v.removeOnLayoutChangeListener(this);
                updateScrollIndicator();
            }
        });
    }

    private void buildGrid(List<MenuItem> menuItems) {
        if (scrollView == null || menuGrid == null) {
            return;
        }

        int availableWidth = BottomSheetActionLayoutHelper.getAvailableWidth(context);
        int visibleColumnCount = BottomSheetActionLayoutHelper.getColumnCount(
                context, availableWidth);
        int rowCount = menuItems.size() <= visibleColumnCount ? 1 : MAX_ROW_COUNT;
        int desiredColumnCount = (menuItems.size() + rowCount - 1) / rowCount;
        int totalColumnCount = Math.max(visibleColumnCount, desiredColumnCount);
        int columnsToFit = Math.min(visibleColumnCount, desiredColumnCount);

        // Share the available width between the columns the menu actually needs, so a
        // short menu is not squeezed into the left-most columns of a wide window.
        int itemWidth = Math.min(
                BottomSheetActionLayoutHelper.getItemWidth(availableWidth, columnsToFit),
                dp(MAX_ITEM_WIDTH_DP));

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
            // Let the cell grow with its content: a fixed square clips the label on
            // narrow screens or with large font scales.
            params.height = ViewGroup.LayoutParams.WRAP_CONTENT;
            menuGrid.addView(itemView, params);
        }

        // Centre the row when the items do not fill the available width.
        int gridWidth = desiredColumnCount * itemWidth;
        int sidePadding = Math.max(0, (availableWidth - gridWidth) / 2);
        if (scrollView.getPaddingLeft() != sidePadding
                || scrollView.getPaddingRight() != sidePadding) {
            scrollView.setPaddingRelative(sidePadding, scrollView.getPaddingTop(),
                    sidePadding, scrollView.getPaddingBottom());
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
        if (scrollView == null || indicatorContainer == null
                || indicatorThumb == null || menuGrid == null) {
            return;
        }

        int viewportWidth = scrollView.getWidth();
        int contentWidth = menuGrid.getWidth();
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
                trackWidth * viewportWidth / (2f * contentWidth)));
        int travel = Math.max(0, trackWidth - thumbWidth);

        // Only write the width back when it actually changed: this runs on every
        // scroll frame, and setLayoutParams() requests a layout pass each time.
        ViewGroup.LayoutParams params = indicatorThumb.getLayoutParams();
        if (params.width != thumbWidth) {
            params.width = thumbWidth;
            indicatorThumb.setLayoutParams(params);
        }

        float ratio = Math.min(1f, Math.max(0f,
                scrollView.getScrollX() / (float) scrollableWidth));
        boolean rtl = ViewCompat.getLayoutDirection(indicatorContainer)
                == ViewCompat.LAYOUT_DIRECTION_RTL;
        indicatorThumb.setX(travel * (rtl ? 1f - ratio : ratio));
    }

    private int dp(int value) {
        return Math.round(value * context.getResources().getDisplayMetrics().density);
    }

    public void dismiss() {
        detachFloatingView();
    }

    private void detachFloatingView() {
        // The ViewTreeObserver belongs to the window, not to this view, so the
        // listener has to be removed explicitly or it keeps this instance (and the
        // Activity it holds) alive across every show/dismiss cycle.
        removeScrollListener();

        if (floatingView != null) {
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

    private void removeScrollListener() {
        if (scrollObserver != null && scrollChangedListener != null
                && scrollObserver.isAlive()) {
            scrollObserver.removeOnScrollChangedListener(scrollChangedListener);
        }
        scrollObserver = null;
        scrollChangedListener = null;
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
