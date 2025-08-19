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
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.viewpager.widget.PagerAdapter;
import androidx.viewpager.widget.ViewPager;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.google.android.flexbox.FlexWrap;
import com.google.android.flexbox.FlexboxLayout;
import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;

import java.util.ArrayList;
import java.util.List;

public class BottomSheetActionViewPager {
    private Activity context;
    private final int ColumnCount = 5;
    private final int RowCount = 2;
    private final int PerPageItemCount = ColumnCount * RowCount;
    private final int screenWidth = ScreenUtils.getAppScreenWidth();
    private final int itemWidth = screenWidth / ColumnCount;
    private int currentPage = 0;

    public BottomSheetActionViewPager(Activity context) {
        this.context = context;
    }

    private View floatingView;

    public void show(List<MenuItem> menuItems) {
        if (CollectionUtils.isEmpty(menuItems)) {
            dismiss();
            return;
        }


        if (floatingView != null && floatingView.isAttachedToWindow()) {
//            adapter.submitList(menuItems);
            return;
        }

        int pageCount;
        if (menuItems.size() % PerPageItemCount == 0) {
            pageCount = menuItems.size() / PerPageItemCount;
        } else {
            pageCount = menuItems.size() / PerPageItemCount + 1;
        }

        List<View> views = new ArrayList<>();
        for (int i = 0; i < pageCount; i++) {
            int start = i * PerPageItemCount;
            int end = (i + 1) * PerPageItemCount;

            List<MenuItem> items;
            if (end > menuItems.size() - 1) {
                items = menuItems.subList(start, menuItems.size());
            } else {
                items = menuItems.subList(start, end);
            }

            if (CollectionUtils.isEmpty(items)) {
                continue;
            }
            FlexboxLayout flexboxLayout = new FlexboxLayout(context);
            flexboxLayout.setMaxLine(2);
            flexboxLayout.setFlexWrap(FlexWrap.WRAP);
            for (MenuItem menuItem : items) {
                View v = LayoutInflater.from(context).inflate(R.layout.bottom_sheet_item_grid, null, false);

                ImageView icon = v.findViewById(R.id.icon);
                TextView name = v.findViewById(R.id.text);

                icon.setImageDrawable(menuItem.getIcon());
                name.setText(menuItem.getTitle());

                name.setEnabled(menuItem.isEnabled());
                v.setClickable(menuItem.isEnabled());

                int color;
                if (menuItem.isEnabled()) {
                    color = ContextCompat.getColor(context, R.color.bottom_sheet_pop_enable_color);
                } else {
                    color = ContextCompat.getColor(context, R.color.bottom_sheet_pop_disable_color);
                }
                icon.setImageTintList(ColorStateList.valueOf(color));
                name.setTextColor(color);

                FlexboxLayout.LayoutParams fl = new FlexboxLayout.LayoutParams(itemWidth, itemWidth);
                flexboxLayout.addView(v, fl);
            }

            views.add(flexboxLayout);

        }

        floatingView = LayoutInflater.from(context).inflate(R.layout.layout_bottom_sheet_menu_view_pager, null, false);
        ViewPager pager = floatingView.findViewById(R.id.pager);
        pager.addOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {

            }

            @Override
            public void onPageSelected(int position) {
                currentPage = position;

                updatePageIndicator(currentPage, pageCount);
            }

            @Override
            public void onPageScrollStateChanged(int state) {

            }
        });

        pager.setAdapter(new PagerAdapter() {
            @Override
            public int getCount() {
                return pageCount;
            }

            @Override
            public boolean isViewFromObject(@NonNull View view, @NonNull Object object) {
                return view == object;
            }

            @NonNull
            @Override
            public Object instantiateItem(@NonNull ViewGroup container, int position) {
                View view = views.get(position);
                ViewGroup.LayoutParams vl = new ViewGroup.LayoutParams(itemWidth, itemWidth);
                container.addView(view, vl);
                return view;
            }

            @Override
            public void destroyItem(@NonNull ViewGroup container, int position, @NonNull Object object) {
                container.removeView((View) object);
            }
        });

        pager.setCurrentItem(currentPage);


        applyEdgeToEdge(floatingView);

        View decorView = context.getWindow().getDecorView();
        FrameLayout content = decorView.findViewById(android.R.id.content);


        FrameLayout.LayoutParams p = new FrameLayout.LayoutParams(-1, (int) (itemWidth * 2.5));
        p.gravity = Gravity.BOTTOM;
        content.addView(floatingView, p);

        updatePageIndicator(currentPage, pageCount);
    }


    private void updatePageIndicator(int currentPage, int totalPages) {
        LinearLayout indicatorLayout = floatingView.findViewById(R.id.page_indicator);
        indicatorLayout.removeAllViews();

        for (int i = 0; i < totalPages; i++) {
            ImageView dot = new ImageView(context);
            if (i == currentPage) {
                dot.setImageResource(R.drawable.indicator_selected);
            } else {
                dot.setImageResource(R.drawable.indicator_unselected);
            }

            int size = Constants.DP.DP_4;
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(size, size);
            params.setMargins(8, 0, 8, 0);

            indicatorLayout.addView(dot, params);
        }
    }

    public void dismiss() {
        if (floatingView == null || !floatingView.isAttachedToWindow()) {
            floatingView = null;
            return;
        }

        ViewParent parent = floatingView.getParent();
        if (parent instanceof ViewGroup) {
            ((ViewGroup) parent).removeView(floatingView);
        }

        floatingView = null;
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
