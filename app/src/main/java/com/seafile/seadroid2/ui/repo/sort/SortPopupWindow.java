package com.seafile.seadroid2.ui.repo.sort;

import android.content.Context;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.PopupWindow;

import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.annotation.Todo;

import java.util.ArrayList;
import java.util.List;

@Todo
public class SortPopupWindow {

    private final Context context;

    private PopupWindow popupWindow;

    private final List<SortMenuItem> items = new ArrayList<>();

    public SortPopupWindow(Context context) {

        this.context = context;

        items.add(new SortMenuItem(context.getString(R.string.menu_action_view_list), true,0));
        items.add(new SortMenuItem(context.getString(R.string.menu_action_view_list), true,0));
        items.add(new SortMenuItem(context.getString(R.string.menu_action_view_list), true,0));
        items.add(new SortMenuItem(context.getString(R.string.menu_action_view_list), true,0));
        items.add(new SortMenuItem(context.getString(R.string.menu_action_view_list), true,0));
    }

    public void show(View anchor) {

        View root = LayoutInflater.from(context).inflate(R.layout.layout_menu_popup_sort, null);

        RecyclerView recyclerView = root.findViewById(R.id.recyclerView);

        recyclerView.setLayoutManager(new LinearLayoutManager(context));

        SortMenuAdapter adapter =
                new SortMenuAdapter(
                        items,
                        position -> {

                            updateChecked(position);

                            recyclerView.getAdapter()
                                    .notifyDataSetChanged();

                            popupWindow.dismiss();
                        });

        recyclerView.setAdapter(adapter);

        popupWindow = new PopupWindow(root, dp(220), -2, true);

        popupWindow.setOutsideTouchable(true);

        popupWindow.setElevation(dp(8));

        popupWindow.showAsDropDown(anchor);
    }

    private void updateChecked(int position) {

        for (int i = 0; i < items.size(); i++) {
            items.get(i).checked = i == position;
        }
    }

    private int dp(int value) {

        return (int) TypedValue.applyDimension(
                TypedValue.COMPLEX_UNIT_DIP,
                value,
                context.getResources().getDisplayMetrics()
        );
    }
}