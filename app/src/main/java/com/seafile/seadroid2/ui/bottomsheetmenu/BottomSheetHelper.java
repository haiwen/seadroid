package com.seafile.seadroid2.ui.bottomsheetmenu;

import android.view.MenuItem;

import androidx.fragment.app.FragmentActivity;

import java.util.List;

public class BottomSheetHelper {

    public static BottomSheetMenuFragment.Builder buildSheet(FragmentActivity activity, int menuSheetId, OnMenuClickListener onMenuClickListener) {
        return new BottomSheetMenuFragment.Builder(activity)
                .setMenuSheetId(menuSheetId)
                .setColumnCount(1)
                .setCancelable(true)
                .setOnMenuClickListener(onMenuClickListener);
    }

    public static void showSheet(FragmentActivity activity, int menuSheetId, OnMenuClickListener onMenuClickListener) {
        new BottomSheetMenuFragment.Builder(activity)
                .setMenuSheetId(menuSheetId)
                .setColumnCount(1)
                .setCancelable(true)
                .setOnMenuClickListener(onMenuClickListener)
                .show(activity.getSupportFragmentManager());
    }

    public static void showSheet(FragmentActivity activity, List<MenuItem> menuItems, OnMenuClickListener onMenuClickListener) {
        if (menuItems == null || menuItems.isEmpty()) {
            return;
        }
        new BottomSheetMenuFragment.Builder(activity)
                .setMenuItems(menuItems)
                .setColumnCount(1)
                .setCancelable(true)
                .setOnMenuClickListener(onMenuClickListener)
                .show(activity.getSupportFragmentManager());
    }
}
