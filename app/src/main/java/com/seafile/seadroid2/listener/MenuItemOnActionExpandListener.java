package com.seafile.seadroid2.listener;

import android.view.Menu;
import android.view.MenuItem;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.R;

import java.util.HashMap;

public class MenuItemOnActionExpandListener implements MenuItem.OnActionExpandListener {
    private final Menu menu;
    private final HashMap<String, Boolean> menuIdState = new HashMap<>();

    public MenuItemOnActionExpandListener(Menu menu) {
        this.menu = menu;
    }

    @Override
    public boolean onMenuItemActionExpand(@NonNull MenuItem item) {

        // Save the state of Menu
        menuIdState.put("search", menu.findItem(R.id.menu_action_search).isVisible());
        menuIdState.put("sortGroup", menu.findItem(R.id.menu_action_sort).isVisible());
        menuIdState.put("createRepo", menu.findItem(R.id.create_repo).isVisible());
        menuIdState.put("add", menu.findItem(R.id.add).isVisible());
        menuIdState.put("select", menu.findItem(R.id.select).isVisible());

        // hide other menu items
        menu.findItem(R.id.menu_action_search).setVisible(false);
        menu.findItem(R.id.menu_action_sort).setVisible(false);
        menu.findItem(R.id.create_repo).setVisible(false);
        menu.findItem(R.id.add).setVisible(false);
        menu.findItem(R.id.select).setVisible(false);

        return true;
    }

    @Override
    public boolean onMenuItemActionCollapse(@NonNull MenuItem item) {

        menu.findItem(R.id.menu_action_search).setVisible(Boolean.TRUE.equals(menuIdState.get("search")));
        menu.findItem(R.id.menu_action_sort).setVisible(Boolean.TRUE.equals(menuIdState.get("sortGroup")));
        menu.findItem(R.id.create_repo).setVisible(Boolean.TRUE.equals(menuIdState.get("createRepo")));
        menu.findItem(R.id.add).setVisible(Boolean.TRUE.equals(menuIdState.get("add")));
        menu.findItem(R.id.select).setVisible(Boolean.TRUE.equals(menuIdState.get("select")));

        return true;
    }
}
