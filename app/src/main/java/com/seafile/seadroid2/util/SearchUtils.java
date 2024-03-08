package com.seafile.seadroid2.util;

import android.app.Activity;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.TextView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.DrawableRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.appcompat.app.AppCompatActivity;

import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.search.SearchBar;
import com.google.android.material.search.SearchView;
import com.seafile.seadroid2.R;

public class SearchUtils {
    public static void setUpSearchBar(@NonNull Activity activity, @NonNull SearchBar searchBar) {
        searchBar.inflateMenu(R.menu.menu_search);
        searchBar.setOnMenuItemClickListener(
                menuItem -> {
                    ToastUtils.showLong(menuItem.getTitle());
                    return true;
                });
    }

    public static void setUpSearchView(
            @NonNull AppCompatActivity activity,
            @NonNull SearchBar searchBar,
            @NonNull SearchView searchView) {



        OnBackPressedCallback onBackPressedCallback =
                new OnBackPressedCallback(/* enabled= */ false) {
                    @Override
                    public void handleOnBackPressed() {
                        searchView.hide();
                    }
                };

        activity.getOnBackPressedDispatcher().addCallback(activity, onBackPressedCallback);
        searchView.addTransitionListener((searchView1, previousState, newState) ->
                onBackPressedCallback.setEnabled(newState == SearchView.TransitionState.SHOWN));
    }

    private static void submitSearchQuery(SearchBar searchBar, SearchView searchView, String query) {
        searchBar.setText(query);
        searchView.hide();
    }

    public static void startOnLoadAnimation(@NonNull SearchBar searchBar, @Nullable Bundle bundle) {
        // Don't start animation on rotation. Only needed in demo because minIntervalSeconds is 0.
        if (bundle == null) {
            searchBar.startOnLoadAnimation();
        }
    }

    public static void setUpSuggestions(
            @NonNull ViewGroup suggestionContainer,
            @NonNull SearchBar searchBar,
            @NonNull SearchView searchView) {
//        addSuggestionTitleView(suggestionContainer, R.string.cat_searchview_suggestion_section_title_yesterday);
//        addSuggestionItemViews(suggestionContainer, getYesterdaySuggestions(), searchBar, searchView);
//
//        addSuggestionTitleView(
//                suggestionContainer, R.string.cat_searchview_suggestion_section_title_this_week);
//        addSuggestionItemViews(suggestionContainer, getThisWeekSuggestions(), searchBar, searchView);
    }

    private static void addSuggestionTitleView(ViewGroup parent, @StringRes int titleResId) {

        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_search_suggestion, parent, false);
        TextView titleView = (TextView) view.findViewById(R.id.cat_searchbar_suggestion_title);
        titleView.setText(titleResId);
        parent.addView(titleView);
    }

//    private static void addSuggestionItemViews(
//            ViewGroup parent,
//            List<SuggestionItem> suggestionItems,
//            SearchBar searchBar,
//            SearchView searchView) {
//        for (SuggestionItem suggestionItem : suggestionItems) {
//            addSuggestionItemView(parent, suggestionItem, searchBar, searchView);
//        }
//    }

    private static class SuggestionItem {
        @DrawableRes
        private final int iconResId;
        private final String title;
        private final String subtitle;

        private SuggestionItem(int iconResId, String title, String subtitle) {
            this.iconResId = iconResId;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
