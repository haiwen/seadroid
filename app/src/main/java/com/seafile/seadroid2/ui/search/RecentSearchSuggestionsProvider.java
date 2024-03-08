package com.seafile.seadroid2.ui.search;

import android.app.SearchManager;
import android.content.ContentResolver;
import android.content.SearchRecentSuggestionsProvider;
import android.content.UriMatcher;
import android.database.Cursor;
import android.net.Uri;

import com.seafile.seadroid2.BuildConfig;

public class RecentSearchSuggestionsProvider extends SearchRecentSuggestionsProvider {
    public static final String AUTHORITY = BuildConfig.APPLICATION_ID + ".ui.search.RecentSearchSuggestionsProvider";
    public static final int MODE = DATABASE_MODE_QUERIES;

    private static final int SUGGESTIONS_CODE = 5;
    public static final Uri CONTENT_URI = Uri.parse("content://" + AUTHORITY);
    private UriMatcher matcher;

    public RecentSearchSuggestionsProvider() {
        matcher = new UriMatcher(UriMatcher.NO_MATCH);

        matcher.addURI(AUTHORITY, SearchManager.SUGGEST_URI_PATH_QUERY, SUGGESTIONS_CODE);
        setupSuggestions(AUTHORITY, MODE);
    }

}
