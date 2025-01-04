package com.seafile.seadroid2.widget.prefs;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.ContextThemeWrapper;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.preference.ListPreference;
import androidx.preference.PreferenceViewHolder;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.widget.prefs.background_pref.BackgroundListPreference;
import com.seafile.seadroid2.widget.prefs.simplemenu.SimpleMenuPopupWindow;


/**
 * A version of {@link ListPreference} that use Simple Menus in Material Design 1 as drop down.
 * <p>
 * On pre-Lollipop, it will fallback to {@link ListPreference}.
 */

public class SimpleMenuPreference extends BackgroundListPreference {

    private static boolean sLightFixEnabled = false;

    public static boolean isLightFixEnabled() {
        return sLightFixEnabled;
    }

    public static void setLightFixEnabled(boolean lightFixEnabled) {
        sLightFixEnabled = lightFixEnabled;
    }

    private View mAnchor;
    private View mItemView;
    private SimpleMenuPopupWindow mPopupWindow;

    public SimpleMenuPreference(Context context) {
        this(context, null);
    }

    @Override
    public int getLayoutId() {
        return R.layout.layout_pref_title_summary_empty;
    }

    public SimpleMenuPreference(Context context, AttributeSet attrs) {
        this(context, attrs, R.attr.simpleMenuPreferenceStyle);
    }

    public SimpleMenuPreference(Context context, AttributeSet attrs, int defStyle) {
        this(context, attrs, defStyle, R.style.Preference_SimpleMenuPreference);
    }

    public SimpleMenuPreference(Context context, AttributeSet attrs, int defStyleAttr,
                                int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);

        TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.SimpleMenuPreference, defStyleAttr, defStyleRes);

        int popupStyle = a.getResourceId(R.styleable.SimpleMenuPreference_android_popupMenuStyle, R.style.Widget_Preference_SimpleMenuPreference_PopupMenu);
        int popupTheme = a.getResourceId(R.styleable.SimpleMenuPreference_android_popupTheme, R.style.ThemeOverlay_Preference_SimpleMenuPreference_PopupMenu);
        Context popupContext;
        if (popupTheme != 0) {
            popupContext = new ContextThemeWrapper(context, popupTheme);
        } else {
            popupContext = context;
        }

        mPopupWindow = new SimpleMenuPopupWindow(popupContext, attrs, R.styleable.SimpleMenuPreference_android_popupMenuStyle, popupStyle);
        mPopupWindow.setOnItemClickListener(i -> {
            String value = getEntryValues()[i].toString();
            if (callChangeListener(value)) {
                setValue(value);
            }
        });

        a.recycle();
    }

    @Override
    protected void onClick() {

        if (getEntries() == null || getEntries().length == 0) {
            return;
        }

        if (mPopupWindow == null) {
            return;
        }

        mPopupWindow.setEntries(getEntries());
        mPopupWindow.setSelectedIndex(findIndexOfValue(getValue()));

        View container = (View) mItemView   // itemView
                .getParent();               // -> list (RecyclerView)

        mPopupWindow.show(mItemView, container, (int) mAnchor.getX());
    }

    @Override
    public void setEntries(@NonNull CharSequence[] entries) {
        super.setEntries(entries);

        mPopupWindow.requestMeasure();
    }

    @Override
    public void setValue(String value) {
        super.setValue(value);
    }

    @Override
    public void onBindViewHolder(PreferenceViewHolder view) {
        super.onBindViewHolder(view);

        mItemView = view.itemView;
        mAnchor = view.itemView.findViewById(android.R.id.empty);

        if (mAnchor == null) {
            throw new IllegalStateException("SimpleMenuPreference item layout must contain" +
                    "a view id is android.R.id.empty to support iconSpaceReserved");
        }
    }
}
