package com.seafile.seadroid2.widget.prefs.simplemenu;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Outline;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewOutlineProvider;
import android.widget.PopupWindow;
import android.widget.TextView;

import androidx.annotation.RequiresApi;
import androidx.annotation.RestrictTo;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.Arrays;

import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static androidx.annotation.RestrictTo.Scope.LIBRARY_GROUP_PREFIX;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.widget.prefs.SimpleMenuPreference;

/**
 * Extension of {@link PopupWindow} that implements Simple Menus in Material Design 1.
 */
@RestrictTo(LIBRARY_GROUP_PREFIX)
@RequiresApi(api = Build.VERSION_CODES.LOLLIPOP)
public class SimpleMenuPopupWindow extends PopupWindow {

    public static final int POPUP_MENU = 0;
    public static final int DIALOG = 1;

    public static final int HORIZONTAL = 0;
    public static final int VERTICAL = 1;

    public interface OnItemClickListener {
        void onClick(int i);
    }

    protected final int[] elevation = new int[2];
    protected final int[][] margin = new int[2][2];
    protected final int[][] listPadding = new int[2][2];
    protected final int itemHeight;
    protected final int dialogMaxWidth;
    protected final int unit;
    protected final int maxUnits;

    private int mMode = POPUP_MENU;

    private boolean mRequestMeasure = true;

    private final RecyclerView mList;
    private final SimpleMenuListAdapter mAdapter;

    private OnItemClickListener mOnItemClickListener;
    private CharSequence[] mEntries;
    private int mSelectedIndex;

    private int mMeasuredWidth;

    public SimpleMenuPopupWindow(Context context) {
        this(context, null);
    }

    public SimpleMenuPopupWindow(Context context, AttributeSet attrs) {
        this(context, attrs, R.styleable.SimpleMenuPreference_android_popupMenuStyle);
    }

    public SimpleMenuPopupWindow(Context context, AttributeSet attrs, int defStyleAttr) {
        this(context, attrs, defStyleAttr, R.style.Widget_Preference_SimpleMenuPreference_PopupMenu);
    }

    @SuppressLint("InflateParams")
    public SimpleMenuPopupWindow(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);

        setFocusable(true);
        setOutsideTouchable(false);

        TypedArray a = context.obtainStyledAttributes(
                attrs, R.styleable.SimpleMenuPopup, defStyleAttr, defStyleRes);

        elevation[POPUP_MENU] = (int) a.getDimension(R.styleable.SimpleMenuPopup_listElevation, 4f);
        elevation[DIALOG] = (int) a.getDimension(R.styleable.SimpleMenuPopup_dialogElevation, 48f);
        margin[POPUP_MENU][HORIZONTAL] = (int) a.getDimension(R.styleable.SimpleMenuPopup_listMarginHorizontal, 0);
        margin[POPUP_MENU][VERTICAL] = (int) a.getDimension(R.styleable.SimpleMenuPopup_listMarginVertical, 0);
        margin[DIALOG][HORIZONTAL] = (int) a.getDimension(R.styleable.SimpleMenuPopup_dialogMarginHorizontal, 0);
        margin[DIALOG][VERTICAL] = (int) a.getDimension(R.styleable.SimpleMenuPopup_dialogMarginVertical, 0);
        listPadding[POPUP_MENU][HORIZONTAL] = (int) a.getDimension(R.styleable.SimpleMenuPopup_listItemPadding, 0);
        listPadding[DIALOG][HORIZONTAL] = (int) a.getDimension(R.styleable.SimpleMenuPopup_dialogItemPadding, 0);
        dialogMaxWidth = (int) a.getDimension(R.styleable.SimpleMenuPopup_dialogMaxWidth, 0);
        unit = (int) a.getDimension(R.styleable.SimpleMenuPopup_unit, 0);
        maxUnits = a.getInteger(R.styleable.SimpleMenuPopup_maxUnits, 0);

        mList = (RecyclerView) LayoutInflater.from(context).inflate(R.layout.simple_menu_list, null);
        mList.setFocusable(true);
        mList.setLayoutManager(new LinearLayoutManager(context));
        mList.setItemAnimator(null);
        mList.setOutlineProvider(new ViewOutlineProvider() {
            @Override
            public void getOutline(View view, Outline outline) {
                getBackground().getOutline(outline);
            }
        });
        mList.setClipToOutline(true);

        setContentView(mList);

        mAdapter = new SimpleMenuListAdapter(this);
        mList.setAdapter(mAdapter);

        a.recycle();

        // TODO do not hardcode
        itemHeight = Math.round(context.getResources().getDisplayMetrics().density * 48);
        listPadding[POPUP_MENU][VERTICAL] = listPadding[DIALOG][VERTICAL] = Math.round(context.getResources().getDisplayMetrics().density * 8);
    }

    public OnItemClickListener getOnItemClickListener() {
        return mOnItemClickListener;
    }

    public void setOnItemClickListener(OnItemClickListener onItemClickListener) {
        mOnItemClickListener = onItemClickListener;
    }

    protected int getMode() {
        return mMode;
    }

    private void setMode(int mode) {
        mMode = mode;
    }

    protected CharSequence[] getEntries() {
        return mEntries;
    }

    public void setEntries(CharSequence[] entries) {
        mEntries = entries;
    }

    protected int getSelectedIndex() {
        return mSelectedIndex;
    }

    public void setSelectedIndex(int selectedIndex) {
        mSelectedIndex = selectedIndex;
    }

    @Override
    public RecyclerView getContentView() {
        return (RecyclerView) super.getContentView();
    }

    @Override
    public CustomBoundsDrawable getBackground() {
        Drawable background = super.getBackground();
        if (background != null
                && !(background instanceof CustomBoundsDrawable)) {
            setBackgroundDrawable(background);
        }
        return (CustomBoundsDrawable) super.getBackground();
    }

    @Override
    public void setBackgroundDrawable(Drawable background) {
        if (background == null) {
            throw new IllegalStateException("SimpleMenuPopupWindow must have a background");
        }

        if (!(background instanceof CustomBoundsDrawable)) {
            background = new CustomBoundsDrawable(background);
        }
        super.setBackgroundDrawable(background);
    }

    /**
     * Show the PopupWindow
     *
     * @param anchor      View that will be used to calc the position of windows
     * @param container   View that will be used to calc the position of windows
     * @param extraMargin extra margin start
     */
    public void show(View anchor, View container, int extraMargin) {
        int maxMaxWidth = container.getWidth() - margin[POPUP_MENU][HORIZONTAL] * 2;
        int measuredWidth = measureWidth(maxMaxWidth, mEntries);
        if (measuredWidth == -1) {
            setMode(DIALOG);
        } else if (measuredWidth != 0) {
            setMode(POPUP_MENU);

            mMeasuredWidth = measuredWidth;
        }

        mAdapter.notifyDataSetChanged();

        // clear last bounds
        Rect zeroRect = new Rect();
        getBackground().setCustomBounds(zeroRect);
        getContentView().invalidateOutline();

        if (mMode == POPUP_MENU) {
            showPopupMenu(anchor, container, mMeasuredWidth, extraMargin);

            if (SimpleMenuPreference.isLightFixEnabled()) {
                mList.post(() -> Light.resetLightCenterForPopupWindow(SimpleMenuPopupWindow.this));
            }
        } else {
            showDialog(anchor, container);
        }
    }

    /**
     * Show popup window in dialog mode
     *
     * @param parent    a parent view to get the {@link View#getWindowToken()} token from
     * @param container Container view that holds preference list, also used to calc width
     */
    private void showDialog(View parent, View container) {
        final int index = Math.max(0, mSelectedIndex);
        final int count = mEntries.length;

        getContentView().setOverScrollMode(View.OVER_SCROLL_IF_CONTENT_SCROLLS);
        getContentView().scrollToPosition(index);

        setWidth(Math.min(dialogMaxWidth, container.getWidth() - margin[DIALOG][HORIZONTAL] * 2));
        setHeight(WRAP_CONTENT);
        setAnimationStyle(R.style.Animation_Preference_SimpleMenuCenter);
        setElevation(elevation[DIALOG]);

        super.showAtLocation(parent, Gravity.CENTER_VERTICAL, 0, 0);

        getContentView().post(() -> {
            // disable over scroll when no scroll
            LinearLayoutManager lm = (LinearLayoutManager) getContentView().getLayoutManager();
            if (lm.findFirstCompletelyVisibleItemPosition() == 0
                    && lm.findLastCompletelyVisibleItemPosition() == count - 1) {
                getContentView().setOverScrollMode(View.OVER_SCROLL_NEVER);
            }

            int width = getContentView().getWidth();
            int height = getContentView().getHeight();
            Rect start = new Rect(width / 2, height / 2, width / 2, height / 2);

            SimpleMenuAnimation.startEnterAnimation(getBackground(), getContentView(),
                    width, height, width / 2, height / 2, start, itemHeight, elevation[DIALOG] / 4, index);
        });
    }

    /**
     * Show popup window in popup mode
     *
     * @param anchor    View that will be used to calc the position of the window
     * @param container Container view that holds preference list, also used to calc width
     * @param width     Measured width of this window
     */
    private void showPopupMenu(View anchor, View container, int width, int extraMargin) {
        final boolean rtl = container.getResources().getConfiguration().getLayoutDirection() == View.LAYOUT_DIRECTION_RTL;

        final int index = Math.max(0, mSelectedIndex);
        final int count = mEntries.length;

        final int anchorTop = anchor.getTop() - container.getPaddingTop();
        final int anchorHeight = anchor.getHeight();
        final int measuredHeight = itemHeight * count + listPadding[POPUP_MENU][VERTICAL] * 2;

        int[] location = new int[2];
        container.getLocationInWindow(location);

        final int containerTopInWindow = location[1] + container.getPaddingTop();
        final int containerHeight = container.getHeight() - container.getPaddingTop() - container.getPaddingBottom();

        int y;

        int height;
        int elevation = this.elevation[POPUP_MENU];
        int centerX = rtl
                ? location[0] + extraMargin - width + listPadding[POPUP_MENU][HORIZONTAL]
                : location[0] + extraMargin + listPadding[POPUP_MENU][HORIZONTAL];
        int centerY;
        int animItemHeight = itemHeight + listPadding[POPUP_MENU][VERTICAL] * 2;
        int animIndex = index;
        Rect animStartRect;

        if (measuredHeight > containerHeight) {
            // too high, use scroll
            y = containerTopInWindow + margin[POPUP_MENU][VERTICAL];

            // scroll to select item
            final int scroll = itemHeight * index
                    - anchorTop + listPadding[POPUP_MENU][VERTICAL] + margin[POPUP_MENU][VERTICAL]
                    - anchorHeight / 2 + itemHeight / 2;

            getContentView().post(() -> {
                getContentView().scrollBy(0, -measuredHeight); // to top
                getContentView().scrollBy(0, scroll);
            });
            getContentView().setOverScrollMode(View.OVER_SCROLL_IF_CONTENT_SCROLLS);

            height = containerHeight - margin[POPUP_MENU][VERTICAL] * 2;

            centerY = itemHeight * index;
        } else {
            // calc align to selected
            y = containerTopInWindow + anchorTop + anchorHeight / 2 - itemHeight / 2
                    - listPadding[POPUP_MENU][VERTICAL] - index * itemHeight;

            // make sure window is in parent view
            int maxY = containerTopInWindow + containerHeight
                    - measuredHeight - margin[POPUP_MENU][VERTICAL];
            y = Math.min(y, maxY);

            int minY = containerTopInWindow + margin[POPUP_MENU][VERTICAL];
            y = Math.max(y, minY);

            getContentView().setOverScrollMode(View.OVER_SCROLL_NEVER);

            height = measuredHeight;

            // center of selected item
            centerY = (int) (listPadding[POPUP_MENU][VERTICAL] + index * itemHeight + itemHeight * 0.5);
        }

        setWidth(width);
        setHeight(height);
        setElevation(elevation);
        setAnimationStyle(R.style.Animation_Preference_SimpleMenuCenter);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
            setEnterTransition(null);
            setExitTransition(null);
        }

        super.showAtLocation(anchor, Gravity.NO_GRAVITY, centerX, y);

        int startTop = centerY - (int) (itemHeight * 0.2);
        int startBottom = centerY + (int) (itemHeight * 0.2);
        int startLeft;
        int startRight;

        if (!rtl) {
            startLeft = centerX;
            startRight = centerX + unit;
        } else {
            startLeft = centerX + width - unit;
            startRight = centerX + width;
        }

        animStartRect = new Rect(startLeft, startTop, startRight, startBottom);

        int animElevation = (int) Math.round(elevation * 0.25);

        getContentView().post(() -> SimpleMenuAnimation.startEnterAnimation(getBackground(), getContentView(),
                width, height, centerX, centerY, animStartRect, animItemHeight, animElevation, animIndex));
    }

    /**
     * Request a measurement before next show, call this when entries changed.
     */
    public void requestMeasure() {
        mRequestMeasure = true;
    }

    /**
     * Measure window width
     *
     * @param maxWidth max width for popup
     * @param entries  Entries of preference hold this window
     * @return 0: skip
     * -1: use dialog
     * other: measuredWidth
     */
    private int measureWidth(int maxWidth, CharSequence[] entries) {
        // skip if should not measure
        if (!mRequestMeasure) {
            return 0;
        }

        mRequestMeasure = false;

        entries = Arrays.copyOf(entries, entries.length);

        Arrays.sort(entries, (o1, o2) -> o2.length() - o1.length());

        Context context = getContentView().getContext();
        int width = 0;

        maxWidth = Math.min(unit * maxUnits, maxWidth);

        Rect bounds = new Rect();

        TextView view = LayoutInflater.from(context).inflate(R.layout.simple_menu_item, null, false).findViewById(android.R.id.text1);
        Paint textPaint = view.getPaint();

        for (CharSequence chs : entries) {
            textPaint.getTextBounds(chs.toString(), 0, chs.toString().length(), bounds);

            width = Math.max(width, bounds.right + 1 + Math.round(listPadding[POPUP_MENU][HORIZONTAL] * 2 + 1));

            // more than one line should use dialog
            if (width > maxWidth
                    || chs.toString().contains("\n")) {
                return -1;
            }
        }

        // width is a multiple of a unit
        int w = 0;
        while (width > w) {
            w += unit;
        }

        return w;
    }

    @Override
    public void showAtLocation(View parent, int gravity, int x, int y) {
        throw new UnsupportedOperationException("use show(anchor) to show the window");
    }

    @Override
    public void showAsDropDown(View anchor) {
        throw new UnsupportedOperationException("use show(anchor) to show the window");
    }

    @Override
    public void showAsDropDown(View anchor, int xoff, int yoff) {
        throw new UnsupportedOperationException("use show(anchor) to show the window");
    }

    @Override
    public void showAsDropDown(View anchor, int xoff, int yoff, int gravity) {
        throw new UnsupportedOperationException("use show(anchor) to show the window");
    }
}
