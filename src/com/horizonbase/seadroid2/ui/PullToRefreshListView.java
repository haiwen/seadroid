package com.horizonbase.seadroid2.ui;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;
import android.view.animation.RotateAnimation;
import android.widget.*;
import android.widget.AbsListView.OnScrollListener;
import com.horizonbase.seadroid2.R;

/**
 * Custom pull to refresh widget
 *
 * @author liux (http://my.oschina.net/liux)
 * @version 1.0
 * @created 2012-3-21
 */
public class PullToRefreshListView extends ListView implements OnScrollListener {

    private final static String DEBGU_TAG = "PullToRefreshListView";

    // Pull to Refresh flag
    private final static int PULL_To_REFRESH = 0;
    // Release to refresh flag
    private final static int RELEASE_To_REFRESH = 1;
    // Refreshing flag
    private final static int REFRESHING = 2;
    // Done refreshing flag
    private final static int DONE = 3;

    private LayoutInflater inflater;

    private LinearLayout headView;
    private TextView tipsTextview;
    private TextView lastUpdatedTextView;
    private ImageView arrowImageView;
    private ProgressBar progressBar;
    // arrow animations of pull to refresh
    private RotateAnimation animation;
    private RotateAnimation reverseAnimation;

    // use to ensure the value of startY only record once during the whole touch event
    private boolean isRecored;

    private int headContentWidth;
    private int headContentHeight;
    private int headContentOriginalTopPadding;

    private int startY;
    private int firstItemIndex;
    private int currentScrollState;

    private int state;

    private boolean isBack;

    public OnRefreshListener refreshListener;

    public PullToRefreshListView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(context);
    }

    public PullToRefreshListView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init(context);
    }

    private void init(Context context) {
        // set animations
        animation = new RotateAnimation(0, -180,
                RotateAnimation.RELATIVE_TO_SELF, 0.5f,
                RotateAnimation.RELATIVE_TO_SELF, 0.5f);
        animation.setInterpolator(new LinearInterpolator());
        animation.setDuration(100);
        animation.setFillAfter(true);

        reverseAnimation = new RotateAnimation(-180, 0,
                RotateAnimation.RELATIVE_TO_SELF, 0.5f,
                RotateAnimation.RELATIVE_TO_SELF, 0.5f);
        reverseAnimation.setInterpolator(new LinearInterpolator());
        reverseAnimation.setDuration(100);
        reverseAnimation.setFillAfter(true);

        inflater = LayoutInflater.from(context);
        headView = (LinearLayout) inflater.inflate(R.layout.pull_to_refresh_head, null);

        arrowImageView = (ImageView) headView.findViewById(R.id.head_arrowImageView);
        arrowImageView.setMinimumWidth(50);
        arrowImageView.setMinimumHeight(50);
        progressBar = (ProgressBar) headView.findViewById(R.id.head_progressBar);
        tipsTextview = (TextView) headView.findViewById(R.id.head_tipsTextView);
        lastUpdatedTextView = (TextView) headView.findViewById(R.id.head_lastUpdatedTextView);

        headContentOriginalTopPadding = headView.getPaddingTop();

        measureView(headView);
        headContentHeight = headView.getMeasuredHeight();
        headContentWidth = headView.getMeasuredWidth();

        headView.setPadding(headView.getPaddingLeft(), -1 * headContentHeight, headView.getPaddingRight(), headView.getPaddingBottom());
        headView.invalidate();

        // System.out.println("initial height: " + headContentHeight); 
        // System.out.println("initial TopPad:" + headContentOriginalTopPadding);

        addHeaderView(headView);
        setOnScrollListener(this);
    }

    public void onScroll(AbsListView view, int firstVisiableItem, int visibleItemCount, int totalItemCount) {
        firstItemIndex = firstVisiableItem;
    }

    public void onScrollStateChanged(AbsListView view, int scrollState) {
        currentScrollState = scrollState;
    }

    public boolean onTouchEvent(MotionEvent event) {
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                if (firstItemIndex == 0 && !isRecored) {
                    startY = (int) event.getY();
                    isRecored = true;
                    // System.out.println("ACTION_DOWN-Y:" + startY);
                }
                break;

            case MotionEvent.ACTION_CANCEL:// lose focus & cancel refresh
            case MotionEvent.ACTION_UP:

                if (state != REFRESHING) {
                    if (state == DONE) {
                        // System.out.println("ACTION_UP:DONE");
                    } else if (state == PULL_To_REFRESH) {
                        state = DONE;
                        changeHeaderViewByState();
                        // System.out.println("ACTION_UP:PULL_To_REFRESH-->DONE");
                    } else if (state == RELEASE_To_REFRESH) {
                        state = REFRESHING;
                        changeHeaderViewByState();
                        onRefresh();
                        // System.out.println("ACTION_UP:RELEASE_To_REFRESH-->REFRESHING");
                    }
                }

                isRecored = false;
                isBack = false;

                break;

            case MotionEvent.ACTION_MOVE:
                int tempY = (int) event.getY();
                // System.out.println("ACTION_MOVE Y:"+tempY);
                if (!isRecored && firstItemIndex == 0) {
                    // System.out.println("Y:"+tempY);
                    isRecored = true;
                    startY = tempY;
                }
                if (state != REFRESHING && isRecored) {
                    // ready to refresh
                    if (state == RELEASE_To_REFRESH) {
                        if ((tempY - startY < headContentHeight + 20)
                                && (tempY - startY) > 0) {
                            state = PULL_To_REFRESH;
                            changeHeaderViewByState();
                            // System.out.println("ACTION_MOVE:RELEASE_To_REFRESH-->PULL_To_REFRESH");
                        } else if (tempY - startY <= 0) {
                            state = DONE;
                            changeHeaderViewByState();
                            // System.out.println("ACTION_MOVE:RELEASE_To_REFRESH-->DONE");
                        }
                        //
                        else {
                            //
                        }
                    } else if (state == PULL_To_REFRESH) {
                        if (tempY - startY >= headContentHeight + 20 && currentScrollState == SCROLL_STATE_TOUCH_SCROLL) {
                            state = RELEASE_To_REFRESH;
                            isBack = true;
                            changeHeaderViewByState();
                            // System.out.println("PULL_To_REFRESH-->RELEASE_To_REFRESH");
                        } else if (tempY - startY <= 0) {
                            state = DONE;
                            changeHeaderViewByState();
                            // System.out.println("PULL_To_REFRESH-->DONE");
                        }
                    } else if (state == DONE) {
                        if (tempY - startY > 0) {
                            state = PULL_To_REFRESH;
                            changeHeaderViewByState();
                            // System.out.println("DONE-->PULL_To_REFRESH");
                        }
                    }

                    if (state == PULL_To_REFRESH) {
                        int topPadding = (int) ((-1 * headContentHeight + (tempY - startY)));
                        headView.setPadding(headView.getPaddingLeft(), topPadding, headView.getPaddingRight(), headView.getPaddingBottom());
                        headView.invalidate();
                        // System.out.println("PULL_To_REFRESH-TopPad:" + topPadding);
                    }

                    if (state == RELEASE_To_REFRESH) {
                        int topPadding = (int) ((tempY - startY - headContentHeight));
                        headView.setPadding(headView.getPaddingLeft(), topPadding, headView.getPaddingRight(), headView.getPaddingBottom());
                        headView.invalidate();
                        // System.out.println("RELEASE_To_REFRESH-TopPad:" + topPadding);
                    }
                }
                break;
        }
        return super.onTouchEvent(event);
    }

    /**
     * call this method to update ui when state changes
     */
    private void changeHeaderViewByState() {
        switch (state) {
            case RELEASE_To_REFRESH:

                arrowImageView.setVisibility(View.VISIBLE);
                progressBar.setVisibility(View.GONE);
                tipsTextview.setVisibility(View.VISIBLE);
                lastUpdatedTextView.setVisibility(View.VISIBLE);

                arrowImageView.clearAnimation();
                arrowImageView.startAnimation(animation);

                tipsTextview.setText(R.string.pull_to_refresh_release_label);

                // Log.v(DEBGU_TAG, "state: release to refresh");
                break;
            case PULL_To_REFRESH:

                progressBar.setVisibility(View.GONE);
                tipsTextview.setVisibility(View.VISIBLE);
                lastUpdatedTextView.setVisibility(View.VISIBLE);
                arrowImageView.clearAnimation();
                arrowImageView.setVisibility(View.VISIBLE);
                if (isBack) {
                    isBack = false;
                    arrowImageView.clearAnimation();
                    arrowImageView.startAnimation(reverseAnimation);
                }
                tipsTextview.setText(R.string.pull_to_refresh_pull_label);

                // Log.v(DEBGU_TAG, "state: pull to refresh");
                break;

            case REFRESHING:
                // System.out.println("REFRESHING-TopPad:" + headContentOriginalTopPadding);
                headView.setPadding(headView.getPaddingLeft(), headContentOriginalTopPadding, headView.getPaddingRight(), headView.getPaddingBottom());
                headView.invalidate();

                progressBar.setVisibility(View.VISIBLE);
                arrowImageView.clearAnimation();
                arrowImageView.setVisibility(View.GONE);
                tipsTextview.setText(R.string.pull_to_refresh_refreshing_label);
                lastUpdatedTextView.setVisibility(View.GONE);

                // Log.v(DEBGU_TAG, "state: refreshing...");
                break;
            case DONE:
                // System.out.println("DONE-TopPad: " + (-1 * headContentHeight));
                headView.setPadding(headView.getPaddingLeft(), -1 * headContentHeight, headView.getPaddingRight(), headView.getPaddingBottom());
                headView.invalidate();

                progressBar.setVisibility(View.GONE);
                arrowImageView.clearAnimation();
                //  change icon
                arrowImageView.setImageResource(R.drawable.pulltorefresh_arrow);

                tipsTextview.setText(R.string.pull_to_refresh_pull_label);
                lastUpdatedTextView.setVisibility(View.VISIBLE);

                // Log.v(DEBGU_TAG, "state: done");
                break;
        }
    }

    // click to refresh
    public void clickRefresh() {
        setSelection(0);
        state = REFRESHING;
        changeHeaderViewByState();
        onRefresh();
    }

    public void setOnRefreshListener(OnRefreshListener refreshListener) {
        this.refreshListener = refreshListener;
    }

    public interface OnRefreshListener {
        public void onRefresh();
    }

    public void onRefreshComplete(String update) {
        lastUpdatedTextView.setText(update);
        onRefreshComplete();
    }

    public void onRefreshComplete() {
        state = DONE;
        changeHeaderViewByState();
    }

    private void onRefresh() {
        if (refreshListener != null) {
            refreshListener.onRefresh();
        }
    }

    // calculate width and height value of headview
    private void measureView(View child) {
        ViewGroup.LayoutParams p = child.getLayoutParams();
        if (p == null) {
            p = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.FILL_PARENT,
                    ViewGroup.LayoutParams.WRAP_CONTENT);
        }
        int childWidthSpec = ViewGroup.getChildMeasureSpec(0, 0 + 0, p.width);
        int lpHeight = p.height;
        int childHeightSpec;
        if (lpHeight > 0) {
            childHeightSpec = MeasureSpec.makeMeasureSpec(lpHeight,
                    MeasureSpec.EXACTLY);
        } else {
            childHeightSpec = MeasureSpec.makeMeasureSpec(0,
                    MeasureSpec.UNSPECIFIED);
        }
        child.measure(childWidthSpec, childHeightSpec);
    }

}
