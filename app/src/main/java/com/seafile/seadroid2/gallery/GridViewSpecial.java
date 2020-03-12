package com.seafile.seadroid2.gallery;

import java.util.HashMap;
import java.lang.reflect.Field;

import android.app.Activity;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.media.AudioManager;
import android.os.Handler;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.view.GestureDetector;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewConfiguration;
import android.view.GestureDetector.SimpleOnGestureListener;
import android.widget.Scroller;
import static com.seafile.seadroid2.gallery.Util.assertTrue;

import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;


public class GridViewSpecial extends View {
    @SuppressWarnings("unused")
    private static final String TAG = "GridViewSpecial";
    private static final float MAX_FLING_VELOCITY = 2500;

    public static interface Listener {
        public void onImageClicked(int index);
        public void onImageTapped(int index);
        public void onLayoutComplete(boolean changed);

        /**
         * Invoked when the <code>GridViewSpecial</code> scrolls.
         *
         * @param scrollPosition the position of the scroller in the range
         *         [0, 1], when 0 means on the top and 1 means on the buttom
         */
        public void onScroll(float scrollPosition);
    }

    public static interface DrawAdapter {
        public void drawImage(Canvas canvas, IImage image,
                Bitmap b, int xPos, int yPos, int w, int h);
        public void drawDecoration(Canvas canvas, IImage image,
                int xPos, int yPos, int w, int h);
    }

    public static final int INDEX_NONE = -1;

    // There are two cell size we will use. It can be set by setSizeChoice().
    // The mLeftEdgePadding fields is filled in onLayout(). See the comments
    // in onLayout() for details.
    static class LayoutSpec {
        LayoutSpec(int w, int h, int intercellSpacing, int leftEdgePadding,
                DisplayMetrics metrics) {
            mCellWidth = dpToPx(w, metrics);
            mCellHeight = dpToPx(h, metrics);
            mCellSpacing = dpToPx(intercellSpacing, metrics);
            mLeftEdgePadding = dpToPx(leftEdgePadding, metrics);
        }
        int mCellWidth, mCellHeight;
        int mCellSpacing;
        int mLeftEdgePadding;
    }

    private LayoutSpec [] mCellSizeChoices;

    private void initCellSize() {
        Activity a = (Activity) getContext();
        DisplayMetrics metrics = new DisplayMetrics();
        a.getWindowManager().getDefaultDisplay().getMetrics(metrics);
        mCellSizeChoices = new LayoutSpec[] {
            new LayoutSpec(67, 67, 8, 0, metrics),
            new LayoutSpec(92, 92, 8, 0, metrics),
        };
    }

    // Converts dp to pixel.
    private static int dpToPx(int dp, DisplayMetrics metrics) {
        return (int) (metrics.density * dp);
    }

    // These are set in init().
    private final Handler mHandler = new Handler();
    private GestureDetector mGestureDetector;
    private ImageBlockManager mImageBlockManager;

    // These are set in set*() functions.
    private ImageLoader mLoader;
    private Listener mListener = null;
    private DrawAdapter mDrawAdapter = null;
    private IImageList mAllImages = ImageManager.makeEmptyImageList();
    private int mSizeChoice = 1;  // default is big cell size

    // These are set in onLayout().
    private LayoutSpec mSpec;
    private int mColumns;
    private int mMaxScrollY;

    // We can handle events only if onLayout() is completed.
    private boolean mLayoutComplete = false;

    // Selection state
    private int mCurrentSelection = INDEX_NONE;
    private int mCurrentPressState = 0;
    private static final int TAPPING_FLAG = 1;
    private static final int CLICKING_FLAG = 2;

    // These are cached derived information.
    private int mCount;  // Cache mImageList.getCount();
    private int mRows;  // Cache (mCount + mColumns - 1) / mColumns
    private int mBlockHeight; // Cache mSpec.mCellSpacing + mSpec.mCellHeight

    private boolean mRunning = false;
    private Scroller mScroller = null;

    public GridViewSpecial(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(context);
    }

    private void init(Context context) {
        setVerticalScrollBarEnabled(true);
        //initializeScrollbars(context.obtainStyledAttributes(
        //        R.styleable.GalleryScrollBar));
        mGestureDetector = new GestureDetector(context,
                new MyGestureDetector());
        setFocusableInTouchMode(true);
        initCellSize();
    }

    private final Runnable mRedrawCallback = new Runnable() {
                public void run() {
                    invalidate();
                }
            };

    public void setLoader(ImageLoader loader) {
        assertTrue(!mRunning);
        mLoader = loader;
    }

    public void setListener(Listener listener) {
        assertTrue(!mRunning);
        mListener = listener;
    }

    public void setDrawAdapter(DrawAdapter adapter) {
        assertTrue(!mRunning);
        mDrawAdapter = adapter;
    }

    public void setImageList(IImageList list) {
        assertTrue(!mRunning);
        mAllImages = list;
        mCount = mAllImages.getCount();
    }

    public void setSizeChoice(int choice) {
        assertTrue(!mRunning);
        if (mSizeChoice == choice) return;
        mSizeChoice = choice;
    }

    @Override
    public void onLayout(boolean changed, int left, int top,
                         int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);

        if (!mRunning) {
            return;
        }

        mSpec = mCellSizeChoices[mSizeChoice];

        int width = right - left;

        // The width is divided into following parts:
        //
        // LeftEdgePadding CellWidth (CellSpacing CellWidth)* RightEdgePadding
        //
        // We determine number of cells (columns) first, then the left and right
        // padding are derived. We make left and right paddings the same size.
        //
        // The height is divided into following parts:
        //
        // CellSpacing (CellHeight CellSpacing)+

        mColumns = 1 + (width - mSpec.mCellWidth)
                / (mSpec.mCellWidth + mSpec.mCellSpacing);

        mSpec.mLeftEdgePadding = (width
                - ((mColumns - 1) * mSpec.mCellSpacing)
                - (mColumns * mSpec.mCellWidth)) / 2;

        mRows = (mCount + mColumns - 1) / mColumns;
        mBlockHeight = mSpec.mCellSpacing + mSpec.mCellHeight;
        mMaxScrollY = mSpec.mCellSpacing + (mRows * mBlockHeight)
                - (bottom - top);

        // Put mScrollY in the valid range. This matters if mMaxScrollY is
        // changed. For example, orientation changed from portrait to landscape.
        int newScrollY = Math.max(0, Math.min(mMaxScrollY, getScrollY()));

        // XXX: horrible hack to access android SDK hidden protected var mScrollY
        try {
            Field scrollYField = View.class.getDeclaredField("mScrollY");
            scrollYField.setAccessible(true);
            scrollYField.set(this, newScrollY);
        } catch(Exception e) {
        }

        generateOutlineBitmap();

        if (mImageBlockManager != null) {
            mImageBlockManager.recycle();
        }

        mImageBlockManager = new ImageBlockManager(mHandler, mRedrawCallback,
                mAllImages, mLoader, mDrawAdapter, mSpec, mColumns, width,
                mOutline[OUTLINE_EMPTY]);

        mListener.onLayoutComplete(changed);

        moveDataWindow();

        mLayoutComplete = true;
    }

    @Override
    protected int computeVerticalScrollRange() {
        return mMaxScrollY + getHeight();
    }

    // We cache the three outlines from NinePatch to Bitmap to speed up
    // drawing. The cache must be updated if the cell size is changed.
    public static final int OUTLINE_EMPTY = 0;
    public static final int OUTLINE_PRESSED = 1;
    public static final int OUTLINE_SELECTED = 2;

    public Bitmap mOutline[] = new Bitmap[3];

    private void generateOutlineBitmap() {
        int w = mSpec.mCellWidth;
        int h = mSpec.mCellHeight;

        for (int i = 0; i < mOutline.length; i++) {
            mOutline[i] = Bitmap.createBitmap(w, h, Bitmap.Config.ARGB_8888);
        }

        Drawable cellOutline;
        cellOutline = GridViewSpecial.this.getResources()
                .getDrawable(android.R.drawable.gallery_thumb);
        cellOutline.setBounds(0, 0, w, h);
        Canvas canvas = new Canvas();

        canvas.setBitmap(mOutline[OUTLINE_EMPTY]);
        cellOutline.setState(EMPTY_STATE_SET);
        cellOutline.draw(canvas);

        canvas.setBitmap(mOutline[OUTLINE_PRESSED]);
        cellOutline.setState(
                PRESSED_ENABLED_FOCUSED_SELECTED_WINDOW_FOCUSED_STATE_SET);
        cellOutline.draw(canvas);

        canvas.setBitmap(mOutline[OUTLINE_SELECTED]);
        cellOutline.setState(ENABLED_FOCUSED_SELECTED_WINDOW_FOCUSED_STATE_SET);
        cellOutline.draw(canvas);
    }

    private void moveDataWindow() {
        // Calculate visible region according to scroll position.
        int startRow = (getScrollY() - mSpec.mCellSpacing) / mBlockHeight;
        int endRow = (getScrollY() + getHeight() - mSpec.mCellSpacing - 1)
                / mBlockHeight + 1;

        // Limit startRow and endRow to the valid range.
        // Make sure we handle the mRows == 0 case right.
        startRow = Math.max(Math.min(startRow, mRows - 1), 0);
        endRow = Math.max(Math.min(endRow, mRows), 0);
        mImageBlockManager.setVisibleRows(startRow, endRow);
    }

    // In MyGestureDetector we have to check canHandleEvent() because
    // GestureDetector could queue events and fire them later. At that time
    // stop() may have already been called and we can't handle the events.
    private class MyGestureDetector extends SimpleOnGestureListener {
        private AudioManager mAudioManager;

        @Override
        public boolean onDown(MotionEvent e) {
            if (!canHandleEvent()) return false;
            if (mScroller != null && !mScroller.isFinished()) {
                mScroller.forceFinished(true);
                return false;
            }
            int index = computeSelectedIndex(e.getX(), e.getY());
            if (index >= 0 && index < mCount) {
                setSelectedIndex(index);
            } else {
                setSelectedIndex(INDEX_NONE);
            }
            return true;
        }

        @Override
        public boolean onFling(MotionEvent e1, MotionEvent e2,
                float velocityX, float velocityY) {
            if (!canHandleEvent()) return false;
            if (velocityY > MAX_FLING_VELOCITY) {
                velocityY = MAX_FLING_VELOCITY;
            } else if (velocityY < -MAX_FLING_VELOCITY) {
                velocityY = -MAX_FLING_VELOCITY;
            }

            setSelectedIndex(INDEX_NONE);
            mScroller = new Scroller(getContext());
            mScroller.fling(0, getScrollY(), 0, -(int) velocityY, 0, 0, 0,
                    mMaxScrollY);
            computeScroll();

            return true;
        }

        @Override
        public void onLongPress(MotionEvent e) {
            if (!canHandleEvent()) return;
            onSingleTapConfirmed(e);
            // performLongClick();
        }

        @Override
        public boolean onScroll(MotionEvent e1, MotionEvent e2,
                                float distanceX, float distanceY) {
            if (!canHandleEvent()) return false;
            setSelectedIndex(INDEX_NONE);
            scrollBy(0, (int) distanceY);
            invalidate();
            return true;
        }

        @Override
        public boolean onSingleTapConfirmed(MotionEvent e) {
            if (!canHandleEvent()) return false;
            int index = computeSelectedIndex(e.getX(), e.getY());
            if (index >= 0 && index < mCount) {
                // Play click sound.
                if (mAudioManager == null) {
                    mAudioManager = (AudioManager) getContext()
                            .getSystemService(Context.AUDIO_SERVICE);
                }
                mAudioManager.playSoundEffect(AudioManager.FX_KEY_CLICK);

                // MultipleImageSelectionActivity.onImageTapped will call
                // invalidate() to force redraw
                mListener.onImageTapped(index);
                return true;
            }
            return false;
        }
    }

    public int getCurrentSelection() {
        return mCurrentSelection;
    }

    public void invalidateImage(int index) {
        if (index != INDEX_NONE) {
            mImageBlockManager.invalidateImage(index);
        }
    }

    /**
     *
     * @param index <code>INDEX_NONE</code> (-1) means remove selection.
     */
    public void setSelectedIndex(int index) {
        // A selection box will be shown for the image that being selected,
        // (by finger or by the dpad center key). The selection box can be drawn
        // in two colors. One color (yellow) is used when the the image is
        // still being tapped or clicked (the finger is still on the touch
        // screen or the dpad center key is not released). Another color
        // (orange) is used after the finger leaves touch screen or the dpad
        // center key is released.

        if (mCurrentSelection == index) {
            return;
        }
        // This happens when the last picture is deleted.
        mCurrentSelection = Math.min(index, mCount - 1);

        if (mCurrentSelection != INDEX_NONE) {
            ensureVisible(mCurrentSelection);
        }
        invalidate();
    }

    public void scrollToImage(int index) {
        Rect r = getRectForPosition(index);
        scrollTo(0, r.top);
    }

    public void scrollToVisible(int index) {
        Rect r = getRectForPosition(index);
        int top = getScrollY();
        int bottom = getScrollY() + getHeight();
        if (r.bottom > bottom) {
            scrollTo(0, r.bottom - getHeight());
        } else if (r.top < top) {
            scrollTo(0, r.top);
        }
    }

    private void ensureVisible(int pos) {
        Rect r = getRectForPosition(pos);
        int top = getScrollY();
        int bot = top + getHeight();

        if (r.bottom > bot) {
            mScroller = new Scroller(getContext());
            mScroller.startScroll(getScrollX(), getScrollY(), 0,
                    r.bottom - getHeight() - getScrollY(), 200);
            computeScroll();
        } else if (r.top < top) {
            mScroller = new Scroller(getContext());
            mScroller.startScroll(getScrollX(), getScrollY(), 0, r.top - getScrollY(), 200);
            computeScroll();
        }
    }

    public void start() {
        // These must be set before start().
        assertTrue(mLoader != null);
        assertTrue(mListener != null);
        assertTrue(mDrawAdapter != null);
        mRunning = true;
        requestLayout();
    }

    // If the the underlying data is changed, for example,
    // an image is deleted, or the size choice is changed,
    // The following sequence is needed:
    //
    // mGvs.stop();
    // mGvs.set...(...);
    // mGvs.set...(...);
    // mGvs.start();
    public void stop() {
        // Remove the long press callback from the queue if we are going to
        // stop.
        mHandler.removeCallbacks(mLongPressCallback);
        mScroller = null;
        if (mImageBlockManager != null) {
            mImageBlockManager.recycle();
            mImageBlockManager = null;
        }
        mRunning = false;
        mCurrentSelection = INDEX_NONE;
    }

    @Override
    public void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        if (!canHandleEvent()) return;
        mImageBlockManager.doDraw(canvas, getWidth(), getHeight(), getScrollY());
        paintDecoration(canvas);
        paintSelection(canvas);
        moveDataWindow();
    }

    @Override
    public void computeScroll() {
        if (mScroller != null) {
            boolean more = mScroller.computeScrollOffset();
            scrollTo(0, mScroller.getCurrY());
            if (more) {
                invalidate();  // So we draw again
            } else {
                mScroller = null;
            }
        } else {
            super.computeScroll();
        }
    }

    // Return the rectange for the thumbnail in the given position.
    Rect getRectForPosition(int pos) {
        int row = pos / mColumns;
        int col = pos - (row * mColumns);

        int left = mSpec.mLeftEdgePadding
                + (col * (mSpec.mCellWidth + mSpec.mCellSpacing));
        int top = row * mBlockHeight;

        return new Rect(left, top,
                left + mSpec.mCellWidth + mSpec.mCellSpacing,
                top + mSpec.mCellHeight + mSpec.mCellSpacing);
    }

    // Inverse of getRectForPosition: from screen coordinate to image position.
    int computeSelectedIndex(float xFloat, float yFloat) {
        int x = (int) xFloat;
        int y = (int) yFloat;

        int spacing = mSpec.mCellSpacing;
        int leftSpacing = mSpec.mLeftEdgePadding;

        int row = (getScrollY() + y - spacing) / (mSpec.mCellHeight + spacing);
        int col = Math.min(mColumns - 1,
                (x - leftSpacing) / (mSpec.mCellWidth + spacing));
        return (row * mColumns) + col;
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        if (!canHandleEvent()) {
            return false;
        }
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mCurrentPressState |= TAPPING_FLAG;
                invalidate();
                break;
            case MotionEvent.ACTION_UP:
                mCurrentPressState &= ~TAPPING_FLAG;
                invalidate();
                break;
        }
        mGestureDetector.onTouchEvent(ev);
        // Consume all events
        return true;
    }

    @Override
    public void scrollBy(int x, int y) {
        scrollTo(getScrollX() + x, getScrollY() + y);
    }

    public void scrollTo(float scrollPosition) {
        scrollTo(0, Math.round(scrollPosition * mMaxScrollY));
    }

    @Override
    public void scrollTo(int x, int y) {
        y = Math.max(0, Math.min(mMaxScrollY, y));
        if (mSpec != null) {
            mListener.onScroll((float) getScrollY() / mMaxScrollY);
        }
        super.scrollTo(x, y);
    }

    private boolean canHandleEvent() {
        return mRunning && mLayoutComplete;
    }

    private final Runnable mLongPressCallback = new Runnable() {
        public void run() {
            mCurrentPressState &= ~CLICKING_FLAG;
            showContextMenu();
        }
    };

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (!canHandleEvent()) return false;
        int sel = mCurrentSelection;
        if (sel != INDEX_NONE) {
            switch (keyCode) {
                case KeyEvent.KEYCODE_DPAD_RIGHT:
                    if (sel != mCount - 1 && (sel % mColumns < mColumns - 1)) {
                        sel += 1;
                    }
                    break;
                case KeyEvent.KEYCODE_DPAD_LEFT:
                    if (sel > 0 && (sel % mColumns != 0)) {
                        sel -= 1;
                    }
                    break;
                case KeyEvent.KEYCODE_DPAD_UP:
                    if (sel >= mColumns) {
                        sel -= mColumns;
                    }
                    break;
                case KeyEvent.KEYCODE_DPAD_DOWN:
                    sel = Math.min(mCount - 1, sel + mColumns);
                    break;
                case KeyEvent.KEYCODE_DPAD_CENTER:
                    if (event.getRepeatCount() == 0) {
                        mCurrentPressState |= CLICKING_FLAG;
                        mHandler.postDelayed(mLongPressCallback,
                                ViewConfiguration.getLongPressTimeout());
                    }
                    break;
                default:
                    return super.onKeyDown(keyCode, event);
            }
        } else {
            switch (keyCode) {
                case KeyEvent.KEYCODE_DPAD_RIGHT:
                case KeyEvent.KEYCODE_DPAD_LEFT:
                case KeyEvent.KEYCODE_DPAD_UP:
                case KeyEvent.KEYCODE_DPAD_DOWN:
                        int startRow =
                                (getScrollY() - mSpec.mCellSpacing) / mBlockHeight;
                        int topPos = startRow * mColumns;
                        Rect r = getRectForPosition(topPos);
                        if (r.top < getScrollY()) {
                            topPos += mColumns;
                        }
                        topPos = Math.min(mCount - 1, topPos);
                        sel = topPos;
                    break;
                default:
                    return super.onKeyDown(keyCode, event);
            }
        }
        setSelectedIndex(sel);
        return true;
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        if (!canHandleEvent()) return false;

        if (keyCode == KeyEvent.KEYCODE_DPAD_CENTER) {
            mCurrentPressState &= ~CLICKING_FLAG;
            invalidate();

            // The keyUp doesn't get called when the longpress menu comes up. We
            // only get here when the user lets go of the center key before the
            // longpress menu comes up.
            mHandler.removeCallbacks(mLongPressCallback);

            // open the photo
            mListener.onImageClicked(mCurrentSelection);
            return true;
        }
        return super.onKeyUp(keyCode, event);
    }

    private void paintDecoration(Canvas canvas) {
        // Calculate visible region according to scroll position.
        int startRow = (getScrollY() - mSpec.mCellSpacing) / mBlockHeight;
        int endRow = (getScrollY() + getHeight() - mSpec.mCellSpacing - 1)
                / mBlockHeight + 1;

        // Limit startRow and endRow to the valid range.
        // Make sure we handle the mRows == 0 case right.
        startRow = Math.max(Math.min(startRow, mRows - 1), 0);
        endRow = Math.max(Math.min(endRow, mRows), 0);

        int startIndex = startRow * mColumns;
        int endIndex = Math.min(endRow * mColumns, mCount);

        int xPos = mSpec.mLeftEdgePadding;
        int yPos = mSpec.mCellSpacing + startRow * mBlockHeight;
        int off = 0;
        for (int i = startIndex; i < endIndex; i++) {
            IImage image = mAllImages.getImageAt(i);

            mDrawAdapter.drawDecoration(canvas, image, xPos, yPos,
                    mSpec.mCellWidth, mSpec.mCellHeight);

            // Calculate next position
            off += 1;
            if (off == mColumns) {
                xPos = mSpec.mLeftEdgePadding;
                yPos += mBlockHeight;
                off = 0;
            } else {
                xPos += mSpec.mCellWidth + mSpec.mCellSpacing;
            }
        }
    }

    private void paintSelection(Canvas canvas) {
        if (mCurrentSelection == INDEX_NONE) return;

        if (mCurrentPressState == 0) return;

        int row = mCurrentSelection / mColumns;
        int col = mCurrentSelection - (row * mColumns);

        int spacing = mSpec.mCellSpacing;
        int leftSpacing = mSpec.mLeftEdgePadding;
        int xPos = leftSpacing + (col * (mSpec.mCellWidth + spacing));
        int yTop = spacing + (row * mBlockHeight);

        canvas.drawBitmap(mOutline[OUTLINE_PRESSED], xPos, yTop, null);
    }
}

class ImageBlockManager {
    @SuppressWarnings("unused")
    private static final String TAG = "ImageBlockManager";

    // Number of rows we want to cache.
    // Assume there are 6 rows per page, this caches 5 pages.
    private static final int CACHE_ROWS = 30;

    // mCache maps from row number to the ImageBlock.
    private final HashMap<Integer, ImageBlock> mCache;

    // These are parameters set in the constructor.
    private final Handler mHandler;
    private final Runnable mRedrawCallback;  // Called after a row is loaded,
                                             // so GridViewSpecial can draw
                                             // again using the new images.
    private final IImageList mImageList;
    private final ImageLoader mLoader;
    private final GridViewSpecial.DrawAdapter mDrawAdapter;
    private final GridViewSpecial.LayoutSpec mSpec;
    private final int mColumns;  // Columns per row.
    private final int mBlockWidth;  // The width of an ImageBlock.
    private final Bitmap mOutline;  // The outline bitmap put on top of each
                                    // image.
    private final int mCount;  // Cache mImageList.getCount().
    private final int mRows;  // Cache (mCount + mColumns - 1) / mColumns
    private final int mBlockHeight;  // The height of an ImageBlock.

    // Visible row range: [mStartRow, mEndRow). Set by setVisibleRows().
    private int mStartRow = 0;
    private int mEndRow = 0;

    ImageBlockManager(Handler handler, Runnable redrawCallback,
            IImageList imageList, ImageLoader loader,
            GridViewSpecial.DrawAdapter adapter,
            GridViewSpecial.LayoutSpec spec,
            int columns, int blockWidth, Bitmap outline) {
        mHandler = handler;
        mRedrawCallback = redrawCallback;
        mImageList = imageList;
        mLoader = loader;
        mDrawAdapter = adapter;
        mSpec = spec;
        mColumns = columns;
        mBlockWidth = blockWidth;
        mOutline = outline;
        mBlockHeight = mSpec.mCellSpacing + mSpec.mCellHeight;
        mCount = imageList.getCount();
        mRows = (mCount + mColumns - 1) / mColumns;
        mCache = Maps.newHashMap();
        mPendingRequest = 0;
        initGraphics();
    }

    // Set the window of visible rows. Once set we will start to load them as
    // soon as possible (if they are not already in cache).
    public void setVisibleRows(int startRow, int endRow) {
        if (startRow != mStartRow || endRow != mEndRow) {
            mStartRow = startRow;
            mEndRow = endRow;
            startLoading();
        }
    }

    int mPendingRequest;  // Number of pending requests (sent to ImageLoader).
    // We want to keep enough requests in ImageLoader's queue, but not too
    // many.
    static final int REQUESTS_LOW = 3;
    static final int REQUESTS_HIGH = 6;

    // After clear requests currently in queue, start loading the thumbnails.
    // We need to clear the queue first because the proper order of loading
    // may have changed (because the visible region changed, or some images
    // have been invalidated).
    private void startLoading() {
        clearLoaderQueue();
        continueLoading();
    }

    private void clearLoaderQueue() {
        int[] tags = mLoader.clearQueue();
        for (int pos : tags) {
            int row = pos / mColumns;
            int col = pos - row * mColumns;
            ImageBlock blk = mCache.get(row);
            assertTrue(blk != null);  // We won't reuse the block if it has pending
                                  // requests. See getEmptyBlock().
            blk.cancelRequest(col);
        }
    }

    // Scan the cache and send requests to ImageLoader if needed.
    private void continueLoading() {
        // Check if we still have enough requests in the queue.
        if (mPendingRequest >= REQUESTS_LOW) return;

        // Scan the visible rows.
        for (int i = mStartRow; i < mEndRow; i++) {
            if (scanOne(i)) return;
        }

        int range = (CACHE_ROWS - (mEndRow - mStartRow)) / 2;
        // Scan other rows.
        // d is the distance between the row and visible region.
        for (int d = 1; d <= range; d++) {
            int after = mEndRow - 1 + d;
            int before = mStartRow - d;
            if (after >= mRows && before < 0) {
                break;  // Nothing more the scan.
            }
            if (after < mRows && scanOne(after)) return;
            if (before >= 0 && scanOne(before)) return;
        }
    }

    // Returns true if we can stop scanning.
    private boolean scanOne(int i) {
        mPendingRequest += tryToLoad(i);
        return mPendingRequest >= REQUESTS_HIGH;
    }

    // Returns number of requests we issued for this row.
    private int tryToLoad(int row) {
        assertTrue(row >= 0 && row < mRows);
        ImageBlock blk = mCache.get(row);
        if (blk == null) {
            // Find an empty block
            blk = getEmptyBlock();
            blk.setRow(row);
            blk.invalidate();
            mCache.put(row, blk);
        }
        return blk.loadImages();
    }

    // Get an empty block for the cache.
    private ImageBlock getEmptyBlock() {
        // See if we can allocate a new block.
        if (mCache.size() < CACHE_ROWS) {
            return new ImageBlock();
        }
        // Reclaim the old block with largest distance from the visible region.
        int bestDistance = -1;
        int bestIndex = -1;
        for (int index : mCache.keySet()) {
            // Make sure we don't reclaim a block which still has pending
            // request.
            if (mCache.get(index).hasPendingRequests()) {
                continue;
            }
            int dist = 0;
            if (index >= mEndRow) {
                dist = index - mEndRow + 1;
            } else if (index < mStartRow) {
                dist = mStartRow - index;
            } else {
                // Inside the visible region.
                continue;
            }
            if (dist > bestDistance) {
                bestDistance = dist;
                bestIndex = index;
            }
        }
        return mCache.remove(bestIndex);
    }

    public void invalidateImage(int index) {
        int row = index / mColumns;
        int col = index - (row * mColumns);
        ImageBlock blk = mCache.get(row);
        if (blk == null) return;
        if ((blk.mCompletedMask & (1 << col)) != 0) {
            blk.mCompletedMask &= ~(1 << col);
        }
        startLoading();
    }

    // After calling recycle(), the instance should not be used anymore.
    public void recycle() {
        for (ImageBlock blk : mCache.values()) {
            blk.recycle();
        }
        mCache.clear();
        mEmptyBitmap.recycle();
    }

    // Draw the images to the given canvas.
    public void doDraw(Canvas canvas, int thisWidth, int thisHeight,
            int scrollPos) {
        final int height = mBlockHeight;

        // Note that currentBlock could be negative.
        int currentBlock = (scrollPos < 0)
                ? ((scrollPos - height + 1) / height)
                : (scrollPos / height);

        while (true) {
            final int yPos = currentBlock * height;
            if (yPos >= scrollPos + thisHeight) {
                break;
            }

            ImageBlock blk = mCache.get(currentBlock);
            if (blk != null) {
                blk.doDraw(canvas, 0, yPos);
            } else {
                drawEmptyBlock(canvas, 0, yPos, currentBlock);
            }

            currentBlock += 1;
        }
    }

    // Return number of columns in the given row. (This could be less than
    // mColumns for the last row).
    private int numColumns(int row) {
        return Math.min(mColumns, mCount - row * mColumns);
    }

    // Draw a block which has not been loaded.
    private void drawEmptyBlock(Canvas canvas, int xPos, int yPos, int row) {
        // Draw the background.
        canvas.drawRect(xPos, yPos, xPos + mBlockWidth, yPos + mBlockHeight,
                mBackgroundPaint);

        // Draw the empty images.
        int x = xPos + mSpec.mLeftEdgePadding;
        int y = yPos + mSpec.mCellSpacing;
        int cols = numColumns(row);

        for (int i = 0; i < cols; i++) {
            canvas.drawBitmap(mEmptyBitmap, x, y, null);
            x += (mSpec.mCellWidth + mSpec.mCellSpacing);
        }
    }

    // mEmptyBitmap is what we draw if we the wanted block hasn't been loaded.
    // (If the user scrolls too fast). It is a gray image with normal outline.
    // mBackgroundPaint is used to draw the (black) background outside
    // mEmptyBitmap.
    Paint mBackgroundPaint;
    private Bitmap mEmptyBitmap;

    private void initGraphics() {
        mBackgroundPaint = new Paint();
        mBackgroundPaint.setStyle(Paint.Style.FILL);
        mBackgroundPaint.setColor(0xFF000000);  // black
        mEmptyBitmap = Bitmap.createBitmap(mSpec.mCellWidth, mSpec.mCellHeight,
                Bitmap.Config.RGB_565);
        Canvas canvas = new Canvas(mEmptyBitmap);
        canvas.drawRGB(0xDD, 0xDD, 0xDD);
        canvas.drawBitmap(mOutline, 0, 0, null);
    }

    // ImageBlock stores bitmap for one row. The loaded thumbnail images are
    // drawn to mBitmap. mBitmap is later used in onDraw() of GridViewSpecial.
    private class ImageBlock {
        private Bitmap mBitmap;
        private final Canvas mCanvas;

        // Columns which have been requested to the loader
        private int mRequestedMask;

        // Columns which have been completed from the loader
        private int mCompletedMask;

        // The row number this block represents.
        private int mRow;

        public ImageBlock() {
            mBitmap = Bitmap.createBitmap(mBlockWidth, mBlockHeight,
                    Bitmap.Config.RGB_565);
            mCanvas = new Canvas(mBitmap);
            mRow = -1;
        }

        public void setRow(int row) {
            mRow = row;
        }

        public void invalidate() {
            // We do not change mRequestedMask or do cancelAllRequests()
            // because the data coming from pending requests are valid. (We only
            // invalidate data which has been drawn to the bitmap).
            mCompletedMask = 0;
        }

        // After recycle, the ImageBlock instance should not be accessed.
        public void recycle() {
            cancelAllRequests();
            mBitmap.recycle();
            mBitmap = null;
        }

        private boolean isVisible() {
            return mRow >= mStartRow && mRow < mEndRow;
        }

        // Returns number of requests submitted to ImageLoader.
        public int loadImages() {
            assertTrue(mRow != -1);

            int columns = numColumns(mRow);

            // Calculate what we need.
            int needMask = ((1 << columns) - 1)
                    & ~(mCompletedMask | mRequestedMask);

            if (needMask == 0) {
                return 0;
            }

            int retVal = 0;
            int base = mRow * mColumns;

            for (int col = 0; col < columns; col++) {
                if ((needMask & (1 << col)) == 0) {
                    continue;
                }

                int pos = base + col;

                final IImage image = mImageList.getImageAt(pos);
                if (image != null) {
                    // This callback is passed to ImageLoader. It will invoke
                    // loadImageDone() in the main thread. We limit the callback
                    // thread to be in this very short function. All other
                    // processing is done in the main thread.
                    final int colFinal = col;
                    ImageLoader.LoadedCallback cb =
                            new ImageLoader.LoadedCallback() {
                                    public void run(final Bitmap b) {
                                        mHandler.post(new Runnable() {
                                            public void run() {
                                                loadImageDone(image, b,
                                                        colFinal);
                                            }
                                        });
                                    }
                                };
                    // Load Image
                    mLoader.getBitmap(image, cb, pos);
                    mRequestedMask |= (1 << col);
                    retVal += 1;
                }
            }

            return retVal;
        }

        // Whether this block has pending requests.
        public boolean hasPendingRequests() {
            return mRequestedMask != 0;
        }

        // Called when an image is loaded.
        private void loadImageDone(IImage image, Bitmap b,
                int col) {
            if (mBitmap == null) return;  // This block has been recycled.

            int spacing = mSpec.mCellSpacing;
            int leftSpacing = mSpec.mLeftEdgePadding;
            final int yPos = spacing;
            final int xPos = leftSpacing
                    + (col * (mSpec.mCellWidth + spacing));

            drawBitmap(image, b, xPos, yPos);

            if (b != null) {
                b.recycle();
            }

            int mask = (1 << col);
            assertTrue((mCompletedMask & mask) == 0);
            assertTrue((mRequestedMask & mask) != 0);
            mRequestedMask &= ~mask;
            mCompletedMask |= mask;
            mPendingRequest--;

            if (isVisible()) {
                mRedrawCallback.run();
            }

            // Kick start next block loading.
            continueLoading();
        }

        // Draw the loaded bitmap to the block bitmap.
        private void drawBitmap(
                IImage image, Bitmap b, int xPos, int yPos) {
            mDrawAdapter.drawImage(mCanvas, image, b, xPos, yPos,
                    mSpec.mCellWidth, mSpec.mCellHeight);
            mCanvas.drawBitmap(mOutline, xPos, yPos, null);
        }

        // Draw the block bitmap to the specified canvas.
        public void doDraw(Canvas canvas, int xPos, int yPos) {
            int cols = numColumns(mRow);

            if (cols == mColumns) {
                canvas.drawBitmap(mBitmap, xPos, yPos, null);
            } else {

                // This must be the last row -- we draw only part of the block.
                // Draw the background.
                canvas.drawRect(xPos, yPos, xPos + mBlockWidth,
                        yPos + mBlockHeight, mBackgroundPaint);
                // Draw part of the block.
                int w = mSpec.mLeftEdgePadding
                        + cols * (mSpec.mCellWidth + mSpec.mCellSpacing);
                Rect srcRect = new Rect(0, 0, w, mBlockHeight);
                Rect dstRect = new Rect(srcRect);
                dstRect.offset(xPos, yPos);
                canvas.drawBitmap(mBitmap, srcRect, dstRect, null);
            }

            // Draw the part which has not been loaded.
            int isEmpty = ((1 << cols) - 1) & ~mCompletedMask;

            if (isEmpty != 0) {
                int x = xPos + mSpec.mLeftEdgePadding;
                int y = yPos + mSpec.mCellSpacing;

                for (int i = 0; i < cols; i++) {
                    if ((isEmpty & (1 << i)) != 0) {
                        canvas.drawBitmap(mEmptyBitmap, x, y, null);
                    }
                    x += (mSpec.mCellWidth + mSpec.mCellSpacing);
                }
            }
        }

        // Mark a request as cancelled. The request has already been removed
        // from the queue of ImageLoader, so we only need to mark the fact.
        public void cancelRequest(int col) {
            int mask = (1 << col);
            assertTrue((mRequestedMask & mask) != 0);
            mRequestedMask &= ~mask;
            mPendingRequest--;
        }

        // Try to cancel all pending requests for this block. After this
        // completes there could still be requests not cancelled (because it is
        // already in progress). We deal with that situation by setting mBitmap
        // to null in recycle() and check this in loadImageDone().
        private void cancelAllRequests() {
            for (int i = 0; i < mColumns; i++) {
                int mask = (1 << i);
                if ((mRequestedMask & mask) != 0) {
                    int pos = (mRow * mColumns) + i;
                    if (mLoader.cancel(mImageList.getImageAt(pos))) {
                        mRequestedMask &= ~mask;
                        mPendingRequest--;
                    }
                }
            }
        }
    }
}