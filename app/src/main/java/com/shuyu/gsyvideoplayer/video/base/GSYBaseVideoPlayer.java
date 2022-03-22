package com.shuyu.gsyvideoplayer.video.base;

import android.app.Activity;
import android.content.Context;
import android.content.pm.ActivityInfo;
import android.content.res.Configuration;
import android.graphics.Color;
import android.graphics.Point;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.FrameLayout;

import com.seafile.seadroid2.R;
import com.shuyu.gsyvideoplayer.utils.CommonUtil;
import com.shuyu.gsyvideoplayer.utils.Debuger;
import com.shuyu.gsyvideoplayer.utils.OrientationUtils;
import com.shuyu.gsyvideoplayer.view.SmallVideoTouch;
import com.transitionseverywhere.TransitionManager;

import java.lang.reflect.Constructor;

import static com.shuyu.gsyvideoplayer.utils.CommonUtil.getActionBarHeight;
import static com.shuyu.gsyvideoplayer.utils.CommonUtil.getStatusBarHeight;
import static com.shuyu.gsyvideoplayer.utils.CommonUtil.hideNavKey;
import static com.shuyu.gsyvideoplayer.utils.CommonUtil.hideSupportActionBar;
import static com.shuyu.gsyvideoplayer.utils.CommonUtil.showNavKey;
import static com.shuyu.gsyvideoplayer.utils.CommonUtil.showSupportActionBar;

/**
 * Handling full screen and small screen logic
 */

public abstract class GSYBaseVideoPlayer extends GSYVideoControlView {

    protected int mSystemUiVisibility;

    protected int[] mListItemRect;

    protected int[] mListItemSize;

    protected boolean mActionBar = false;

    protected boolean mStatusBar = false;

    protected boolean mShowFullAnimation = true;

    protected boolean mRotateViewAuto = true;

    protected boolean mRotateWithSystem = true;

    protected boolean mLockLand = false;

    protected boolean mAutoFullWithSize = false;

    protected View mSmallClose;

    protected OrientationUtils mOrientationUtils;

    protected View.OnClickListener mBackFromFullScreenListener;

    public GSYBaseVideoPlayer(Context context, Boolean fullFlag) {
        super(context, fullFlag);
    }

    public GSYBaseVideoPlayer(Context context) {
        super(context);
    }

    public GSYBaseVideoPlayer(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public GSYBaseVideoPlayer(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void init(Context context) {
        super.init(context);
        mSmallClose = findViewById(R.id.small_close);
    }


    @Override
    public void onBackFullscreen() {
        clearFullscreenLayout();
    }

    @Override
    protected void setSmallVideoTextureView() {
        if (mProgressBar != null) {
            mProgressBar.setOnTouchListener(null);
            mProgressBar.setVisibility(INVISIBLE);
        }
        if (mFullscreenButton != null) {
            mFullscreenButton.setOnTouchListener(null);
            mFullscreenButton.setVisibility(INVISIBLE);
        }
        if (mCurrentTimeTextView != null) {
            mCurrentTimeTextView.setVisibility(INVISIBLE);
        }
        if (mTextureViewContainer != null) {
            mTextureViewContainer.setOnClickListener(null);
        }
        if (mSmallClose != null) {
            mSmallClose.setVisibility(VISIBLE);
            mSmallClose.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View v) {
                    hideSmallVideo();
                    releaseVideos();
                }
            });
        }
    }

    @Override
    protected void lockTouchLogic() {
        super.lockTouchLogic();
        if (!mLockCurScreen) {
            if (mOrientationUtils != null)
                mOrientationUtils.setEnable(isRotateViewAuto());
        } else {
            if (mOrientationUtils != null)
                mOrientationUtils.setEnable(false);
        }
    }

    @Override
    public void onPrepared() {
        super.onPrepared();
        checkAutoFullSizeWhenFull();
    }

    @Override
    public void onInfo(int what, int extra) {
        super.onInfo(what, extra);
        if (what == getGSYVideoManager().getRotateInfoFlag()) {
            checkAutoFullSizeWhenFull();
        }
    }


    private ViewGroup getViewGroup() {
        return (ViewGroup) (CommonUtil.scanForActivity(getContext())).findViewById(Window.ID_ANDROID_CONTENT);
    }

    private void removeVideo(ViewGroup vp, int id) {
        View old = vp.findViewById(id);
        if (old != null) {
            if (old.getParent() != null) {
                ViewGroup viewGroup = (ViewGroup) old.getParent();
                vp.removeView(viewGroup);
            }
        }
    }

    private void saveLocationStatus(Context context, boolean statusBar, boolean actionBar) {
        getLocationOnScreen(mListItemRect);
        int statusBarH = getStatusBarHeight(context);
        int actionBerH = getActionBarHeight((Activity) context);
        if (statusBar) {
            mListItemRect[1] = mListItemRect[1] - statusBarH;
        }
        if (actionBar) {
            mListItemRect[1] = mListItemRect[1] - actionBerH;
        }
        mListItemSize[0] = getWidth();
        mListItemSize[1] = getHeight();
    }

    /**
     * Clone switch parameters
     *
     * @param from
     * @param to
     */
    protected void cloneParams(GSYBaseVideoPlayer from, GSYBaseVideoPlayer to) {
        to.mHadPlay = from.mHadPlay;
        to.mPlayTag = from.mPlayTag;
        to.mPlayPosition = from.mPlayPosition;
        to.mEffectFilter = from.mEffectFilter;
        to.mFullPauseBitmap = from.mFullPauseBitmap;
        to.mNeedShowWifiTip = from.mNeedShowWifiTip;
        to.mShrinkImageRes = from.mShrinkImageRes;
        to.mEnlargeImageRes = from.mEnlargeImageRes;
        to.mRotate = from.mRotate;
        to.mShowPauseCover = from.mShowPauseCover;
        to.mDismissControlTime = from.mDismissControlTime;
        to.mSeekRatio = from.mSeekRatio;
        to.mNetChanged = from.mNetChanged;
        to.mNetSate = from.mNetSate;
        to.mRotateWithSystem = from.mRotateWithSystem;
        to.mBackUpPlayingBufferState = from.mBackUpPlayingBufferState;
        to.mRenderer = from.mRenderer;
        to.mMode = from.mMode;
        to.mBackFromFullScreenListener = from.mBackFromFullScreenListener;
        to.mGSYVideoProgressListener = from.mGSYVideoProgressListener;
        to.mHadPrepared = from.mHadPrepared;
        to.mStartAfterPrepared = from.mStartAfterPrepared;
        to.mPauseBeforePrepared = from.mPauseBeforePrepared;
        to.mReleaseWhenLossAudio = from.mReleaseWhenLossAudio;
        to.mVideoAllCallBack = from.mVideoAllCallBack;
        to.mActionBar = from.mActionBar;
        to.mStatusBar = from.mStatusBar;
        to.mAutoFullWithSize = from.mAutoFullWithSize;
        if (from.mSetUpLazy) {
            to.setUpLazy(from.mOriginUrl, from.mCache, from.mCachePath, from.mMapHeadData, from.mTitle);
            to.mUrl = from.mUrl;
        } else {
            to.setUp(from.mOriginUrl, from.mCache, from.mCachePath, from.mMapHeadData, from.mTitle);
        }
        to.setLooping(from.isLooping());
        to.setIsTouchWigetFull(from.mIsTouchWigetFull);
        to.setSpeed(from.getSpeed(), from.mSoundTouch);
        to.setStateAndUi(from.mCurrentState);
    }

    private void pauseFullCoverLogic() {
        if (mCurrentState == GSYVideoPlayer.CURRENT_STATE_PAUSE && mTextureView != null
                && (mFullPauseBitmap == null || mFullPauseBitmap.isRecycled()) && mShowPauseCover) {
            try {
                initCover();
            } catch (Exception e) {
                e.printStackTrace();
                mFullPauseBitmap = null;
            }
        }
    }

    /**
     * When the full-screen pause returns, the return page is not black
     */
    private void pauseFullBackCoverLogic(GSYBaseVideoPlayer gsyVideoPlayer) {
        if (gsyVideoPlayer.mCurrentState == GSYVideoPlayer.CURRENT_STATE_PAUSE
                && gsyVideoPlayer.mTextureView != null && mShowPauseCover) {
            if (gsyVideoPlayer.mFullPauseBitmap != null
                    && !gsyVideoPlayer.mFullPauseBitmap.isRecycled() && mShowPauseCover) {
                mFullPauseBitmap = gsyVideoPlayer.mFullPauseBitmap;
            } else if (mShowPauseCover) {
                try {
                    initCover();
                } catch (Exception e) {
                    e.printStackTrace();
                    mFullPauseBitmap = null;
                }
            }
        }
    }

    /**
     * full screen
     */
    protected void resolveFullVideoShow(Context context, final GSYBaseVideoPlayer gsyVideoPlayer, final FrameLayout frameLayout) {
        LayoutParams lp = (LayoutParams) gsyVideoPlayer.getLayoutParams();
        lp.setMargins(0, 0, 0, 0);
        lp.height = ViewGroup.LayoutParams.MATCH_PARENT;
        lp.width = ViewGroup.LayoutParams.MATCH_PARENT;
        lp.gravity = Gravity.CENTER;
        gsyVideoPlayer.setLayoutParams(lp);
        gsyVideoPlayer.setIfCurrentIsFullscreen(true);
        mOrientationUtils = new OrientationUtils((Activity) context, gsyVideoPlayer);
        mOrientationUtils.setEnable(isRotateViewAuto());
        mOrientationUtils.setRotateWithSystem(mRotateWithSystem);
        gsyVideoPlayer.mOrientationUtils = mOrientationUtils;

        final boolean isVertical = isVerticalFullByVideoSize();
        final boolean isLockLand = isLockLandByAutoFullSize();

        if (isShowFullAnimation()) {
            postDelayed(new Runnable() {
                @Override
                public void run() {
                    Debuger.printfLog("GSYVideoBase resolveFullVideoShow isVerticalFullByVideoSize " + isVertical);
                    if (!isVertical && isLockLand && mOrientationUtils.getIsLand() != 1) {
                        mOrientationUtils.resolveByClick();
                    }
                    gsyVideoPlayer.setVisibility(VISIBLE);
                    frameLayout.setVisibility(VISIBLE);
                }
            }, 300);
        } else {
            if (!isVertical && isLockLand) {
                mOrientationUtils.resolveByClick();
            }
            gsyVideoPlayer.setVisibility(VISIBLE);
            frameLayout.setVisibility(VISIBLE);
        }


        if (mVideoAllCallBack != null) {
            Debuger.printfError("onEnterFullscreen");
            mVideoAllCallBack.onEnterFullscreen(mOriginUrl, mTitle, gsyVideoPlayer);
        }
        mIfCurrentIsFullscreen = true;

        checkoutState();
    }

    /**
     * recover
     */
    protected void resolveNormalVideoShow(View oldF, ViewGroup vp, GSYVideoPlayer gsyVideoPlayer) {

        if (oldF != null && oldF.getParent() != null) {
            ViewGroup viewGroup = (ViewGroup) oldF.getParent();
            vp.removeView(viewGroup);
        }
        mCurrentState = getGSYVideoManager().getLastState();
        if (gsyVideoPlayer != null) {
            cloneParams(gsyVideoPlayer, this);
        }
        getGSYVideoManager().setListener(getGSYVideoManager().lastListener());
        getGSYVideoManager().setLastListener(null);
        setStateAndUi(mCurrentState);
        addTextureView();
        mSaveChangeViewTIme = System.currentTimeMillis();
        if (mVideoAllCallBack != null) {
            Debuger.printfError("onQuitFullscreen");
            mVideoAllCallBack.onQuitFullscreen(mOriginUrl, mTitle, this);
        }
        mIfCurrentIsFullscreen = false;
        if (mHideKey) {
            showNavKey(mContext, mSystemUiVisibility);
        }
        showSupportActionBar(mContext, mActionBar, mStatusBar);
        getFullscreenButton().setImageResource(getEnlargeImageRes());
    }


    /**
     * Exit the window layer to play the full screen effect
     */
    @SuppressWarnings("ResourceType")
    protected void clearFullscreenLayout() {
        mIfCurrentIsFullscreen = false;
        int delay = 0;
        if (mOrientationUtils != null) {
            delay = mOrientationUtils.backToProtVideo();
            mOrientationUtils.setEnable(false);
            if (mOrientationUtils != null) {
                mOrientationUtils.releaseListener();
                mOrientationUtils = null;
            }
        }

        if (!mShowFullAnimation) {
            delay = 0;
        }

        final ViewGroup vp = getViewGroup();
        final View oldF = vp.findViewById(getFullId());
        if (oldF != null) {
            GSYVideoPlayer gsyVideoPlayer = (GSYVideoPlayer) oldF;
            gsyVideoPlayer.mIfCurrentIsFullscreen = false;
        }

        postDelayed(new Runnable() {
            @Override
            public void run() {
                backToNormal();
            }
        }, delay);

    }

    /**
     * back to normal
     */
    @SuppressWarnings("ResourceType")
    protected void backToNormal() {

        final ViewGroup vp = getViewGroup();

        final View oldF = vp.findViewById(getFullId());
        final GSYVideoPlayer gsyVideoPlayer;
        if (oldF != null) {
            gsyVideoPlayer = (GSYVideoPlayer) oldF;
            pauseFullBackCoverLogic(gsyVideoPlayer);
            if (mShowFullAnimation) {
                TransitionManager.beginDelayedTransition(vp);

                LayoutParams lp = (LayoutParams) gsyVideoPlayer.getLayoutParams();
                lp.setMargins(mListItemRect[0], mListItemRect[1], 0, 0);
                lp.width = mListItemSize[0];
                lp.height = mListItemSize[1];
                lp.gravity = Gravity.NO_GRAVITY;
                gsyVideoPlayer.setLayoutParams(lp);

                postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        resolveNormalVideoShow(oldF, vp, gsyVideoPlayer);
                    }
                }, 400);
            } else {
                resolveNormalVideoShow(oldF, vp, gsyVideoPlayer);
            }

        } else {
            resolveNormalVideoShow(null, vp, null);
        }
    }

    protected Runnable mCheckoutTask = new Runnable() {
        @Override
        public void run() {
            GSYVideoPlayer gsyVideoPlayer = getFullWindowPlayer();
            if (gsyVideoPlayer != null
                    && gsyVideoPlayer.mCurrentState != mCurrentState) {
                if (gsyVideoPlayer.mCurrentState == CURRENT_STATE_PLAYING_BUFFERING_START
                        && mCurrentState != CURRENT_STATE_PREPAREING) {
                    gsyVideoPlayer.setStateAndUi(mCurrentState);
                }
            }
        }
    };

    protected void checkoutState() {
        removeCallbacks(mCheckoutTask);
        postDelayed(mCheckoutTask, 500);
    }

    /**
     * Whether the vertical screen in vertical screen mode
     */
    protected boolean isVerticalVideo() {
        boolean isVertical = false;
        int videoHeight = getCurrentVideoHeight();
        int videoWidth = getCurrentVideoWidth();
        Debuger.printfLog("GSYVideoBase isVerticalVideo  videoHeight " + videoHeight + " videoWidth " + videoWidth);
        Debuger.printfLog("GSYVideoBase isVerticalVideo  mRotate " + mRotate);
        if (videoHeight > 0 && videoWidth > 0) {
            if (mRotate == 90 || mRotate == 270) {
                isVertical = videoWidth > videoHeight;
            } else {
                isVertical = videoHeight > videoWidth;
            }
        }
        return isVertical;
    }

    protected boolean isLockLandByAutoFullSize() {
        boolean isLockLand = mLockLand;
        if (isAutoFullWithSize()) {
            isLockLand = true;
        }
        return isLockLand;
    }

    protected void checkAutoFullSizeWhenFull() {
        if (mIfCurrentIsFullscreen) {
            boolean isV = isVerticalFullByVideoSize();
            Debuger.printfLog("GSYVideoBase onPrepared isVerticalFullByVideoSize " + isV);
            if (isV) {
                if (mOrientationUtils != null)
                    mOrientationUtils.backToProtVideo();
            }
        }
    }

    protected abstract int getFullId();

    protected abstract int getSmallId();

    /************************* open interface *************************/

    /**
     * Whether to automatically select vertical full screen or horizontal full screen according to the video size,
     * note that the default rotation is invalid at this time
     */
    public boolean isVerticalFullByVideoSize() {
        return isVerticalVideo() && isAutoFullWithSize();
    }

    public void onConfigurationChanged(Activity activity, Configuration newConfig, OrientationUtils orientationUtils) {
        onConfigurationChanged(activity, newConfig, orientationUtils, true, true);

    }

    /**
     * Rotation processing
     *
     */
    public void onConfigurationChanged(Activity activity, Configuration newConfig, OrientationUtils orientationUtils, boolean hideActionBar, boolean hideStatusBar) {
        super.onConfigurationChanged(newConfig);
        if (newConfig.orientation == ActivityInfo.SCREEN_ORIENTATION_USER) {
            if (!isIfCurrentIsFullscreen()) {
                startWindowFullscreen(activity, hideActionBar, hideStatusBar);
            }
        } else {
            if (isIfCurrentIsFullscreen() && !isVerticalFullByVideoSize()) {
                backFromFull(activity);
            }
            if (orientationUtils != null) {
                orientationUtils.setEnable(true);
            }
        }

    }

    /**
     * Use window layer to play full screen effect
     *
     * @param context
     */
    @SuppressWarnings("ResourceType, unchecked")
    public GSYBaseVideoPlayer startWindowFullscreen(final Context context, final boolean actionBar, final boolean statusBar) {


        mSystemUiVisibility = ((Activity) context).getWindow().getDecorView().getSystemUiVisibility();

        hideSupportActionBar(context, actionBar, statusBar);

        if (mHideKey) {
            hideNavKey(context);
        }

        this.mActionBar = actionBar;

        this.mStatusBar = statusBar;

        mListItemRect = new int[2];

        mListItemSize = new int[2];

        final ViewGroup vp = getViewGroup();

        removeVideo(vp, getFullId());

        //Handling Pause Logic
        pauseFullCoverLogic();

        if (mTextureViewContainer.getChildCount() > 0) {
            mTextureViewContainer.removeAllViews();
        }

        saveLocationStatus(context, statusBar, actionBar);

        //Turn off non-fullscreen timers when switching
        cancelProgressTimer();

        boolean hadNewConstructor = true;

        try {
            GSYBaseVideoPlayer.this.getClass().getConstructor(Context.class, Boolean.class);
        } catch (Exception e) {
            hadNewConstructor = false;
        }

        try {
            //Selected by overloaded different constructors
            Constructor<GSYBaseVideoPlayer> constructor;
            final GSYBaseVideoPlayer gsyVideoPlayer;
            if (!hadNewConstructor) {
                constructor = (Constructor<GSYBaseVideoPlayer>) GSYBaseVideoPlayer.this.getClass().getConstructor(Context.class);
                gsyVideoPlayer = constructor.newInstance(getActivityContext());
            } else {
                constructor = (Constructor<GSYBaseVideoPlayer>) GSYBaseVideoPlayer.this.getClass().getConstructor(Context.class, Boolean.class);
                gsyVideoPlayer = constructor.newInstance(getActivityContext(), true);
            }

            gsyVideoPlayer.setId(getFullId());
            gsyVideoPlayer.setIfCurrentIsFullscreen(true);
            gsyVideoPlayer.setVideoAllCallBack(mVideoAllCallBack);

            cloneParams(this, gsyVideoPlayer);

            if (gsyVideoPlayer.getFullscreenButton() != null) {
                gsyVideoPlayer.getFullscreenButton().setImageResource(getShrinkImageRes());
                gsyVideoPlayer.getFullscreenButton().setOnClickListener(new OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        if (mBackFromFullScreenListener == null) {
                            clearFullscreenLayout();
                        } else {
                            mBackFromFullScreenListener.onClick(v);
                        }
                    }
                });
            }

            if (gsyVideoPlayer.getBackButton() != null) {
                gsyVideoPlayer.getBackButton().setVisibility(VISIBLE);
                gsyVideoPlayer.getBackButton().setOnClickListener(new OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        if (mBackFromFullScreenListener == null) {
                            clearFullscreenLayout();
                        } else {
                            mBackFromFullScreenListener.onClick(v);
                        }
                    }
                });
            }

            final LayoutParams lpParent = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
            final FrameLayout frameLayout = new FrameLayout(context);
            frameLayout.setBackgroundColor(Color.BLACK);

            if (mShowFullAnimation) {
                LayoutParams lp = new LayoutParams(getWidth(), getHeight());
                lp.setMargins(mListItemRect[0], mListItemRect[1], 0, 0);
                frameLayout.addView(gsyVideoPlayer, lp);
                vp.addView(frameLayout, lpParent);
                postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        TransitionManager.beginDelayedTransition(vp);
                        resolveFullVideoShow(context, gsyVideoPlayer, frameLayout);
                    }
                }, 300);
            } else {
                LayoutParams lp = new LayoutParams(getWidth(), getHeight());
                frameLayout.addView(gsyVideoPlayer, lp);
                vp.addView(frameLayout, lpParent);
                gsyVideoPlayer.setVisibility(INVISIBLE);
                frameLayout.setVisibility(INVISIBLE);
                resolveFullVideoShow(context, gsyVideoPlayer, frameLayout);
            }

            gsyVideoPlayer.addTextureView();

            gsyVideoPlayer.startProgressTimer();

            getGSYVideoManager().setLastListener(this);
            getGSYVideoManager().setListener(gsyVideoPlayer);

            checkoutState();
            return gsyVideoPlayer;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }


    /**
     * Show small window
     */
    @SuppressWarnings("ResourceType, unchecked")
    public GSYBaseVideoPlayer showSmallVideo(Point size, final boolean actionBar, final boolean statusBar) {

        final ViewGroup vp = getViewGroup();

        removeVideo(vp, getSmallId());

        if (mTextureViewContainer.getChildCount() > 0) {
            mTextureViewContainer.removeAllViews();
        }

        try {
            Constructor<GSYBaseVideoPlayer> constructor = (Constructor<GSYBaseVideoPlayer>) GSYBaseVideoPlayer.this.getClass().getConstructor(Context.class);
            GSYBaseVideoPlayer gsyVideoPlayer = constructor.newInstance(getActivityContext());
            gsyVideoPlayer.setId(getSmallId());

            LayoutParams lpParent = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
            FrameLayout frameLayout = new FrameLayout(mContext);

            LayoutParams lp = new LayoutParams(size.x, size.y);
            int marginLeft = CommonUtil.getScreenWidth(mContext) - size.x;
            int marginTop = CommonUtil.getScreenHeight(mContext) - size.y;

            if (actionBar) {
                marginTop = marginTop - getActionBarHeight((Activity) mContext);
            }

            if (statusBar) {
                marginTop = marginTop - getStatusBarHeight(mContext);
            }

            lp.setMargins(marginLeft, marginTop, 0, 0);
            frameLayout.addView(gsyVideoPlayer, lp);

            vp.addView(frameLayout, lpParent);

            cloneParams(this, gsyVideoPlayer);

            gsyVideoPlayer.setIsTouchWiget(false);

            gsyVideoPlayer.addTextureView();
            gsyVideoPlayer.onClickUiToggle();
            gsyVideoPlayer.setVideoAllCallBack(mVideoAllCallBack);
            gsyVideoPlayer.setSmallVideoTextureView(new SmallVideoTouch(gsyVideoPlayer, marginLeft, marginTop));

            getGSYVideoManager().setLastListener(this);
            getGSYVideoManager().setListener(gsyVideoPlayer);
            if (mVideoAllCallBack != null) {
                Debuger.printfError("onEnterSmallWidget");
                mVideoAllCallBack.onEnterSmallWidget(mOriginUrl, mTitle, gsyVideoPlayer);
            }

            return gsyVideoPlayer;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * hide small windows
     */
    @SuppressWarnings("ResourceType")
    public void hideSmallVideo() {
        final ViewGroup vp = getViewGroup();
        GSYVideoPlayer gsyVideoPlayer = (GSYVideoPlayer) vp.findViewById(getSmallId());
        removeVideo(vp, getSmallId());
        mCurrentState = getGSYVideoManager().getLastState();
        if (gsyVideoPlayer != null) {
            cloneParams(gsyVideoPlayer, this);
        }
        getGSYVideoManager().setListener(getGSYVideoManager().lastListener());
        getGSYVideoManager().setLastListener(null);
        setStateAndUi(mCurrentState);
        addTextureView();
        mSaveChangeViewTIme = System.currentTimeMillis();
        if (mVideoAllCallBack != null) {
            Debuger.printfLog("onQuitSmallWidget");
            mVideoAllCallBack.onQuitSmallWidget(mOriginUrl, mTitle, this);
        }
    }

    public boolean isShowFullAnimation() {
        return mShowFullAnimation;
    }

    /**
     * Full screen animation
     *
     * @param showFullAnimation Whether to use full screen animation
     */
    public void setShowFullAnimation(boolean showFullAnimation) {
        this.mShowFullAnimation = showFullAnimation;
    }

    public boolean isRotateViewAuto() {
        if (mAutoFullWithSize) {
            return false;
        }
        return mRotateViewAuto;
    }

    /**
     * Whether to enable automatic rotation
     */
    public void setRotateViewAuto(boolean rotateViewAuto) {
        this.mRotateViewAuto = rotateViewAuto;
        if (mOrientationUtils != null) {
            mOrientationUtils.setEnable(rotateViewAuto);
        }
    }

    public boolean isLockLand() {
        return mLockLand;
    }

    public void setLockLand(boolean lockLand) {
        this.mLockLand = lockLand;
    }


    public boolean isRotateWithSystem() {
        return mRotateWithSystem;
    }

    public void setRotateWithSystem(boolean rotateWithSystem) {
        this.mRotateWithSystem = rotateWithSystem;
    }

    /**
     * Get the fullscreen player object
     *
     */
    @SuppressWarnings("ResourceType")
    public GSYVideoPlayer getFullWindowPlayer() {
        ViewGroup vp = (ViewGroup) (CommonUtil.scanForActivity(getContext())).findViewById(Window.ID_ANDROID_CONTENT);
        final View full = vp.findViewById(getFullId());
        GSYVideoPlayer gsyVideoPlayer = null;
        if (full != null) {
            gsyVideoPlayer = (GSYVideoPlayer) full;
        }
        return gsyVideoPlayer;
    }

    /**
     * Get the widget player object
     *
     * @return GSYVideoPlayer If not, return null。
     */
    @SuppressWarnings("ResourceType")
    public GSYVideoPlayer getSmallWindowPlayer() {
        ViewGroup vp = (ViewGroup) (CommonUtil.scanForActivity(getContext())).findViewById(Window.ID_ANDROID_CONTENT);
        final View small = vp.findViewById(getSmallId());
        GSYVideoPlayer gsyVideoPlayer = null;
        if (small != null) {
            gsyVideoPlayer = (GSYVideoPlayer) small;
        }
        return gsyVideoPlayer;
    }

    /**
     * Get the playback controls that are currently playing
     */
    public GSYBaseVideoPlayer getCurrentPlayer() {
        if (getFullWindowPlayer() != null) {
            return getFullWindowPlayer();
        }
        if (getSmallWindowPlayer() != null) {
            return getSmallWindowPlayer();
        }
        return this;
    }

    public void setBackFromFullScreenListener(OnClickListener backFromFullScreenListener) {
        this.mBackFromFullScreenListener = backFromFullScreenListener;
    }

    public void setFullHideActionBar(boolean actionBar) {
        this.mActionBar = actionBar;
    }

    public void setFullHideStatusBar(boolean statusBar) {
        this.mStatusBar = statusBar;
    }

    public boolean isFullHideActionBar() {
        return mActionBar;
    }

    public boolean isFullHideStatusBar() {
        return mStatusBar;
    }

    public int getSaveBeforeFullSystemUiVisibility() {
        return mSystemUiVisibility;
    }

    public void setSaveBeforeFullSystemUiVisibility(int systemUiVisibility) {
        this.mSystemUiVisibility = systemUiVisibility;
    }

    public boolean isAutoFullWithSize() {
        return mAutoFullWithSize;
    }

    public void setAutoFullWithSize(boolean autoFullWithSize) {
        this.mAutoFullWithSize = autoFullWithSize;
    }
}
