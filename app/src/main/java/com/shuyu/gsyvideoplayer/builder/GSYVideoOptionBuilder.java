package com.shuyu.gsyvideoplayer.builder;

import android.graphics.drawable.Drawable;
import android.view.View;

import com.shuyu.gsyvideoplayer.listener.GSYVideoProgressListener;
import com.shuyu.gsyvideoplayer.render.view.GSYVideoGLView;
import com.shuyu.gsyvideoplayer.render.effect.NoEffect;
import com.shuyu.gsyvideoplayer.listener.LockClickListener;
import com.shuyu.gsyvideoplayer.listener.VideoAllCallBack;
import com.shuyu.gsyvideoplayer.video.base.GSYBaseVideoPlayer;
import com.shuyu.gsyvideoplayer.video.StandardGSYVideoPlayer;

import java.io.File;
import java.util.Map;

/**
 * Configure tools Class。
 * It's not a normal Builder, it's just a collection of all settings and configurations.
 * Each configuration can actually find a separate setting in the corresponding video interface
 * It's just for convenience
 */

public class GSYVideoOptionBuilder {

    //Exit full screen case image
    protected int mShrinkImageRes = -1;

    //Case image displayed in full screen
    protected int mEnlargeImageRes = -1;

    protected int mPlayPosition = -22;

    //Touch to fast forward dialog's progress high volume color
    protected int mDialogProgressHighLightColor = -11;

    protected int mDialogProgressNormalColor = -11;

    protected int mDismissControlTime = 2500;

    protected long mSeekOnStart = -1;

    protected float mSeekRatio = 1;

    protected float mSpeed = 1;

    protected boolean mHideKey = true;

    protected boolean mShowFullAnimation = true;

    protected boolean mAutoFullWithSize = false;

    protected boolean mNeedShowWifiTip = true;

    protected boolean mRotateViewAuto = true;

    protected boolean mLockLand = false;

    protected boolean mLooping = false;

    protected boolean mIsTouchWiget = true;

    protected boolean mIsTouchWigetFull = true;

    protected boolean mShowPauseCover = true;

    protected boolean mRotateWithSystem = true;

    //Cache while playing
    protected boolean mCacheWithPlay;

    protected boolean mNeedLockFull;

    protected boolean mThumbPlay;

    protected boolean mSounchTouch;

    protected boolean mSetUpLazy = false;

    protected boolean mStartAfterPrepared = true;

    protected boolean mReleaseWhenLossAudio = true;

    protected boolean mActionBar = false;

    protected boolean mStatusBar = false;

    protected String mPlayTag = "";

    protected String mUrl;

    protected String mVideoTitle = null;

    protected File mCachePath;

    protected Map<String, String> mMapHeadData;

    protected VideoAllCallBack mVideoAllCallBack;

    protected LockClickListener mLockClickListener;

    protected View mThumbImageView;

    protected Drawable mBottomProgressDrawable;

    protected Drawable mBottomShowProgressDrawable;

    protected Drawable mBottomShowProgressThumbDrawable;

    protected Drawable mVolumeProgressDrawable;

    protected Drawable mDialogProgressBarDrawable;

    protected GSYVideoGLView.ShaderInterface mEffectFilter = new NoEffect();

    protected GSYVideoProgressListener mGSYVideoProgressListener;


    public GSYVideoOptionBuilder setAutoFullWithSize(boolean autoFullWithSize) {
        this.mAutoFullWithSize = autoFullWithSize;
        return this;
    }

    /**
     * Full screen animation
     */
    public GSYVideoOptionBuilder setShowFullAnimation(boolean showFullAnimation) {
        this.mShowFullAnimation = showFullAnimation;
        return this;
    }

    /**
     * set loop
     */
    public GSYVideoOptionBuilder setLooping(boolean looping) {
        this.mLooping = looping;
        return this;
    }


    /**
     * Set the callback during playback
     *
     * @param mVideoAllCallBack
     */
    public GSYVideoOptionBuilder setVideoAllCallBack(VideoAllCallBack mVideoAllCallBack) {
        this.mVideoAllCallBack = mVideoAllCallBack;
        return this;
    }

    /**
     * Whether to enable automatic rotation
     */
    public GSYVideoOptionBuilder setRotateViewAuto(boolean rotateViewAuto) {
        this.mRotateViewAuto = rotateViewAuto;
        return this;
    }

    public GSYVideoOptionBuilder setLockLand(boolean lockLand) {
        this.mLockLand = lockLand;
        return this;
    }

    /**
     * Play speed
     */
    public GSYVideoOptionBuilder setSpeed(float speed) {
        this.mSpeed = speed;
        return this;
    }


    public GSYVideoOptionBuilder setSoundTouch(boolean soundTouch) {
        this.mSounchTouch = soundTouch;
        return this;
    }

    public GSYVideoOptionBuilder setHideKey(boolean hideKey) {
        this.mHideKey = hideKey;
        return this;
    }

    public GSYVideoOptionBuilder setIsTouchWiget(boolean isTouchWiget) {
        this.mIsTouchWiget = isTouchWiget;
        return this;
    }

    public GSYVideoOptionBuilder setIsTouchWigetFull(boolean isTouchWigetFull) {
        this.mIsTouchWigetFull = isTouchWigetFull;
        return this;
    }


    public GSYVideoOptionBuilder setNeedShowWifiTip(boolean needShowWifiTip) {
        this.mNeedShowWifiTip = needShowWifiTip;
        return this;
    }

    public GSYVideoOptionBuilder setEnlargeImageRes(int mEnlargeImageRes) {
        this.mEnlargeImageRes = mEnlargeImageRes;
        return this;
    }

    public GSYVideoOptionBuilder setShrinkImageRes(int mShrinkImageRes) {
        this.mShrinkImageRes = mShrinkImageRes;
        return this;
    }


    public GSYVideoOptionBuilder setShowPauseCover(boolean showPauseCover) {
        this.mShowPauseCover = showPauseCover;
        return this;
    }

    public GSYVideoOptionBuilder setSeekRatio(float seekRatio) {
        if (seekRatio < 0) {
            return this;
        }
        this.mSeekRatio = seekRatio;
        return this;
    }

    public GSYVideoOptionBuilder setRotateWithSystem(boolean rotateWithSystem) {
        this.mRotateWithSystem = rotateWithSystem;
        return this;
    }

    public GSYVideoOptionBuilder setPlayTag(String playTag) {
        this.mPlayTag = playTag;
        return this;
    }


    public GSYVideoOptionBuilder setPlayPosition(int playPosition) {
        this.mPlayPosition = playPosition;
        return this;
    }

    /**
     * where to start playing
     */
    public GSYVideoOptionBuilder setSeekOnStart(long seekOnStart) {
        this.mSeekOnStart = seekOnStart;
        return this;
    }

    public GSYVideoOptionBuilder setUrl(String url) {
        this.mUrl = url;
        return this;
    }

    public GSYVideoOptionBuilder setVideoTitle(String videoTitle) {
        this.mVideoTitle = videoTitle;
        return this;
    }

    public GSYVideoOptionBuilder setCacheWithPlay(boolean cacheWithPlay) {
        this.mCacheWithPlay = cacheWithPlay;
        return this;
    }

    public GSYVideoOptionBuilder setStartAfterPrepared(boolean startAfterPrepared) {
        this.mStartAfterPrepared = startAfterPrepared;
        return this;
    }


    /**
     * Losing audio focus for a long time, pausing the player
     */
    public GSYVideoOptionBuilder setReleaseWhenLossAudio(boolean releaseWhenLossAudio) {
        this.mReleaseWhenLossAudio = releaseWhenLossAudio;
        return this;
    }

    /**
     * Customize the specified cache path, it is recommended not to set it, use the default path
     *
     * @param cachePath
     */
    public GSYVideoOptionBuilder setCachePath(File cachePath) {
        this.mCachePath = cachePath;
        return this;
    }

    /**
     * Set request headers
     *
     * @param mapHeadData
     */
    public GSYVideoOptionBuilder setMapHeadData(Map<String, String> mapHeadData) {
        this.mMapHeadData = mapHeadData;
        return this;
    }


    public GSYVideoOptionBuilder setGSYVideoProgressListener(GSYVideoProgressListener videoProgressListener) {
        this.mGSYVideoProgressListener = videoProgressListener;
        return this;
    }

    public GSYVideoOptionBuilder setThumbImageView(View view) {
        mThumbImageView = view;
        return this;
    }

    public GSYVideoOptionBuilder setBottomShowProgressBarDrawable(Drawable drawable, Drawable thumb) {
        mBottomShowProgressDrawable = drawable;
        mBottomShowProgressThumbDrawable = thumb;
        return this;
    }

    public GSYVideoOptionBuilder setBottomProgressBarDrawable(Drawable drawable) {
        mBottomProgressDrawable = drawable;
        return this;
    }

    public GSYVideoOptionBuilder setDialogVolumeProgressBar(Drawable drawable) {
        mVolumeProgressDrawable = drawable;
        return this;
    }


    public GSYVideoOptionBuilder setDialogProgressBar(Drawable drawable) {
        mDialogProgressBarDrawable = drawable;
        return this;
    }

    public GSYVideoOptionBuilder setDialogProgressColor(int highLightColor, int normalColor) {
        mDialogProgressHighLightColor = highLightColor;
        mDialogProgressNormalColor = normalColor;
        return this;
    }

    /**
     * Whether to click the cover to play
     */
    public GSYVideoOptionBuilder setThumbPlay(boolean thumbPlay) {
        this.mThumbPlay = thumbPlay;
        return this;
    }

    /**
     * Do you need a full screen lock screen feature
     */
    public GSYVideoOptionBuilder setNeedLockFull(boolean needLoadFull) {
        this.mNeedLockFull = needLoadFull;
        return this;
    }

    /**
     * lock screen click
     */
    public GSYVideoOptionBuilder setLockClickListener(LockClickListener lockClickListener) {
        this.mLockClickListener = lockClickListener;
        return this;
    }

    /**
     * Set the disappearing time of touch display control ui
     *
     * @param dismissControlTime millisecond，Default 2500
     */
    public GSYVideoOptionBuilder setDismissControlTime(int dismissControlTime) {
        this.mDismissControlTime = dismissControlTime;
        return this;
    }

    /**
     * Set filter effects
     */
    public GSYVideoOptionBuilder setEffectFilter(GSYVideoGLView.ShaderInterface effectFilter) {
        this.mEffectFilter = effectFilter;
        return this;
    }

    /**
     * The setup is actually executed before playback
     */
    public GSYVideoOptionBuilder setSetUpLazy(boolean setUpLazy) {
        this.mSetUpLazy = setUpLazy;
        return this;
    }

    public GSYVideoOptionBuilder setFullHideActionBar(boolean actionBar) {
        this.mActionBar = actionBar;
        return this;
    }

    public GSYVideoOptionBuilder setFullHideStatusBar(boolean statusBar) {
        this.mStatusBar = statusBar;
        return this;
    }

    public void build(StandardGSYVideoPlayer gsyVideoPlayer) {
        if (mBottomShowProgressDrawable != null && mBottomShowProgressThumbDrawable != null) {
            gsyVideoPlayer.setBottomShowProgressBarDrawable(mBottomShowProgressDrawable, mBottomShowProgressThumbDrawable);
        }
        if (mBottomProgressDrawable != null) {
            gsyVideoPlayer.setBottomProgressBarDrawable(mBottomProgressDrawable);
        }
        if (mVolumeProgressDrawable != null) {
            gsyVideoPlayer.setDialogVolumeProgressBar(mVolumeProgressDrawable);
        }

        if (mDialogProgressBarDrawable != null) {
            gsyVideoPlayer.setDialogProgressBar(mDialogProgressBarDrawable);
        }

        if (mDialogProgressHighLightColor > 0 && mDialogProgressNormalColor > 0) {
            gsyVideoPlayer.setDialogProgressColor(mDialogProgressHighLightColor, mDialogProgressNormalColor);
        }

        build((GSYBaseVideoPlayer) gsyVideoPlayer);
    }

    public void build(GSYBaseVideoPlayer gsyVideoPlayer) {
        gsyVideoPlayer.setPlayTag(mPlayTag);
        gsyVideoPlayer.setPlayPosition(mPlayPosition);

        gsyVideoPlayer.setThumbPlay(mThumbPlay);

        if (mThumbImageView != null) {
            gsyVideoPlayer.setThumbImageView(mThumbImageView);
        }

        gsyVideoPlayer.setNeedLockFull(mNeedLockFull);

        if (mLockClickListener != null) {
            gsyVideoPlayer.setLockClickListener(mLockClickListener);
        }

        gsyVideoPlayer.setDismissControlTime(mDismissControlTime);


        if (mSeekOnStart > 0) {
            gsyVideoPlayer.setSeekOnStart(mSeekOnStart);
        }

        gsyVideoPlayer.setShowFullAnimation(mShowFullAnimation);
        gsyVideoPlayer.setLooping(mLooping);
        if (mVideoAllCallBack != null) {
            gsyVideoPlayer.setVideoAllCallBack(mVideoAllCallBack);
        }
        if (mGSYVideoProgressListener != null) {
            gsyVideoPlayer.setGSYVideoProgressListener(mGSYVideoProgressListener);
        }
        gsyVideoPlayer.setAutoFullWithSize(mAutoFullWithSize);
        gsyVideoPlayer.setRotateViewAuto(mRotateViewAuto);
        gsyVideoPlayer.setLockLand(mLockLand);
        gsyVideoPlayer.setSpeed(mSpeed, mSounchTouch);
        gsyVideoPlayer.setHideKey(mHideKey);
        gsyVideoPlayer.setIsTouchWiget(mIsTouchWiget);
        gsyVideoPlayer.setIsTouchWigetFull(mIsTouchWigetFull);
        gsyVideoPlayer.setNeedShowWifiTip(mNeedShowWifiTip);
        gsyVideoPlayer.setEffectFilter(mEffectFilter);
        gsyVideoPlayer.setStartAfterPrepared(mStartAfterPrepared);
        gsyVideoPlayer.setReleaseWhenLossAudio(mReleaseWhenLossAudio);
        gsyVideoPlayer.setFullHideActionBar(mActionBar);
        gsyVideoPlayer.setFullHideStatusBar(mStatusBar);
        if (mEnlargeImageRes > 0) {
            gsyVideoPlayer.setEnlargeImageRes(mEnlargeImageRes);
        }
        if (mShrinkImageRes > 0) {
            gsyVideoPlayer.setShrinkImageRes(mShrinkImageRes);
        }
        gsyVideoPlayer.setShowPauseCover(mShowPauseCover);
        gsyVideoPlayer.setSeekRatio(mSeekRatio);
        gsyVideoPlayer.setRotateWithSystem(mRotateWithSystem);
        if (mSetUpLazy) {
            gsyVideoPlayer.setUpLazy(mUrl, mCacheWithPlay, mCachePath, mMapHeadData, mVideoTitle);
        } else {
            gsyVideoPlayer.setUp(mUrl, mCacheWithPlay, mCachePath, mMapHeadData, mVideoTitle);
        }
    }

}
