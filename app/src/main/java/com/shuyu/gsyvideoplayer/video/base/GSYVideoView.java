package com.shuyu.gsyvideoplayer.video.base;

import android.app.Activity;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Rect;
import android.graphics.RectF;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.support.annotation.AttrRes;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.AttributeSet;
import android.view.InflateException;
import android.view.Surface;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;

import com.seafile.seadroid2.R;
import com.shuyu.gsyvideoplayer.listener.GSYMediaPlayerListener;
import com.shuyu.gsyvideoplayer.listener.VideoAllCallBack;
import com.shuyu.gsyvideoplayer.utils.CommonUtil;
import com.shuyu.gsyvideoplayer.utils.Debuger;
import com.shuyu.gsyvideoplayer.utils.NetInfoModule;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import static com.shuyu.gsyvideoplayer.utils.CommonUtil.getTextSpeed;

/**
 * Related layers such as video callback and state processing
 */

public abstract class GSYVideoView extends GSYTextureRenderView implements GSYMediaPlayerListener {

    public static final int CURRENT_STATE_NORMAL = 0;
    public static final int CURRENT_STATE_PREPAREING = 1;
    public static final int CURRENT_STATE_PLAYING = 2;
    public static final int CURRENT_STATE_PLAYING_BUFFERING_START = 3;
    public static final int CURRENT_STATE_PAUSE = 5;
    public static final int CURRENT_STATE_AUTO_COMPLETE = 6;
    public static final int CURRENT_STATE_ERROR = 7;

    public static final int CHANGE_DELAY_TIME = 2000;

    protected int mCurrentState = -1;

    protected int mPlayPosition = -22;

    protected int mScreenWidth;

    protected int mScreenHeight;

    protected int mBufferPoint;

    protected int mBackUpPlayingBufferState = -1;

    protected long mSeekOnStart = -1;

    protected long mCurrentPosition;

    protected long mSaveChangeViewTIme = 0;

    protected float mSpeed = 1;

    protected boolean mCache = false;

    protected boolean mIfCurrentIsFullscreen = false;

    protected boolean mLooping = false;

    protected boolean mHadPlay = false;

    protected boolean mNetChanged = false;

    protected boolean mSoundTouch = false;

    protected boolean mShowPauseCover = false;

    protected boolean mPauseBeforePrepared = false;

    protected boolean mStartAfterPrepared = true;

    //Prepared
    protected boolean mHadPrepared = false;

    protected boolean mReleaseWhenLossAudio = true;

    protected AudioManager mAudioManager;

    protected String mPlayTag = "";

    protected Context mContext;

    protected String mOriginUrl;

    protected String mUrl;

    protected String mTitle;

    protected String mNetSate = "NORMAL";

    protected File mCachePath;

    protected VideoAllCallBack mVideoAllCallBack;

    //http request header
    protected Map<String, String> mMapHeadData = new HashMap<>();

    protected NetInfoModule mNetInfoModule;

    public GSYVideoView(@NonNull Context context) {
        super(context);
        init(context);
    }

    public GSYVideoView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init(context);
    }

    public GSYVideoView(@NonNull Context context, @Nullable AttributeSet attrs, @AttrRes int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    public GSYVideoView(Context context, Boolean fullFlag) {
        super(context);
        mIfCurrentIsFullscreen = fullFlag;
        init(context);
    }

    @Override
    protected void showPauseCover() {
        if (mCurrentState == CURRENT_STATE_PAUSE && mFullPauseBitmap != null
                && !mFullPauseBitmap.isRecycled() && mShowPauseCover
                && mSurface != null && mSurface.isValid()) {
            if (getGSYVideoManager().isSurfaceSupportLockCanvas()) {
                try {
                    RectF rectF = new RectF(0, 0, mTextureView.getWidth(), mTextureView.getHeight());
                    Canvas canvas = mSurface.lockCanvas(new Rect(0, 0, mTextureView.getWidth(), mTextureView.getHeight()));
                    if (canvas != null) {
                        canvas.drawBitmap(mFullPauseBitmap, null, rectF, null);
                        mSurface.unlockCanvasAndPost(canvas);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

    }

    @Override
    protected void releasePauseCover() {
        try {
            if (mCurrentState != CURRENT_STATE_PAUSE && mFullPauseBitmap != null
                    && !mFullPauseBitmap.isRecycled() && mShowPauseCover) {
                mFullPauseBitmap.recycle();
                mFullPauseBitmap = null;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public int getCurrentVideoWidth() {
        if (getGSYVideoManager() != null) {
            return getGSYVideoManager().getVideoWidth();
        }
        return 0;
    }

    @Override
    public int getCurrentVideoHeight() {
        if (getGSYVideoManager() != null) {
            return getGSYVideoManager().getVideoHeight();
        }
        return 0;
    }

    @Override
    public int getVideoSarNum() {
        if (getGSYVideoManager() != null) {
            return getGSYVideoManager().getVideoSarNum();
        }
        return 0;
    }

    @Override
    public int getVideoSarDen() {
        if (getGSYVideoManager() != null) {
            return getGSYVideoManager().getVideoSarDen();
        }
        return 0;
    }

    protected void updatePauseCover() {
        if ((mFullPauseBitmap == null || mFullPauseBitmap.isRecycled()) && mShowPauseCover) {
            try {
                initCover();
            } catch (Exception e) {
                e.printStackTrace();
                mFullPauseBitmap = null;
            }
        }
    }

    protected Context getActivityContext() {
        return CommonUtil.getActivityContext(getContext());
    }

    protected void init(Context context) {

        if (getActivityContext() != null) {
            this.mContext = getActivityContext();
        } else {
            this.mContext = context;
        }

        initInflate(mContext);

        mTextureViewContainer = (ViewGroup) findViewById(R.id.surface_container);
        if (isInEditMode())
            return;
        mScreenWidth = getActivityContext().getResources().getDisplayMetrics().widthPixels;
        mScreenHeight = getActivityContext().getResources().getDisplayMetrics().heightPixels;
        mAudioManager = (AudioManager) getActivityContext().getApplicationContext().getSystemService(Context.AUDIO_SERVICE);

    }

    protected void initInflate(Context context) {
        try {
            View.inflate(context, getLayoutId(), this);
        } catch (InflateException e) {
            if (e.toString().contains("GSYImageCover")) {
                Debuger.printfError("********************\n" +
                        "*****   注意   *****" +
                        "********************\n" +
                        "*This version needs to clear the GSYImageCover in the layout file\n" +
                        "****  Attention  ***\n" +
                        "*Please remove GSYImageCover from Layout in this Version\n" +
                        "********************\n");
                e.printStackTrace();
                throw new InflateException("This version needs to clear the GSYImageCover in the layout file，please remove GSYImageCover from your layout");
            } else {
                e.printStackTrace();
            }
        }
    }

    /**
     * start play logic
     */
    protected void startButtonLogic() {
        if (mVideoAllCallBack != null && mCurrentState == CURRENT_STATE_NORMAL) {
            Debuger.printfLog("onClickStartIcon");
            mVideoAllCallBack.onClickStartIcon(mOriginUrl, mTitle, this);
        } else if (mVideoAllCallBack != null) {
            Debuger.printfLog("onClickStartError");
            mVideoAllCallBack.onClickStartError(mOriginUrl, mTitle, this);
        }
        prepareVideo();
    }

    /**
     * Start state video playback
     */
    protected void prepareVideo() {
        startPrepare();
    }

    protected void startPrepare() {
        if (getGSYVideoManager().listener() != null) {
            getGSYVideoManager().listener().onCompletion();
        }
        if (mVideoAllCallBack != null) {
            Debuger.printfLog("onStartPrepared");
            mVideoAllCallBack.onStartPrepared(mOriginUrl, mTitle, this);
        }
        getGSYVideoManager().setListener(this);
        getGSYVideoManager().setPlayTag(mPlayTag);
        getGSYVideoManager().setPlayPosition(mPlayPosition);
        mAudioManager.requestAudioFocus(onAudioFocusChangeListener, AudioManager.STREAM_MUSIC, AudioManager.AUDIOFOCUS_GAIN_TRANSIENT);
        ((Activity) getActivityContext()).getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        mBackUpPlayingBufferState = -1;
        getGSYVideoManager().prepare(mUrl, (mMapHeadData == null) ? new HashMap<String, String>() : mMapHeadData, mLooping, mSpeed, mCache, mCachePath);
        setStateAndUi(CURRENT_STATE_PREPAREING);
    }

    /**
     * Monitor whether other external multimedia starts to play
     */
    protected AudioManager.OnAudioFocusChangeListener onAudioFocusChangeListener = new AudioManager.OnAudioFocusChangeListener() {
        @Override
        public void onAudioFocusChange(int focusChange) {
            switch (focusChange) {
                case AudioManager.AUDIOFOCUS_GAIN:
                    onGankAudio();
                    break;
                case AudioManager.AUDIOFOCUS_LOSS:
                    onLossAudio();
                    break;
                case AudioManager.AUDIOFOCUS_LOSS_TRANSIENT:
                    onLossTransientAudio();
                    break;
                case AudioManager.AUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK:
                    onLossTransientCanDuck();
                    break;
            }
        }
    };

    /**
     * get Audio Focus
     */
    protected void onGankAudio() {
    }

    protected void onLossAudio() {
        this.post(new Runnable() {
            public void run() {
                if (GSYVideoView.this.mReleaseWhenLossAudio) {
                    GSYVideoView.this.releaseVideos();
                } else {
                    GSYVideoView.this.onVideoPause();
                }

            }
        });
    }

    protected void onLossTransientAudio() {
        try {
            this.onVideoPause();
        } catch (Exception var2) {
            var2.printStackTrace();
        }

    }

    protected void onLossTransientCanDuck() {
    }


    /**
     * Set playback URL
     *
     * @param title         title
     * @return
     */
    public boolean setUp(String url, boolean cacheWithPlay, String title) {
        return setUp(url, cacheWithPlay, ((File) null), title);
    }


    /**
     * Set playback URL
     *
     * @param title         title
     * @return
     */
    public boolean setUp(String url, boolean cacheWithPlay, File cachePath, Map<String, String> mapHeadData, String title) {
        if (setUp(url, cacheWithPlay, cachePath, title)) {
            if (this.mMapHeadData != null) {
                this.mMapHeadData.clear();
            } else {
                this.mMapHeadData = new HashMap<>();
            }
            if (mapHeadData != null) {
                this.mMapHeadData.putAll(mapHeadData);
            }
            return true;
        }
        return false;
    }

    /**
     * Set playback URL
     *
     * @param title         title
     * @return
     */
    public boolean setUp(String url, boolean cacheWithPlay, File cachePath, String title) {
        return setUp(url, cacheWithPlay, cachePath, title, true);
    }

    /**
     * Set playback URL
     *
     * @return
     */
    protected boolean setUp(String url, boolean cacheWithPlay, File cachePath, String title, boolean changeState) {
        mCache = cacheWithPlay;
        mCachePath = cachePath;
        mOriginUrl = url;
        if (isCurrentMediaListener() &&
                (System.currentTimeMillis() - mSaveChangeViewTIme) < CHANGE_DELAY_TIME)
            return false;
        mCurrentState = CURRENT_STATE_NORMAL;
        this.mUrl = url;
        this.mTitle = title;
        if (changeState)
            setStateAndUi(CURRENT_STATE_NORMAL);
        return true;
    }


    public void onVideoReset() {
        setStateAndUi(CURRENT_STATE_NORMAL);
    }

    @Override
    public void onVideoPause() {
        if (mCurrentState == CURRENT_STATE_PREPAREING) {
            mPauseBeforePrepared = true;
        }
        try {
            if (getGSYVideoManager() != null &&
                    getGSYVideoManager().isPlaying()) {
                setStateAndUi(CURRENT_STATE_PAUSE);
                mCurrentPosition = getGSYVideoManager().getCurrentPosition();
                if (getGSYVideoManager() != null)
                    getGSYVideoManager().pause();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onVideoResume() {
        onVideoResume(true);
    }

    @Override
    public void onVideoResume(boolean seek) {
        mPauseBeforePrepared = false;
        if (mCurrentState == CURRENT_STATE_PAUSE) {
            try {
                if (mCurrentPosition > 0 && getGSYVideoManager() != null) {
                    if (seek) {
                        getGSYVideoManager().seekTo(mCurrentPosition);
                    }
                    getGSYVideoManager().start();
                    setStateAndUi(CURRENT_STATE_PLAYING);
                    if (mAudioManager != null && !mReleaseWhenLossAudio) {
                        mAudioManager.requestAudioFocus(onAudioFocusChangeListener, AudioManager.STREAM_MUSIC, AudioManager.AUDIOFOCUS_GAIN_TRANSIENT);
                    }
                    mCurrentPosition = 0;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    protected void netWorkErrorLogic() {
        final long currentPosition = getCurrentPositionWhenPlaying();
        Debuger.printfError("******* Net State Changed. renew player to connect *******" + currentPosition);
        getGSYVideoManager().releaseMediaPlayer();
        postDelayed(new Runnable() {
            @Override
            public void run() {
                setSeekOnStart(currentPosition);
                startPlayLogic();
            }
        }, 500);
    }


    protected void deleteCacheFileWhenError() {
        clearCurrentCache();
        Debuger.printfError("Link Or mCache Error, Please Try Again " + mOriginUrl);
        if (mCache) {
            Debuger.printfError("mCache Link " + mUrl);
        }
        mUrl = mOriginUrl;
    }

    @Override
    public void onPrepared() {

        if (mCurrentState != CURRENT_STATE_PREPAREING) return;

        mHadPrepared = true;

        if (mVideoAllCallBack != null && isCurrentMediaListener()) {
            Debuger.printfLog("onPrepared");
            mVideoAllCallBack.onPrepared(mOriginUrl, mTitle, this);
        }

        if (!mStartAfterPrepared) {
            setStateAndUi(CURRENT_STATE_PAUSE);
            return;
        }

        startAfterPrepared();
    }

    @Override
    public void onAutoCompletion() {
        setStateAndUi(CURRENT_STATE_AUTO_COMPLETE);

        mSaveChangeViewTIme = 0;
        mCurrentPosition = 0;

        if (mTextureViewContainer.getChildCount() > 0) {
            mTextureViewContainer.removeAllViews();
        }

        if (!mIfCurrentIsFullscreen)
            getGSYVideoManager().setLastListener(null);
        mAudioManager.abandonAudioFocus(onAudioFocusChangeListener);
        ((Activity) getActivityContext()).getWindow().clearFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);

        releaseNetWorkState();

        if (mVideoAllCallBack != null && isCurrentMediaListener()) {
            Debuger.printfLog("onAutoComplete");
            mVideoAllCallBack.onAutoComplete(mOriginUrl, mTitle, this);
        }
    }

    @Override
    public void onCompletion() {
        //make me normal first
        setStateAndUi(CURRENT_STATE_NORMAL);

        mSaveChangeViewTIme = 0;
        mCurrentPosition = 0;

        if (mTextureViewContainer.getChildCount() > 0) {
            mTextureViewContainer.removeAllViews();
        }

        if (!mIfCurrentIsFullscreen) {
            getGSYVideoManager().setListener(null);
            getGSYVideoManager().setLastListener(null);
        }
        getGSYVideoManager().setCurrentVideoHeight(0);
        getGSYVideoManager().setCurrentVideoWidth(0);

        mAudioManager.abandonAudioFocus(onAudioFocusChangeListener);
        ((Activity) getActivityContext()).getWindow().clearFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);

        releaseNetWorkState();

    }

    @Override
    public void onSeekComplete() {

    }

    @Override
    public void onError(int what, int extra) {

        if (mNetChanged) {
            mNetChanged = false;
            netWorkErrorLogic();
            if (mVideoAllCallBack != null) {
                mVideoAllCallBack.onPlayError(mOriginUrl, mTitle, this);
            }
            return;
        }

        if (what != 38 && what != -38) {
            setStateAndUi(CURRENT_STATE_ERROR);
            deleteCacheFileWhenError();
            if (mVideoAllCallBack != null) {
                mVideoAllCallBack.onPlayError(mOriginUrl, mTitle, this);
            }
        }
    }

    @Override
    public void onInfo(int what, int extra) {
        if (what == MediaPlayer.MEDIA_INFO_BUFFERING_START) {
            mBackUpPlayingBufferState = mCurrentState;
            if (mHadPlay && mCurrentState != CURRENT_STATE_PREPAREING && mCurrentState > 0)
                setStateAndUi(CURRENT_STATE_PLAYING_BUFFERING_START);

        } else if (what == MediaPlayer.MEDIA_INFO_BUFFERING_END) {
            if (mBackUpPlayingBufferState != -1) {
                if (mBackUpPlayingBufferState == CURRENT_STATE_PLAYING_BUFFERING_START) {
                    mBackUpPlayingBufferState = CURRENT_STATE_PLAYING;
                }
                if (mHadPlay && mCurrentState != CURRENT_STATE_PREPAREING && mCurrentState > 0)
                    setStateAndUi(mBackUpPlayingBufferState);

                mBackUpPlayingBufferState = -1;
            }
        } else if (what == getGSYVideoManager().getRotateInfoFlag()) {
            mRotate = extra;
            Debuger.printfLog("Video Rotate Info " + extra);
            if (mTextureView != null)
                mTextureView.setRotation(mRotate);
        }
    }

    @Override
    public void onVideoSizeChanged() {
        int mVideoWidth = getGSYVideoManager().getCurrentVideoWidth();
        int mVideoHeight = getGSYVideoManager().getCurrentVideoHeight();
        if (mVideoWidth != 0 && mVideoHeight != 0 && mTextureView != null) {
            mTextureView.requestLayout();
        }
    }

    @Override
    protected void setDisplay(Surface surface) {
        getGSYVideoManager().setDisplay(surface);
    }

    @Override
    protected void releaseSurface(Surface surface) {
        getGSYVideoManager().releaseSurface(surface);
    }

    public void clearCurrentCache() {
        if (getGSYVideoManager().isCacheFile() && mCache) {
            Debuger.printfError("Play Error " + mUrl);
            mUrl = mOriginUrl;
            getGSYVideoManager().clearCache(mContext, mCachePath, mOriginUrl);
        } else if (mUrl.contains("127.0.0.1")) {
            getGSYVideoManager().clearCache(getContext(), mCachePath, mOriginUrl);
        }

    }

    public int getCurrentPositionWhenPlaying() {
        int position = 0;
        if (mCurrentState == CURRENT_STATE_PLAYING || mCurrentState == CURRENT_STATE_PAUSE) {
            try {
                position = (int) getGSYVideoManager().getCurrentPosition();
            } catch (Exception e) {
                e.printStackTrace();
                return position;
            }
        }
        if (position == 0 && mCurrentPosition > 0) {
            return (int) mCurrentPosition;
        }
        return position;
    }

    public int getDuration() {
        int duration = 0;
        try {
            duration = (int) getGSYVideoManager().getDuration();
        } catch (Exception e) {
            e.printStackTrace();
            return duration;
        }
        return duration;
    }

    public void release() {
        mSaveChangeViewTIme = 0;
        if (isCurrentMediaListener() &&
                (System.currentTimeMillis() - mSaveChangeViewTIme) > CHANGE_DELAY_TIME) {
            releaseVideos();
        }
    }

    public void startAfterPrepared() {

        if (!mHadPrepared) {
            prepareVideo();
        }

        try {
            if (getGSYVideoManager() != null) {
                getGSYVideoManager().start();
            }

            setStateAndUi(CURRENT_STATE_PLAYING);

            if (getGSYVideoManager() != null && mSeekOnStart > 0) {
                getGSYVideoManager().seekTo(mSeekOnStart);
                mSeekOnStart = 0;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        addTextureView();

        createNetWorkState();

        listenerNetWorkState();

        mHadPlay = true;

        if (mTextureView != null) {
            mTextureView.onResume();
        }

        if (mPauseBeforePrepared) {
            onVideoPause();
            mPauseBeforePrepared = false;
        }
    }

    protected boolean isCurrentMediaListener() {
        return getGSYVideoManager().listener() != null
                && getGSYVideoManager().listener() == this;
    }

    protected void createNetWorkState() {
        if (mNetInfoModule == null) {
            mNetInfoModule = new NetInfoModule(getActivityContext().getApplicationContext(), new NetInfoModule.NetChangeListener() {
                @Override
                public void changed(String state) {
                    if (!mNetSate.equals(state)) {
                        Debuger.printfError("******* change network state ******* " + state);
                        mNetChanged = true;
                    }
                    mNetSate = state;
                }
            });
            mNetSate = mNetInfoModule.getCurrentConnectionType();
        }
    }

    protected void listenerNetWorkState() {
        if (mNetInfoModule != null) {
            mNetInfoModule.onHostResume();
        }
    }

    protected void unListenerNetWorkState() {
        if (mNetInfoModule != null) {
            mNetInfoModule.onHostPause();
        }
    }

    protected void releaseNetWorkState() {
        if (mNetInfoModule != null) {
            mNetInfoModule.onHostPause();
            mNetInfoModule = null;
        }
    }

    /************************* Need to inherit the processing part *************************/

    protected abstract boolean backFromFull(Context context);

    protected abstract void releaseVideos();

    protected abstract void setStateAndUi(int state);

    public abstract GSYVideoViewBridge getGSYVideoManager();

    public abstract int getLayoutId();

    public abstract void startPlayLogic();


    /************************* public interface *************************/

    public int getCurrentState() {
        return mCurrentState;
    }

    public boolean isInPlayingState() {
        return (mCurrentState >= 0 && mCurrentState != CURRENT_STATE_NORMAL
                && mCurrentState != CURRENT_STATE_AUTO_COMPLETE && mCurrentState != CURRENT_STATE_ERROR);
    }

    public String getPlayTag() {
        return mPlayTag;
    }

    public void setPlayTag(String playTag) {
        this.mPlayTag = playTag;
    }


    public int getPlayPosition() {
        return mPlayPosition;
    }

    public void setPlayPosition(int playPosition) {
        this.mPlayPosition = playPosition;
    }

    public long getNetSpeed() {
        return getGSYVideoManager().getNetSpeed();
    }

    public String getNetSpeedText() {
        long speed = getNetSpeed();
        return getTextSpeed(speed);
    }

    public long getSeekOnStart() {
        return mSeekOnStart;
    }

    public void setSeekOnStart(long seekOnStart) {
        this.mSeekOnStart = seekOnStart;
    }

    public int getBuffterPoint() {
        return mBufferPoint;
    }

    public boolean isIfCurrentIsFullscreen() {
        return mIfCurrentIsFullscreen;
    }

    public void setIfCurrentIsFullscreen(boolean ifCurrentIsFullscreen) {
        this.mIfCurrentIsFullscreen = ifCurrentIsFullscreen;
    }

    public boolean isLooping() {
        return mLooping;
    }

    public void setLooping(boolean looping) {
        this.mLooping = looping;
    }


    public void setVideoAllCallBack(VideoAllCallBack mVideoAllCallBack) {
        this.mVideoAllCallBack = mVideoAllCallBack;
    }

    public float getSpeed() {
        return mSpeed;
    }

    public void setSpeed(float speed) {
        setSpeed(speed, false);
    }

    public void setSpeed(float speed, boolean soundTouch) {
        this.mSpeed = speed;
        this.mSoundTouch = soundTouch;
        if (getGSYVideoManager() != null) {
            getGSYVideoManager().setSpeed(speed, soundTouch);
        }
    }

    public void setSpeedPlaying(float speed, boolean soundTouch) {
        setSpeed(speed, soundTouch);
        getGSYVideoManager().setSpeedPlaying(speed, soundTouch);
    }

    public boolean isShowPauseCover() {
        return mShowPauseCover;
    }

    public void setShowPauseCover(boolean showPauseCover) {
        this.mShowPauseCover = showPauseCover;
    }

    /**
     * seekto what you want
     */
    public void seekTo(long position) {
        try {
            if (getGSYVideoManager() != null && position > 0) {
                getGSYVideoManager().seekTo(position);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public boolean isStartAfterPrepared() {
        return mStartAfterPrepared;
    }

    public void setStartAfterPrepared(boolean startAfterPrepared) {
        this.mStartAfterPrepared = startAfterPrepared;
    }

    public boolean isReleaseWhenLossAudio() {
        return mReleaseWhenLossAudio;
    }

    public void setReleaseWhenLossAudio(boolean releaseWhenLossAudio) {
        this.mReleaseWhenLossAudio = releaseWhenLossAudio;
    }

    public Map<String, String> getMapHeadData() {
        return mMapHeadData;
    }

    public void setMapHeadData(Map<String, String> headData) {
        if (headData != null) {
            this.mMapHeadData = headData;
        }
    }
}
