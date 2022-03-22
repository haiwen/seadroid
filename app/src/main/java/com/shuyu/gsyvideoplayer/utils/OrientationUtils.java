package com.shuyu.gsyvideoplayer.utils;

import android.app.Activity;
import android.content.pm.ActivityInfo;
import android.provider.Settings;
import android.view.OrientationEventListener;

import com.shuyu.gsyvideoplayer.video.base.GSYBaseVideoPlayer;

/**
 * Logic for handling screen rotation
 */

public class OrientationUtils {

    private Activity activity;
    private GSYBaseVideoPlayer gsyVideoPlayer;
    private OrientationEventListener orientationEventListener;

    private int screenType = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
    private int mIsLand;

    private boolean mClick = false;
    private boolean mClickLand = false;
    private boolean mClickPort;
    private boolean mEnable = true;
    //Whether to follow the system
    private boolean mRotateWithSystem = true;

    /**
     * @param activity
     * @param gsyVideoPlayer
     */
    public OrientationUtils(Activity activity, GSYBaseVideoPlayer gsyVideoPlayer) {
        this.activity = activity;
        this.gsyVideoPlayer = gsyVideoPlayer;
        init();
    }

    private void init() {
        orientationEventListener = new OrientationEventListener(activity.getApplicationContext()) {
            @Override
            public void onOrientationChanged(int rotation) {
                boolean autoRotateOn = (Settings.System.getInt(activity.getContentResolver(), Settings.System.ACCELEROMETER_ROTATION, 0) == 1);
                if (!autoRotateOn && mRotateWithSystem) {
                    return;
                }
                if (gsyVideoPlayer != null && gsyVideoPlayer.isVerticalFullByVideoSize()) {
                    return;
                }
                // set vertical screen
                if (((rotation >= 0) && (rotation <= 30)) || (rotation >= 330)) {
                    if (mClick) {
                        if (mIsLand > 0 && !mClickLand) {
                            return;
                        } else {
                            mClickPort = true;
                            mClick = false;
                            mIsLand = 0;
                        }
                    } else {
                        if (mIsLand > 0) {
                            screenType = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
                            activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
                            if (gsyVideoPlayer.getFullscreenButton() != null) {
                                if (gsyVideoPlayer.isIfCurrentIsFullscreen()) {
                                    gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getShrinkImageRes());
                                } else {
                                    gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getEnlargeImageRes());
                                }
                            }
                            mIsLand = 0;
                            mClick = false;
                        }
                    }
                }
                // set horizontal screen
                else if (((rotation >= 230) && (rotation <= 310))) {
                    if (mClick) {
                        if (!(mIsLand == 1) && !mClickPort) {
                            return;
                        } else {
                            mClickLand = true;
                            mClick = false;
                            mIsLand = 1;
                        }
                    } else {
                        if (!(mIsLand == 1)) {
                            screenType = ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE;
                            activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
                            if (gsyVideoPlayer.getFullscreenButton() != null) {
                                gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getShrinkImageRes());
                            }
                            mIsLand = 1;
                            mClick = false;
                        }
                    }
                }
                // set reverse horizontal screen
                else if (rotation > 30 && rotation < 95) {
                    if (mClick) {
                        if (!(mIsLand == 2) && !mClickPort) {
                            return;
                        } else {
                            mClickLand = true;
                            mClick = false;
                            mIsLand = 2;
                        }
                    } else if (!(mIsLand == 2)) {
                        screenType = ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE;
                        activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_REVERSE_LANDSCAPE);
                        if (gsyVideoPlayer.getFullscreenButton() != null) {
                            gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getShrinkImageRes());
                        }
                        mIsLand = 2;
                        mClick = false;
                    }
                }
            }
        };
        orientationEventListener.enable();
    }

    /**
     * The logic of click switching, for example, when the vertical screen is clicked, it will switch to the horizontal screen and will not be affected by the screen
     */
    public void resolveByClick() {
        if (mIsLand == 0 && gsyVideoPlayer != null && gsyVideoPlayer.isVerticalFullByVideoSize()) {
            return;
        }
        mClick = true;
        if (mIsLand == 0) {
            screenType = ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE;
            activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
            if (gsyVideoPlayer.getFullscreenButton() != null) {
                gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getShrinkImageRes());
            }
            mIsLand = 1;
            mClickLand = false;
        } else {
            screenType = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
            activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
            if (gsyVideoPlayer.getFullscreenButton() != null) {
                if (gsyVideoPlayer.isIfCurrentIsFullscreen()) {
                    gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getShrinkImageRes());
                } else {
                    gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getEnlargeImageRes());
                }
            }
            mIsLand = 0;
            mClickPort = false;
        }

    }

    /**
     * The style judgments returned by the list. Because the immediate rotation will cause the interface to jump
     */
    public int backToProtVideo() {
        if (mIsLand > 0) {
            mClick = true;
            activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
            if (gsyVideoPlayer != null && gsyVideoPlayer.getFullscreenButton() != null)
                gsyVideoPlayer.getFullscreenButton().setImageResource(gsyVideoPlayer.getEnlargeImageRes());
            mIsLand = 0;
            mClickPort = false;
            return 500;
        }
        return 0;
    }


    public boolean isEnable() {
        return mEnable;
    }

    public void setEnable(boolean enable) {
        this.mEnable = enable;
        if (mEnable) {
            orientationEventListener.enable();
        } else {
            orientationEventListener.disable();
        }
    }

    public void releaseListener() {
        if (orientationEventListener != null) {
            orientationEventListener.disable();
        }
    }

    public boolean isClick() {
        return mClick;
    }

    public void setClick(boolean Click) {
        this.mClick = mClick;
    }

    public boolean isClickLand() {
        return mClickLand;
    }

    public void setClickLand(boolean ClickLand) {
        this.mClickLand = ClickLand;
    }

    public int getIsLand() {
        return mIsLand;
    }

    public void setIsLand(int IsLand) {
        this.mIsLand = IsLand;
    }


    public boolean isClickPort() {
        return mClickPort;
    }

    public void setClickPort(boolean ClickPort) {
        this.mClickPort = ClickPort;
    }

    public int getScreenType() {
        return screenType;
    }

    public void setScreenType(int screenType) {
        this.screenType = screenType;
    }


    public boolean isRotateWithSystem() {
        return mRotateWithSystem;
    }

    /**
     * Whether to update the system rotation, if false, the system will also rotate if it is forbidden to rotate
     *
     * @param rotateWithSystem default true
     */
    public void setRotateWithSystem(boolean rotateWithSystem) {
        this.mRotateWithSystem = rotateWithSystem;
    }
}
