package com.shuyu.gsyvideoplayer;


import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;

import com.seafile.seadroid2.R;
import com.shuyu.gsyvideoplayer.listener.GSYMediaPlayerListener;
import com.shuyu.gsyvideoplayer.utils.CommonUtil;
import com.shuyu.gsyvideoplayer.video.base.GSYVideoPlayer;

import static com.shuyu.gsyvideoplayer.utils.CommonUtil.hideNavKey;

/**
 * video management
 */

public class GSYVideoManager extends GSYVideoBaseManager {

    public static final int SMALL_ID = R.id.small_id;

    public static final int FULLSCREEN_ID = R.id.full_id;

    public static String TAG = "GSYVideoManager";

    @SuppressLint("StaticFieldLeak")
    private static GSYVideoManager videoManager;


    private GSYVideoManager() {
        init();
    }

    public static synchronized GSYVideoManager instance() {
        if (videoManager == null) {
            videoManager = new GSYVideoManager();
        }
        return videoManager;
    }

    /**
     * Synchronously create a temporary manager
     */
    public static synchronized GSYVideoManager tmpInstance(GSYMediaPlayerListener listener) {
        GSYVideoManager gsyVideoManager = new GSYVideoManager();
        gsyVideoManager.bufferPoint = videoManager.bufferPoint;
        gsyVideoManager.optionModelList = videoManager.optionModelList;
        gsyVideoManager.playTag = videoManager.playTag;
        gsyVideoManager.currentVideoWidth = videoManager.currentVideoWidth;
        gsyVideoManager.currentVideoHeight = videoManager.currentVideoHeight;
        gsyVideoManager.context = videoManager.context;
        gsyVideoManager.lastState = videoManager.lastState;
        gsyVideoManager.playPosition = videoManager.playPosition;
        gsyVideoManager.timeOut = videoManager.timeOut;
        gsyVideoManager.needMute = videoManager.needMute;
        gsyVideoManager.needTimeOutOther = videoManager.needTimeOutOther;
        gsyVideoManager.setListener(listener);
        return gsyVideoManager;
    }

    /**
     * Replacement manager
     */
    public static synchronized void changeManager(GSYVideoManager gsyVideoManager) {
        videoManager = gsyVideoManager;
    }

    /**
     * Exit full screen, mainly used for return key
     *
     */
    @SuppressWarnings("ResourceType")
    public static boolean backFromWindowFull(Context context) {
        boolean backFrom = false;
        ViewGroup vp = (ViewGroup) (CommonUtil.scanForActivity(context)).findViewById(Window.ID_ANDROID_CONTENT);
        View oldF = vp.findViewById(FULLSCREEN_ID);
        if (oldF != null) {
            backFrom = true;
            hideNavKey(context);
            if (GSYVideoManager.instance().lastListener() != null) {
                GSYVideoManager.instance().lastListener().onBackFullscreen();
            }
        }
        return backFrom;
    }

    public static void releaseAllVideos() {
        if (GSYVideoManager.instance().listener() != null) {
            GSYVideoManager.instance().listener().onCompletion();
        }
        GSYVideoManager.instance().releaseMediaPlayer();
    }


    public static void onPause() {
        if (GSYVideoManager.instance().listener() != null) {
            GSYVideoManager.instance().listener().onVideoPause();
        }
    }

    public static void onResume() {
        if (GSYVideoManager.instance().listener() != null) {
            GSYVideoManager.instance().listener().onVideoResume();
        }
    }


    public static void onResume(boolean seek) {
        if (GSYVideoManager.instance().listener() != null) {
            GSYVideoManager.instance().listener().onVideoResume(seek);
        }
    }

    @SuppressWarnings("ResourceType")
    public static boolean isFullState(Activity activity) {
        ViewGroup vp = (ViewGroup) (CommonUtil.scanForActivity(activity)).findViewById(Window.ID_ANDROID_CONTENT);
        final View full = vp.findViewById(FULLSCREEN_ID);
        GSYVideoPlayer gsyVideoPlayer = null;
        if (full != null) {
            gsyVideoPlayer = (GSYVideoPlayer) full;
        }
        return gsyVideoPlayer != null;
    }

}