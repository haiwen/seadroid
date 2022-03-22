package com.shuyu.gsyvideoplayer.video.base;

import android.content.Context;
import android.view.Surface;

import com.shuyu.gsyvideoplayer.listener.GSYMediaPlayerListener;
import com.shuyu.gsyvideoplayer.player.IPlayerManager;

import java.io.File;
import java.util.Map;

/**
 * Interface between Manager and View
 */

public interface GSYVideoViewBridge {

    GSYMediaPlayerListener listener();

    GSYMediaPlayerListener lastListener();

    void setListener(GSYMediaPlayerListener listener);

    void setLastListener(GSYMediaPlayerListener lastListener);

    String getPlayTag();
    void setPlayTag(String playTag);
    int getPlayPosition();
    void setPlayPosition(int playPosition);

    /**
     * start ready to play
     *
     * @param url         play url
     */
    void prepare(final String url, final Map<String, String> mapHeadData, boolean loop, float speed, boolean cache, File cachePath);

    IPlayerManager getPlayer();

    int getBufferedPercentage();

    void releaseMediaPlayer();

    void setCurrentVideoHeight(int currentVideoHeight);

    void setCurrentVideoWidth(int currentVideoWidth);

    int getCurrentVideoWidth();

    int getCurrentVideoHeight();

    void setDisplay(Surface holder);

    void releaseSurface(Surface surface);

    int getLastState();

    void setLastState(int lastState);

    boolean isCacheFile();

    boolean cachePreview(Context context, File cacheDir, String url);

    void clearCache(Context context, File cacheDir, String url);

    long getNetSpeed();

    void setSpeed(float speed, boolean soundTouch);

    void setSpeedPlaying(float speed, boolean soundTouch);

    int getRotateInfoFlag();

    void start();

    void stop();

    void pause();

    int getVideoWidth();

    int getVideoHeight();

    boolean isPlaying();

    void seekTo(long time);

    long getCurrentPosition();

    long getDuration();

    int getVideoSarNum();

    int getVideoSarDen();

    boolean isSurfaceSupportLockCanvas();
}
