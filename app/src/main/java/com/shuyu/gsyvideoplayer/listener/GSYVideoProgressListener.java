package com.shuyu.gsyvideoplayer.listener;

/**
 * progress callback
 */
public interface GSYVideoProgressListener {
    /**
     * @param progress Current playback progress
     * @param secProgress Current memory buffer progress
     * @param currentPosition current playback position
     * @param duration total duration
     */
    void onProgress(int progress, int secProgress, int currentPosition, int duration);
}
