package com.shuyu.gsyvideoplayer.listener;
public interface VideoAllCallBack {

    void onStartPrepared(String url, Object... objects);

    void onPrepared(String url, Object... objects);

    void onClickStartIcon(String url, Object... objects);

    void onClickStartError(String url, Object... objects);

    void onClickStop(String url, Object... objects);

    void onClickStopFullscreen(String url, Object... objects);

    void onClickResume(String url, Object... objects);

    void onClickResumeFullscreen(String url, Object... objects);

    void onClickSeekbar(String url, Object... objects);

    void onClickSeekbarFullscreen(String url, Object... objects);

    void onAutoComplete(String url, Object... objects);

    void onEnterFullscreen(String url, Object... objects);

    void onQuitFullscreen(String url, Object... objects);

    void onQuitSmallWidget(String url, Object... objects);

    void onEnterSmallWidget(String url, Object... objects);

    void onTouchScreenSeekVolume(String url, Object... objects);

    void onTouchScreenSeekPosition(String url, Object... objects);

    void onTouchScreenSeekLight(String url, Object... objects);

    void onPlayError(String url, Object... objects);

    void onClickStartThumb(String url, Object... objects);

    void onClickBlank(String url, Object... objects);

    void onClickBlankFullscreen(String url, Object... objects);


}
