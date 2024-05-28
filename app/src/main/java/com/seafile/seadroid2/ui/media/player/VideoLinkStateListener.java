package com.seafile.seadroid2.ui.media.player;

/**
 * get video link state listener
 */
public interface VideoLinkStateListener {
    void onSuccess(String fileLink);

    void onError(String errMsg);
}
