package com.shuyu.gsyvideoplayer.listener;

import java.io.File;

/**
 * Listener created for GIf images
 */

public interface GSYVideoGifSaveListener {

    void process(int curPosition, int total);

    void result(boolean success, File file);
}
