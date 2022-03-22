package com.shuyu.gsyvideoplayer.listener;


import java.io.File;

/**
 * Screenshot to save the result
 */

public interface GSYVideoShotSaveListener {
    void result(boolean success, File file);
}
