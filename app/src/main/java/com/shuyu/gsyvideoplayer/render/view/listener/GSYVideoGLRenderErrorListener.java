package com.shuyu.gsyvideoplayer.render.view.listener;

import com.shuyu.gsyvideoplayer.render.glrender.GSYVideoGLViewBaseRender;

/**
 * GL rendering error
 */
public interface GSYVideoGLRenderErrorListener {
    void onError(GSYVideoGLViewBaseRender render, String Error, int code, boolean byChangedRenderError);
}
