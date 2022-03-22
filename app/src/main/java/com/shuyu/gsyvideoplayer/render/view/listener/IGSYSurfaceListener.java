package com.shuyu.gsyvideoplayer.render.view.listener;

import android.view.Surface;

/**
 * Surface state change callback
 */

public interface IGSYSurfaceListener {
    void onSurfaceAvailable(Surface surface);

    void onSurfaceSizeChanged(Surface surface, int width, int height);

    boolean onSurfaceDestroyed(Surface surface);

    void onSurfaceUpdated(Surface surface);
}
