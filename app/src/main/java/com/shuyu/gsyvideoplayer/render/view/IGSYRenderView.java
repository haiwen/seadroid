package com.shuyu.gsyvideoplayer.render.view;


import android.graphics.Bitmap;
import android.graphics.Matrix;
import android.view.View;

import com.shuyu.gsyvideoplayer.listener.GSYVideoShotListener;
import com.shuyu.gsyvideoplayer.listener.GSYVideoShotSaveListener;
import com.shuyu.gsyvideoplayer.render.glrender.GSYVideoGLViewBaseRender;
import com.shuyu.gsyvideoplayer.render.view.listener.IGSYSurfaceListener;
import com.shuyu.gsyvideoplayer.utils.MeasureHelper;

import java.io.File;


public interface IGSYRenderView {

    IGSYSurfaceListener getIGSYSurfaceListener();

    /**
     * SurfaceSurface change Listener
     */
    void setIGSYSurfaceListener(IGSYSurfaceListener surfaceListener);

    int getSizeH();

    int getSizeW();

    View getRenderView();

    void setVideoParamsListener(MeasureHelper.MeasureFormVideoParamsListener listener);

    void taskShotPic(GSYVideoShotListener gsyVideoShotListener, boolean shotHigh);

    void saveFrame(final File file, final boolean high, final GSYVideoShotSaveListener gsyVideoShotSaveListener);

    Bitmap initCover();

    Bitmap initCoverHigh();

    void onRenderResume();

    void onRenderPause();

    void releaseRenderAll();

    void setRenderMode(int mode);

    void setRenderTransform(Matrix transform);

    void setGLRenderer(GSYVideoGLViewBaseRender renderer);

    void setGLMVPMatrix(float[] MVPMatrix);

    void setGLEffectFilter(GSYVideoGLView.ShaderInterface effectFilter);

}
