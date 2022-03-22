package com.shuyu.gsyvideoplayer.render;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Matrix;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import com.shuyu.gsyvideoplayer.listener.GSYVideoShotListener;
import com.shuyu.gsyvideoplayer.listener.GSYVideoShotSaveListener;
import com.shuyu.gsyvideoplayer.render.glrender.GSYVideoGLViewBaseRender;
import com.shuyu.gsyvideoplayer.render.view.GSYSurfaceView;
import com.shuyu.gsyvideoplayer.render.view.GSYTextureView;
import com.shuyu.gsyvideoplayer.render.view.GSYVideoGLView;
import com.shuyu.gsyvideoplayer.render.view.IGSYRenderView;
import com.shuyu.gsyvideoplayer.render.view.listener.IGSYSurfaceListener;
import com.shuyu.gsyvideoplayer.utils.GSYVideoType;
import com.shuyu.gsyvideoplayer.utils.MeasureHelper;

import java.io.File;

/**
 * render draws intermediate controls
 */

public class GSYRenderView {

    protected IGSYRenderView mShowView;

    /*************************RenderView function start *************************/
    public void requestLayout() {
        if (mShowView != null) {
            mShowView.getRenderView().requestLayout();
        }
    }

    public float getRotation() {
        return mShowView.getRenderView().getRotation();
    }

    public void setRotation(float rotation) {
        if (mShowView != null)
            mShowView.getRenderView().setRotation(rotation);
    }

    public void invalidate() {
        if (mShowView != null)
            mShowView.getRenderView().invalidate();
    }

    public int getWidth() {
        return (mShowView != null) ? mShowView.getRenderView().getWidth() : 0;
    }

    public int getHeight() {
        return (mShowView != null) ? mShowView.getRenderView().getHeight() : 0;
    }

    public View getShowView() {
        if (mShowView != null)
            return mShowView.getRenderView();
        return null;
    }

    public ViewGroup.LayoutParams getLayoutParams() {
        return mShowView.getRenderView().getLayoutParams();
    }

    public void setLayoutParams(ViewGroup.LayoutParams layoutParams) {
        if (mShowView != null)
            mShowView.getRenderView().setLayoutParams(layoutParams);
    }

    /**
     * Add a playback view
     */
    public void addView(final Context context, final ViewGroup textureViewContainer, final int rotate,
                        final IGSYSurfaceListener gsySurfaceListener,
                        final MeasureHelper.MeasureFormVideoParamsListener videoParamsListener,
                        final GSYVideoGLView.ShaderInterface effect, final float[] transform,
                        final GSYVideoGLViewBaseRender customRender, int mode) {
        if (GSYVideoType.getRenderType() == GSYVideoType.SUFRACE) {
            mShowView = GSYSurfaceView.addSurfaceView(context, textureViewContainer, rotate, gsySurfaceListener, videoParamsListener);
        } else if (GSYVideoType.getRenderType() == GSYVideoType.GLSURFACE) {
            mShowView = GSYVideoGLView.addGLView(context, textureViewContainer, rotate, gsySurfaceListener, videoParamsListener, effect, transform, customRender, mode);
        } else {
            mShowView = GSYTextureView.addTextureView(context, textureViewContainer, rotate, gsySurfaceListener, videoParamsListener);
        }
    }

    /*************************RenderView function end *************************/

    /*************************ShowView function start *************************/

    /**
     * Mainly for TextureView, set the rotation
     */
    public void setTransform(Matrix transform) {
        if (mShowView != null)
            mShowView.setRenderTransform(transform);
    }

    /**
     * Initialize bitmap on pause
     */
    public Bitmap initCover() {
        if (mShowView != null)
            return mShowView.initCover();
        return null;
    }

    public Bitmap initCoverHigh() {
        if (mShowView != null)
            return mShowView.initCoverHigh();
        return null;
    }

    /**
     * get screenshot
     */
    public void taskShotPic(GSYVideoShotListener gsyVideoShotListener) {
        this.taskShotPic(gsyVideoShotListener, false);
    }

    public void taskShotPic(GSYVideoShotListener gsyVideoShotListener, boolean shotHigh) {
        if (mShowView != null)
            mShowView.taskShotPic(gsyVideoShotListener, shotHigh);
    }

    /**
     * save screenshot
     */
    public void saveFrame(final File file, GSYVideoShotSaveListener gsyVideoShotSaveListener) {
        saveFrame(file, false, gsyVideoShotSaveListener);
    }

    /**
     * save screenshot
     *
     * @param high Do you need HD
     */
    public void saveFrame(final File file, final boolean high, final GSYVideoShotSaveListener gsyVideoShotSaveListener) {
        if (mShowView != null)
            mShowView.saveFrame(file, high, gsyVideoShotSaveListener);
    }

    /**
     * Mainly for GL
     */
    public void onResume() {
        if (mShowView != null)
            mShowView.onRenderResume();
    }

    /**
     * Mainly for GL
     */
    public void onPause() {
        if (mShowView != null)
            mShowView.onRenderPause();
    }

    /**
     * Mainly for GL
     */
    public void releaseAll() {
        if (mShowView != null)
            mShowView.releaseRenderAll();
    }

    /**
     * Mainly for GL
     */
    public void setGLRenderMode(int mode) {
        if (mShowView != null)
            mShowView.setRenderMode(mode);
    }

    /**
     * Custom GL renderer
     */
    public void setGLRenderer(GSYVideoGLViewBaseRender renderer) {
        if (mShowView != null)
            mShowView.setGLRenderer(renderer);
    }

    /**
     * Screen matrix effect in GL mode
     *
     * @param matrixGL 16 bit length
     */
    public void setMatrixGL(float[] matrixGL) {
        if (mShowView != null)
            mShowView.setGLMVPMatrix(matrixGL);
    }

    /**
     * Set filter effects
     */
    public void setEffectFilter(GSYVideoGLView.ShaderInterface effectFilter) {
        if (mShowView != null)
            mShowView.setGLEffectFilter(effectFilter);
    }


    /*************************ShowView function end *************************/


    /*************************common function *************************/

    public static void addToParent(ViewGroup textureViewContainer, View render) {
        int params = getTextureParams();
        if (textureViewContainer instanceof RelativeLayout) {
            RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(params, params);
            layoutParams.addRule(RelativeLayout.CENTER_IN_PARENT);
            textureViewContainer.addView(render, layoutParams);
        } else if (textureViewContainer instanceof FrameLayout) {
            FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(params, params);
            layoutParams.gravity = Gravity.CENTER;
            textureViewContainer.addView(render, layoutParams);
        }
    }

    /**
     * Get layout parameters
     *
     * @return
     */
    public static int getTextureParams() {
        boolean typeChanged = (GSYVideoType.getShowType() != GSYVideoType.SCREEN_TYPE_DEFAULT);
        return (typeChanged) ? ViewGroup.LayoutParams.WRAP_CONTENT : ViewGroup.LayoutParams.MATCH_PARENT;
    }

}
