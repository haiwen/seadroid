package com.shuyu.gsyvideoplayer.video.base;

import android.content.Context;
import android.graphics.Bitmap;
import android.support.annotation.AttrRes;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.AttributeSet;
import android.view.Surface;
import android.view.TextureView;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import com.shuyu.gsyvideoplayer.render.GSYRenderView;
import com.shuyu.gsyvideoplayer.render.view.GSYVideoGLView;
import com.shuyu.gsyvideoplayer.render.effect.NoEffect;
import com.shuyu.gsyvideoplayer.render.glrender.GSYVideoGLViewBaseRender;
import com.shuyu.gsyvideoplayer.render.view.listener.IGSYSurfaceListener;
import com.shuyu.gsyvideoplayer.utils.GSYVideoType;
import com.shuyu.gsyvideoplayer.utils.MeasureHelper;

/**
 * Draw View
 */

public abstract class GSYTextureRenderView extends FrameLayout implements IGSYSurfaceListener, MeasureHelper.MeasureFormVideoParamsListener {

    protected Surface mSurface;

    protected GSYRenderView mTextureView;

    protected ViewGroup mTextureViewContainer;

    protected Bitmap mFullPauseBitmap;

    protected GSYVideoGLView.ShaderInterface mEffectFilter = new NoEffect();

    protected GSYVideoGLViewBaseRender mRenderer;

    protected float[] mMatrixGL = null;

    protected int mRotate;

    protected int mMode = GSYVideoGLView.MODE_LAYOUT_SIZE;

    public GSYTextureRenderView(@NonNull Context context) {
        super(context);
    }

    public GSYTextureRenderView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public GSYTextureRenderView(@NonNull Context context, @Nullable AttributeSet attrs, @AttrRes int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    /******************** start render  listener****************************/


    @Override
    public void onSurfaceAvailable(Surface surface) {
        pauseLogic(surface, (mTextureView != null && mTextureView.getShowView() instanceof TextureView));
    }

    @Override
    public void onSurfaceSizeChanged(Surface surface, int width, int height) {

    }

    @Override
    public boolean onSurfaceDestroyed(Surface surface) {
        setDisplay(null);
        releaseSurface(surface);
        return true;
    }

    @Override
    public void onSurfaceUpdated(Surface surface) {
        releasePauseCover();
    }


    /******************** end render listener****************************/

    protected void pauseLogic(Surface surface, boolean pauseLogic) {
        mSurface = surface;
        if (pauseLogic)
            showPauseCover();
        setDisplay(mSurface);
    }

    /**
     * Add a playback view
     */
    protected void addTextureView() {
        mTextureView = new GSYRenderView();
        mTextureView.addView(getContext(), mTextureViewContainer, mRotate, this, this, mEffectFilter, mMatrixGL, mRenderer, mMode);
    }

    protected int getTextureParams() {
        boolean typeChanged = (GSYVideoType.getShowType() != GSYVideoType.SCREEN_TYPE_DEFAULT);
        return (typeChanged) ? ViewGroup.LayoutParams.WRAP_CONTENT : ViewGroup.LayoutParams.MATCH_PARENT;
    }

    protected void changeTextureViewShowType() {
        if (mTextureView != null) {
            int params = getTextureParams();
            ViewGroup.LayoutParams layoutParams = mTextureView.getLayoutParams();
            layoutParams.width = params;
            layoutParams.height = params;
            mTextureView.setLayoutParams(layoutParams);
        }
    }

    /**
     * Initialize bitmap on pause
     */
    protected void initCover() {
        if (mTextureView != null) {
            mFullPauseBitmap = mTextureView.initCover();
        }
    }

    protected void setSmallVideoTextureView(OnTouchListener onTouchListener) {
        mTextureViewContainer.setOnTouchListener(onTouchListener);
        mTextureViewContainer.setOnClickListener(null);
        setSmallVideoTextureView();
    }

    public GSYVideoGLView.ShaderInterface getEffectFilter() {
        return mEffectFilter;
    }

    public GSYRenderView getRenderProxy() {
        return mTextureView;
    }

    public void setEffectFilter(GSYVideoGLView.ShaderInterface effectFilter) {
        this.mEffectFilter = effectFilter;
        if (mTextureView != null) {
            mTextureView.setEffectFilter(effectFilter);
        }
    }

    public void setMatrixGL(float[] matrixGL) {
        this.mMatrixGL = matrixGL;
        if (mTextureView != null) {
            mTextureView.setMatrixGL(mMatrixGL);
        }
    }

    public void setCustomGLRenderer(GSYVideoGLViewBaseRender renderer) {
        this.mRenderer = renderer;
        if (mTextureView != null) {
            mTextureView.setGLRenderer(renderer);
        }
    }

    public void setGLRenderMode(int mode) {
        mMode = mode;
        if (mTextureView != null) {
            mTextureView.setGLRenderMode(mode);
        }
    }


    protected abstract void showPauseCover();

    protected abstract void releasePauseCover();

    protected abstract void setSmallVideoTextureView();

    protected abstract void setDisplay(Surface surface);

    protected abstract void releaseSurface(Surface surface);

}
