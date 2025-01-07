package com.seafile.seadroid2.widget.prefs.simplemenu;

import android.annotation.SuppressLint;
import android.graphics.HardwareRenderer;
import android.graphics.Point;
import android.os.Build;
import android.view.Display;
import android.view.View;
import android.widget.PopupWindow;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

@SuppressWarnings({"JavaReflectionMemberAccess", "DiscouragedPrivateApi"})
@SuppressLint("PrivateApi")
class Light {

    /**
     * Android uses <code>displaySize.x / 2 - windowLeft</code> as the x-coordinate of light source (<a href="http://androidxref.com/9.0.0_r3/xref/frameworks/base/core/java/android/view/ThreadedRenderer.java#1021">source code</a>).
     * <br>If the window is on the left of the screen, the light source will be at the right to the window
     * causing shadow on the left side. This make our PopupWindow looks weird.
     * <p>This method reset the x-coordinate of light source to <code>windowLeft + 56dp</code> by using multiply reflections.
     *
     * @param window PopupWindow
     */
    static void resetLightCenterForPopupWindow(PopupWindow window) {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
            return;
        }

        try {
            Class<?> threadedRendererClass = Class.forName("android.view.ThreadedRenderer");
            Class<?> attachInfoClass = Class.forName("android.view.View$AttachInfo");

            View view = window.getContentView().getRootView();
            Method getThreadedRendererMethod = View.class.getDeclaredMethod("getThreadedRenderer");
            getThreadedRendererMethod.setAccessible(true);
            Object threadedRenderer = getThreadedRendererMethod.invoke(view);

            Field attachInfoField = View.class.getDeclaredField("mAttachInfo");
            attachInfoField.setAccessible(true);
            Object attachInfo = attachInfoField.get(view);

            Field pointField = attachInfoClass.getDeclaredField("mPoint");
            pointField.setAccessible(true);
            Point displaySize = (Point) pointField.get(attachInfo);

            Field displayField = attachInfoClass.getDeclaredField("mDisplay");
            displayField.setAccessible(true);
            Display display = (Display) displayField.get(attachInfo);

            display.getRealSize(displaySize);

            Field windowLeftField = attachInfoClass.getDeclaredField("mWindowLeft");
            windowLeftField.setAccessible(true);
            int mWindowLeft = windowLeftField.getInt(attachInfo);

            Field windowTopField = attachInfoClass.getDeclaredField("mWindowTop");
            windowTopField.setAccessible(true);
            int mWindowTop = windowTopField.getInt(attachInfo);

            Field lightYField = threadedRendererClass.getDeclaredField("mLightY");
            lightYField.setAccessible(true);
            float mLightY = lightYField.getFloat(threadedRenderer);

            Field lightZField = threadedRendererClass.getDeclaredField("mLightZ");
            lightZField.setAccessible(true);
            float mLightZ = lightZField.getFloat(threadedRenderer);

            Field lightRadiusField = threadedRendererClass.getDeclaredField("mLightRadius");
            lightRadiusField.setAccessible(true);
            float mLightRadius = lightRadiusField.getFloat(threadedRenderer);

            final float lightX = mWindowLeft;
            final float lightY = mLightY - mWindowTop;

            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                ((HardwareRenderer) threadedRenderer).setLightSourceGeometry(
                        lightX, lightY, mLightZ, mLightRadius
                );
            } else {
                Field nativeProxyField = threadedRendererClass.getDeclaredField("mNativeProxy");
                nativeProxyField.setAccessible(true);
                long mNativeProxy = nativeProxyField.getLong(threadedRenderer);

                Method nSetLightCenterMethod = threadedRendererClass.getDeclaredMethod("nSetLightCenter", long.class, float.class, float.class, float.class);
                nSetLightCenterMethod.setAccessible(true);
                nSetLightCenterMethod.invoke(null, mNativeProxy, lightX, lightY, mLightZ);
            }
        } catch (Throwable tr) {
            tr.printStackTrace();
        }
    }
}
