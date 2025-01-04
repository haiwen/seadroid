package com.seafile.seadroid2.widget.prefs.simplemenu;

import android.annotation.TargetApi;
import android.graphics.Rect;
import android.os.Build;
import android.util.Property;

@TargetApi(Build.VERSION_CODES.LOLLIPOP)
class SimpleMenuBoundsProperty extends Property<PropertyHolder, Rect> {

    public static final Property<PropertyHolder, Rect> BOUNDS;

    static {
        BOUNDS = new SimpleMenuBoundsProperty("bounds");
    }

    @Override
    public Rect get(PropertyHolder holder) {
        return holder.getBounds();
    }

    @Override
    public void set(PropertyHolder holder, Rect value) {
        holder.setBounds(value);

        if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.N_MR1) {
            holder.getContentView().invalidate();
        }
    }

    public SimpleMenuBoundsProperty(String name) {
        super(Rect.class, name);
    }
}
