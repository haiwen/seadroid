package com.seafile.seadroid2.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.coordinatorlayout.widget.CoordinatorLayout;
import androidx.core.view.ViewCompat;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

public class HideBottomBehavior extends CoordinatorLayout.Behavior<LinearLayout> {

    public HideBottomBehavior() {
    }

    public HideBottomBehavior(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    public boolean layoutDependsOn(@NonNull CoordinatorLayout parent, @NonNull LinearLayout child, View dependency) {
//        return dependency.getId() == R.id.appbar;
        return false;
    }

    private boolean isInit = true;

    @Override
    public boolean onDependentViewChanged(@NonNull CoordinatorLayout parent, @NonNull LinearLayout child, @NonNull View dependency) {
        if (isInit) {
            isInit = false;
        } else {
            float y = dependency.getY();
            child.setTranslationY(-(y));
        }
        return true;
    }
}
