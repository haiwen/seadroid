package com.seafile.seadroid2.framework.util;

import android.graphics.Rect;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.window.layout.DisplayFeature;
import androidx.window.layout.FoldingFeature;
import androidx.window.layout.WindowLayoutInfo;

/** Converts WindowManager folding features into a small immutable app-level state. */
public final class FoldableHelper {
    public enum Orientation {
        NONE,
        VERTICAL,
        HORIZONTAL
    }

    public enum Posture {
        NONE,
        FLAT,
        HALF_OPENED
    }

    public enum Occlusion {
        NONE,
        FULL
    }

    public static final class FoldState {
        public final boolean hasFoldingFeature;
        public final boolean separating;
        @NonNull
        public final Orientation orientation;
        @NonNull
        public final Posture posture;
        @NonNull
        public final Occlusion occlusion;
        @Nullable
        public final Rect bounds;

        private FoldState(boolean hasFoldingFeature, boolean separating,
                          @NonNull Orientation orientation, @NonNull Posture posture,
                          @NonNull Occlusion occlusion, @Nullable Rect bounds) {
            this.hasFoldingFeature = hasFoldingFeature;
            this.separating = separating;
            this.orientation = orientation;
            this.posture = posture;
            this.occlusion = occlusion;
            this.bounds = bounds == null ? null : new Rect(bounds);
        }

        @NonNull
        public static FoldState none() {
            return new FoldState(false, false, Orientation.NONE, Posture.NONE,
                    Occlusion.NONE, null);
        }
    }

    private FoldableHelper() {
    }

    @NonNull
    public static FoldState parse(@NonNull WindowLayoutInfo info) {
        FoldingFeature selected = null;
        for (DisplayFeature feature : info.getDisplayFeatures()) {
            if (!(feature instanceof FoldingFeature)) {
                continue;
            }

            FoldingFeature foldingFeature = (FoldingFeature) feature;
            if (selected == null || (foldingFeature.isSeparating() && !selected.isSeparating())) {
                selected = foldingFeature;
            }
        }

        if (selected == null) {
            return FoldState.none();
        }

        Orientation orientation = selected.getOrientation() == FoldingFeature.Orientation.VERTICAL
                ? Orientation.VERTICAL : Orientation.HORIZONTAL;
        Posture posture = selected.getState() == FoldingFeature.State.HALF_OPENED
                ? Posture.HALF_OPENED : Posture.FLAT;
        Occlusion occlusion = selected.getOcclusionType() == FoldingFeature.OcclusionType.FULL
                ? Occlusion.FULL : Occlusion.NONE;

        return new FoldState(true, selected.isSeparating(), orientation, posture,
                occlusion, selected.getBounds());
    }
}
