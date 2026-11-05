package com.seafile.seadroid2.ui.repo.sheetaction;

import android.app.Activity;
import android.content.Context;

import androidx.annotation.NonNull;

/** Shared responsive sizing rules for the action bottom sheet menu. */
final class BottomSheetActionLayoutHelper {
    private static final int PHONE_REFERENCE_WIDTH_DP = 360;
    private static final int LARGE_REFERENCE_WIDTH_DP = 1200;
    private static final int PHONE_ITEM_WIDTH_DP = 72;
    private static final int LARGE_ITEM_WIDTH_DP = 100;
    private static final int MIN_COLUMNS = 5;
    private static final int MAX_COLUMNS = 12;

    private BottomSheetActionLayoutHelper() {
    }

    static int getAvailableWidth(@NonNull Activity activity) {
        int width = activity.getWindow().getDecorView().getWidth();
        if (width > 0) {
            return width;
        }
        return activity.getResources().getDisplayMetrics().widthPixels;
    }

    static int getColumnCount(@NonNull Context context, int availableWidth) {
        float density = context.getResources().getDisplayMetrics().density;
        int widthDp = Math.round(availableWidth / density);

        // Gradually increase the target item width as the window becomes wider.
        // This avoids a hard tablet breakpoint and keeps common phone widths at
        // five columns while allowing intermediate and large windows to use the
        // available space more naturally.
        float progress = (widthDp - PHONE_REFERENCE_WIDTH_DP)
                / (float) (LARGE_REFERENCE_WIDTH_DP - PHONE_REFERENCE_WIDTH_DP);
        progress = Math.max(0f, Math.min(1f, progress));
        float targetItemWidthDp = PHONE_ITEM_WIDTH_DP
                + (LARGE_ITEM_WIDTH_DP - PHONE_ITEM_WIDTH_DP) * progress;

        int columnCount = (int) Math.floor(widthDp / targetItemWidthDp);
        return Math.max(MIN_COLUMNS, Math.min(MAX_COLUMNS, columnCount));
    }

    static int getItemWidth(int availableWidth, int columnCount) {
        return Math.max(1, availableWidth / Math.max(1, columnCount));
    }
}
