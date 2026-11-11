package com.seafile.seadroid2.ui.repo;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;

import java.util.List;
import java.util.function.Supplier;

/** A grid whose span count follows the currently available RecyclerView width. */
public class RepoGridLayoutManager extends GridLayoutManager {
    private static final int MIN_TILE_WIDTH_DP_GRID = 180;
    private static final int MIN_TILE_WIDTH_DP_GALLERY = 90;

    private boolean inRepo;
    private final Context context;
    @NonNull
    private FileViewType fileViewType;

    /** Kept for existing callers and tests that use the historical fixed-size behavior. */
    public RepoGridLayoutManager(@NonNull Context context, boolean inRepo,
                                 @NonNull FileViewType fileViewType,
                                 @NonNull Supplier<List<BaseModel>> items) {
        super(context, legacySpanCount(inRepo, fileViewType));
        this.context = context;
        this.inRepo = inRepo;
        this.fileViewType = fileViewType;
        installSpanSizeLookup(items);
    }

    public RepoGridLayoutManager(@NonNull Context context, boolean inRepo,
                                 @NonNull FileViewType fileViewType, int contentWidthPx,
                                 @NonNull Supplier<List<BaseModel>> items) {
        super(context, calculateSpanCount(context, contentWidthPx, inRepo, fileViewType));
        this.context = context;
        this.inRepo = inRepo;
        this.fileViewType = fileViewType;
        installSpanSizeLookup(items);
    }

    private void installSpanSizeLookup(@NonNull Supplier<List<BaseModel>> items) {
        setSpanSizeLookup(new SpanSizeLookup() {
            @Override
            public int getSpanSize(int position) {
                List<BaseModel> list = items.get();
                if (list == null || position < 0 || position >= list.size()) {
                    return getSpanCount();
                }

                BaseModel item = list.get(position);
                return item instanceof RepoModel || item instanceof GroupItemModel
                        || item instanceof Account ? getSpanCount() : 1;
            }
        });
    }

    public boolean updateLayoutConfig(boolean inRepo, @NonNull FileViewType fileViewType,
                                      int contentWidthPx) {
        this.inRepo = inRepo;
        this.fileViewType = fileViewType;

        int newSpanCount = calculateSpanCount(context, contentWidthPx, inRepo, fileViewType);
        if (newSpanCount == getSpanCount()) {
            return false;
        }

        setSpanCount(newSpanCount);
        return true;
    }

    public static int calculateSpanCount(@NonNull Context context, int contentWidthPx,
                                         boolean inRepo, @NonNull FileViewType fileViewType) {
        if (!inRepo || fileViewType == FileViewType.LIST || contentWidthPx <= 0) {
            return 1;
        }

        float density = context.getResources().getDisplayMetrics().density;
        int minTileDp = fileViewType == FileViewType.GALLERY
                ? MIN_TILE_WIDTH_DP_GALLERY : MIN_TILE_WIDTH_DP_GRID;
        int minTilePx = Math.max(1, Math.round(minTileDp * density));
        return Math.max(1, contentWidthPx / minTilePx);
    }

    private static int legacySpanCount(boolean inRepo, @NonNull FileViewType fileViewType) {
        if (!inRepo || fileViewType == FileViewType.LIST) {
            return 1;
        }
        return fileViewType == FileViewType.GRID ? 2 : 4;
    }
}
