package com.seafile.seadroid2.ui.repo;

import android.content.Context;
import androidx.recyclerview.widget.GridLayoutManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import java.util.List;
import java.util.function.Supplier;

/** Library rows remain full width independently of the preferred file layout. */
public class RepoGridLayoutManager extends GridLayoutManager {
    public RepoGridLayoutManager(Context context, boolean inRepo, FileViewType type,
                                 Supplier<List<BaseModel>> items) {
        super(context, !inRepo || type == FileViewType.LIST ? 1 : type == FileViewType.GRID ? 2 : 4);
        setSpanSizeLookup(new SpanSizeLookup() {
            @Override public int getSpanSize(int position) {
                List<BaseModel> list = items.get();
                if (position < 0 || position >= list.size()) return getSpanCount();
                BaseModel item = list.get(position);
                return item instanceof RepoModel || item instanceof GroupItemModel || item instanceof Account
                        ? getSpanCount() : 1;
            }
        });
    }
}
