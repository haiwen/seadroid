package com.seafile.seadroid2.ui.bottomsheetmenu;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.MenuRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.fragment.app.FragmentManager;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;

import java.util.ArrayList;
import java.util.List;

public class BottomSheetMenuFragment extends BottomSheetDialogFragment {
    private final Builder builder;

    public BottomSheetMenuFragment(Builder builder) {
        this.builder = builder;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (builder == null) {
            throw new IllegalArgumentException("need to create Builder");
        }

        if (builder.columnCount < 0) {
            throw new IllegalArgumentException("columnCount should not less than 0");
        }
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        BottomSheetDialog bottomSheetDialog = new BottomSheetDialog(requireContext());
        bottomSheetDialog.setContentView(R.layout.layout_bottom_sheet_menu_dialog);
        bottomSheetDialog.setDismissWithAnimation(true);

        View parentView = bottomSheetDialog.findViewById(R.id.bottom_sheet_container);
        setView(parentView);
        return bottomSheetDialog;
    }

    public void setView(View rootView) {
        if (TextUtils.isEmpty(builder.title)) {
            rootView.findViewById(R.id.title).setVisibility(View.GONE);
        } else {
            TextView title = rootView.findViewById(R.id.title);
            title.setText(builder.title);
        }

        if (CollectionUtils.isEmpty(builder.menuItems)) {
            return;
        }

        RecyclerView rv = rootView.findViewById(R.id.rv);
        if (builder.columnCount == 1) {
            rv.setLayoutManager(new LinearLayoutManager(requireContext()));
        } else {
            rv.setLayoutManager(new GridLayoutManager(requireContext(), builder.columnCount));
        }

        BottomSheetMenuAdapter adapter = getBottomSheetMenuAdapter();
        adapter.submitList(builder.menuItems);

        rv.setAdapter(adapter);

    }

    private @NonNull BottomSheetMenuAdapter getBottomSheetMenuAdapter() {
        BottomSheetMenuAdapter adapter = new BottomSheetMenuAdapter(builder.columnCount);
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<MenuItem>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<MenuItem, ?> baseQuickAdapter, @NonNull View view, int i) {
                dismiss();
                if (builder.onMenuClickListener != null) {
                    builder.onMenuClickListener.onMenuClick(adapter.getItem(i));
                }
            }
        });
        return adapter;
    }


    public static class Builder {
        public Context context;

        public int columnCount = 1;
        @MenuRes
        public int menuSheetId = -1;

        public boolean cancelable;

        public String title;

        public OnMenuClickListener onMenuClickListener;
        public List<MenuItem> menuItems;

        public Builder(Context context) {
            this.context = context;
        }

        public Builder setColumnCount(int columnCount) {
            this.columnCount = columnCount;
            return this;
        }

        public Builder setMenuSheetId(int menuSheetId) {
            this.menuSheetId = menuSheetId;

            ActionMenu menu = new ActionMenu(context);

            MenuInflater inflater = new MenuInflater(context);
            inflater.inflate(menuSheetId, menu);

            List<MenuItem> items = new ArrayList<MenuItem>(menu.size());
            for (int i = 0; i < menu.size(); i++) {
                items.add(menu.getItem(i));
            }

            return setMenuItems(items);
        }

        public Builder setMenuItems(List<MenuItem> menuItems) {
            if (menuItems != null && !menuItems.isEmpty()) {
                if (this.menuItems == null) {
                    this.menuItems = new ArrayList<>(menuItems.size());
                }
                this.menuItems.addAll(menuItems);
            }

            return this;
        }

        public Builder setCancelable(boolean cancelable) {
            this.cancelable = cancelable;
            return this;
        }

        public Builder setTitle(@StringRes int title) {
            this.title = context.getString(title);
            return this;
        }

        public Builder setTitle(String title) {
            this.title = title;
            return this;
        }

        public Builder removeMenu(int menuId) {
            if (CollectionUtils.isEmpty(this.menuItems)) {
                return this;
            }
            int index = -1;
            for (int i = 0; i < menuItems.size(); i++) {
                if (menuId == menuItems.get(i).getItemId()) {
                    index = i;
                    break;
                }
            }
            if (index > -1) {
                this.menuItems.remove(index);
            }
            return this;
        }

        public Builder setOnMenuClickListener(OnMenuClickListener onMenuClickListener) {
            this.onMenuClickListener = onMenuClickListener;
            return this;
        }

        public BottomSheetMenuFragment build() {
            return new BottomSheetMenuFragment(this);
        }

        public void show(FragmentManager fragmentManager) {
            build().show(fragmentManager, BottomSheetMenuFragment.class.getSimpleName());
        }

        public void show(FragmentManager fragmentManager, String tag) {
            build().show(fragmentManager, tag);
        }
    }
}
