package com.seafile.seadroid2.ui.bottomsheet;

import android.content.DialogInterface;
import android.support.annotation.Nullable;
import android.support.design.widget.CoordinatorLayout;
import android.view.MotionEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.ListAdapter;
import android.widget.ListView;

import com.seafile.seadroid2.R;

public class BottomSheetListFragment extends BaseBottomSheetDialogFragment {

    private CoordinatorLayout container;
    private ListAdapter adapter;
    private ListView listView;
    private AdapterView.OnItemClickListener onItemClickListener;
    private DialogInterface.OnDismissListener onDismissListener;

    @Override
    protected int getLayoutId() {
        return R.layout.dialog_bottom_sheet_list;
    }

    @Override
    protected int getCancelId() {
        return R.id.button_cancel;
    }

    @Override
    protected void initView() {
        listView = getRootView().findViewById(R.id.list_view);
        container = getRootView().findViewById(R.id.bottom_sheet_container);
    }

    @Nullable
    public ListView getListView() {
        return listView;
    }

    public void setOnItemClickListener(AdapterView.OnItemClickListener onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    public void setOnDismissListener(DialogInterface.OnDismissListener onDismissListener) {
        this.onDismissListener = onDismissListener;
    }

    public void setAdapter(ListAdapter adapter) {
        this.adapter = adapter;
    }


    private float downY = 0f, moveY;

    @Override
    protected void init() {
        if (adapter != null) {
            listView.setAdapter(adapter);
        }

        if (onItemClickListener != null) {
            listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
                @Override
                public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                    onItemClickListener.onItemClick(parent, view, position, id);
                }
            });
        }

        if (getDialog() != null && onDismissListener != null) {
            getDialog().setOnDismissListener(new DialogInterface.OnDismissListener() {
                @Override
                public void onDismiss(DialogInterface dialog) {
                    onDismissListener.onDismiss(dialog);
                }
            });
        }
    }
}
