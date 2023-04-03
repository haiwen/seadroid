package com.seafile.seadroid2.ui.bottomsheet;

import android.content.DialogInterface;
import android.support.annotation.Nullable;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;

import com.seafile.seadroid2.R;

public class BottomSheetListFragment extends BaseBottomSheetDialogFragment {

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
        listView = (ListView) getRootView().findViewById(R.id.list_view);
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

    @Override
    protected void init() {
        if (adapter != null) {
            listView.setAdapter(adapter);
        }

        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                if (onItemClickListener != null) {
                    onItemClickListener.onItemClick(parent, view, position, id);
                }
            }
        });

        if (getDialog() != null) {
            getDialog().setOnDismissListener(new DialogInterface.OnDismissListener() {
                @Override
                public void onDismiss(DialogInterface dialog) {
                    if (onDismissListener != null) {
                        onDismissListener.onDismiss(dialog);
                    }
                }
            });
        }
    }
}
