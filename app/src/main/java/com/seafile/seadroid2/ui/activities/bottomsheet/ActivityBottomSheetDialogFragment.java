package com.seafile.seadroid2.ui.activities.bottomsheet;

import android.app.Dialog;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.RecyclerView;

import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.EventDetailsFileItem;

import java.util.List;

public class ActivityBottomSheetDialogFragment extends BottomSheetDialogFragment {

    private com.google.android.material.bottomsheet.BottomSheetDialog bottomSheetDialog;
    private List<EventDetailsFileItem> items;

    public void setItems(List<EventDetailsFileItem> items) {
        this.items = items;
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        bottomSheetDialog = new com.google.android.material.bottomsheet.BottomSheetDialog(requireContext(), R.style.ThemeOverlay_Catalog_BottomSheetDialog_Scrollable);
        bottomSheetDialog.setContentView(R.layout.dialog_activity_bottom_sheet);
        bottomSheetDialog.setDismissWithAnimation(true);

        rv = bottomSheetDialog.findViewById(R.id.rv);

//        View bottomSheetInternal = bottomSheetDialog.findViewById(R.id.design_bottom_sheet);
//        BottomSheetBehavior.from(bottomSheetInternal).setPeekHeight(400);

        setAdapter();

        return bottomSheetDialog;
    }

    private RecyclerView rv;

    public void setAdapter() {
        if (rv != null) {
            ActivityBottomSheetAdapter adapter = new ActivityBottomSheetAdapter();
            QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
            rv.setAdapter(helper.getAdapter());

            adapter.submitList(items);
            if (onItemClickListener != null) {
                adapter.setOnItemClickListener((BaseQuickAdapter.OnItemClickListener<EventDetailsFileItem>) onItemClickListener);
            }
        }
    }

    private BaseQuickAdapter.OnItemClickListener<?> onItemClickListener;

    public void setOnItemClickListener(BaseQuickAdapter.OnItemClickListener<?> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }
}
