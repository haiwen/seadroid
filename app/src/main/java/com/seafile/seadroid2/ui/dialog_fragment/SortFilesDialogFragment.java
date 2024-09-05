package com.seafile.seadroid2.ui.dialog_fragment;

import android.app.Dialog;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.fragment.app.DialogFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnSortItemClickListener;
import com.seafile.seadroid2.framework.datastore.sp.Sorts;

@Deprecated
public class SortFilesDialogFragment extends DialogFragment {
    
    // Use this instance of the interface to deliver action events
    private OnSortItemClickListener mListener;

    public void setOnSortItemClickListener(OnSortItemClickListener mListener) {
        this.mListener = mListener;
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireActivity())
                .setTitle(getString(R.string.sort_files))
                .setSingleChoiceItems(R.array.sort_files_options_array,
                        Sorts.getSortType(),
                        (dialogInterface, i) -> {
                            Sorts.setSortType(i);

                            if (mListener != null) {
                                mListener.onSortFileItemClick(SortFilesDialogFragment.this, i);
                            }
                            dismiss();
                        });
        return builder.create();
    }
}
