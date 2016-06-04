package com.seafile.seadroid2.ui.dialog;

import android.app.Activity;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.support.v7.app.AlertDialog;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.ui.adapter.SeafItemAdapter;

public class SortFilesDialogFragment extends DialogFragment {

    /**
     * The activity that creates an instance of this dialog fragment must
     * implement this interface in order to receive event callbacks.
     * Each method passes the DialogFragment in case the host needs to query it.
     */
    public interface SortItemClickListener {
        void onSortFileItemClick(DialogFragment dialog, int position);
    }

    // Use this instance of the interface to deliver action events
    SortItemClickListener mListener;

    // Override the Fragment.onAttach() method to instantiate the NoticeDialogListener
    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        // Verify that the host activity implements the callback interface
        try {
            // Instantiate the NoticeDialogListener so we can send events to the host
            mListener = (SortItemClickListener) activity;
        } catch (ClassCastException e) {
            // The activity doesn't implement the interface, throw exception
            throw new ClassCastException(activity.toString() + " must implement NoticeDialogListener");
        }
    }

    private int calculateCheckedItem() {
        switch (SettingsManager.instance().getSortFilesTypePref()) {
            case SeafItemAdapter.SORT_BY_NAME:
                if (SettingsManager.instance().getSortFilesOrderPref() == SeafItemAdapter.SORT_ORDER_ASCENDING)
                    return 0;
                else if (SettingsManager.instance().getSortFilesOrderPref() == SeafItemAdapter.SORT_ORDER_DESCENDING)
                    return 1;
                break;
            case SeafItemAdapter.SORT_BY_LAST_MODIFIED_TIME:
                if (SettingsManager.instance().getSortFilesOrderPref() == SeafItemAdapter.SORT_ORDER_ASCENDING)
                    return 2;
                else if (SettingsManager.instance().getSortFilesOrderPref() == SeafItemAdapter.SORT_ORDER_DESCENDING)
                    return 3;
                break;
        }
        return 0;
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity())
                .setTitle(getString(R.string.sort_files))
                .setSingleChoiceItems(R.array.sort_files_options_array,
                        calculateCheckedItem(),
                        new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialogInterface, int i) {
                                mListener.onSortFileItemClick(SortFilesDialogFragment.this, i);
                                dismiss();

                            }

                        });
        return builder.create();
    }
}
