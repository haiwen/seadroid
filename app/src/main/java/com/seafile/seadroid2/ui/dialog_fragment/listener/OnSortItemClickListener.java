package com.seafile.seadroid2.ui.dialog_fragment.listener;

import androidx.fragment.app.DialogFragment;

/**
 * The activity that creates an instance of this dialog fragment must
 * implement this interface in order to receive event callbacks.
 * Each method passes the DialogFragment in case the host needs to query it.
 */
public interface OnSortItemClickListener {
    void onSortFileItemClick(DialogFragment dialog, int position);
}
