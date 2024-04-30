package com.seafile.seadroid2.view;

import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.preference.ListPreference;
import androidx.preference.ListPreferenceDialogFragmentCompat;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

public class ListPreferenceCompat extends ListPreferenceDialogFragmentCompat {
    private int mWhichButtonClicked = 0;
    public static final String ARG_KEY = "key";
    private String key;

    private CharSequence[] mEntries;
    private CharSequence[] mEntryValues;

    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        mWhichButtonClicked = DialogInterface.BUTTON_NEGATIVE;

        if (getArguments() != null) {
            key = getArguments().getString(ARG_KEY);
        }

        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(getContext());
        builder.setTitle(getPreference().getTitle());
        builder.setIcon(getPreference().getIcon());
        builder.setPositiveButton(getPreference().getPositiveButtonText(), this);
        builder.setNegativeButton(getPreference().getNegativeButtonText(), this);
        View v = onCreateDialogView(getContext());
        if (v != null) {
            onBindDialogView(v);
            builder.setView(v);
        } else {
            builder.setMessage(getPreference().getDialogMessage());
        }
        onPrepareDialogBuilder(builder);
        return builder.create();
    }

    @Override
    public void onClick(@NonNull DialogInterface dialog, int which) {
        super.onClick(dialog, which);
        mWhichButtonClicked = which;
    }

    private boolean onDialogClosedWasCalledFromOnDismiss = false;

    @Override
    public void onDismiss(@NonNull DialogInterface dialog) {
        onDialogClosedWasCalledFromOnDismiss = true;
        super.onDismiss(dialog);
    }

    @Override
    public void onDialogClosed(boolean positiveResult) {
        if (onDialogClosedWasCalledFromOnDismiss) {
            onDialogClosedWasCalledFromOnDismiss = false;
            super.onDialogClosed(mWhichButtonClicked == DialogInterface.BUTTON_POSITIVE);

            ListPreference preference = (ListPreference) getPreference();
            if (preference != null && key != null) {
                Bundle bundle = new Bundle();
                bundle.putString("result", preference.getValue());
                getParentFragmentManager().setFragmentResult(key, bundle);
            }

        } else {
            super.onDialogClosed(positiveResult);
        }
    }
}
