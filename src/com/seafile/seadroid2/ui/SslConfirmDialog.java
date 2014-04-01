package com.seafile.seadroid2.ui;

import java.net.MalformedURLException;
import java.net.URL;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.widget.CheckBox;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;


public class SslConfirmDialog extends DialogFragment {
    public interface Listener {
        void onAccepted(boolean rememberChoice);
        void onRejected();
    }

    public static final String FRAGMENT_TAG = "SslConfirmDialog";
    public static final String DEBUG_TAG = "SslConfirmDialog";

    private Account account;
    private Listener listener;
    private TextView messageText;
    private CheckBox rememberChoiceCheckbox;

    public SslConfirmDialog() {
    }

    public SslConfirmDialog(Account account, Listener listener) {
        this.listener = listener;
        this.account = account;
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        LinearLayout view = (LinearLayout)inflater.inflate(R.layout.dialog_ssl_confirm, null);

        messageText = (TextView)view.findViewById(R.id.message);
        rememberChoiceCheckbox = (CheckBox)view.findViewById(R.id.remember_choice);

        String host = null;

        try {
            host = new URL(account.server).getHost();
        } catch (MalformedURLException e) {
            // ignore
        }

        String msg = String.format(getActivity().getString(R.string.ssl_confirm), host);
        messageText.setText(msg);

        builder.setTitle(R.string.ssl_confirm_title);
        builder.setView(view);

        builder.setPositiveButton(R.string.yes, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Log.d(DEBUG_TAG, "listener.onAccepted is called");
                listener.onAccepted(rememberChoiceCheckbox.isChecked());
            }
        });
        builder.setNegativeButton(R.string.no, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Log.d(DEBUG_TAG, "listener.onRejected is called");
                listener.onRejected();
            }
        });

        Dialog dialog = builder.create();

        return dialog;
    }

    @Override
    public void onCancel(DialogInterface dialog) {
        Log.d(DEBUG_TAG, "listener.onRejected is called");
        listener.onRejected();
    }
}
