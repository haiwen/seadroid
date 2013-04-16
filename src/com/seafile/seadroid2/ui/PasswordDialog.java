package com.seafile.seadroid2.ui;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import com.seafile.seadroid2.R;

public class PasswordDialog extends DialogFragment {

    private EditText passwdText;
    PasswordGetListener mListener;
    
    public void setPasswordGetListener(PasswordGetListener listener) {
        mListener = listener;
    }
    
    public interface PasswordGetListener {
        public void onPasswordGet(String password);
    }
    
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        View view = inflater.inflate(R.layout.dialog_password, null);
        passwdText = (EditText) view.findViewById(R.id.password);

        builder.setView(view)
            .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    mListener.onPasswordGet(passwdText.getText().toString());
                }
            });
        builder.setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                
            }
        });
        
        return builder.create();
    }
    
}
