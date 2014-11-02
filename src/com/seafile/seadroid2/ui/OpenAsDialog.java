package com.seafile.seadroid2.ui;

import java.io.File;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;

import com.seafile.seadroid2.R;

public class OpenAsDialog extends DialogFragment {

    private File file;

    public OpenAsDialog(File file) {
        this.file = file;
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(getActivity());
        builder.setTitle(getResources().getString(R.string.open_as));
        builder.setItems(R.array.file_type_array,
                new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        Intent intent = new Intent(Intent.ACTION_VIEW);
                        switch (which) {
                            // XXX: should use final variable here instead of number
                        case 0:
                            intent.setDataAndType((Uri.fromFile(file)), "text/*");
                            startActivity(intent);
                            break;
                        case 1:
                            intent.setDataAndType((Uri.fromFile(file)), "audio/*");
                            startActivity(intent);
                            break;
                        case 2:
                            intent.setDataAndType((Uri.fromFile(file)), "video/*");
                            startActivity(intent);
                            break;
                        case 3:
                            intent.setDataAndType((Uri.fromFile(file)), "image/*");
                            startActivity(intent);
                            break;
                        case 4:
                            intent.setDataAndType((Uri.fromFile(file)), "*/*");
                            startActivity(intent);
                        default:
                            break;
                        }
                    }
                });

        return builder.create();
    }
}
