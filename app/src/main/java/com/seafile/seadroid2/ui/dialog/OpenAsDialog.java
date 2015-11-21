package com.seafile.seadroid2.ui.dialog;

import java.io.File;

import android.app.Dialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;

public class OpenAsDialog extends DialogFragment {
    
    private static final int OPEN_AS_TEXT = 0;
    private static final int OPEN_AS_AUDIO = 1;
    private static final int OPEN_AS_VIDEO = 2;
    private static final int OPEN_AS_IMAGE = 3;
    private static final int OPEN_AS_OTHER = 4;

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
                        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                        switch (which) {
                        case OPEN_AS_TEXT:
                            intent.setDataAndTypeAndNormalize((Uri.fromFile(file)), "text/*");
                            startActivity(intent);
                            break;
                        case OPEN_AS_AUDIO:
                            intent.setDataAndTypeAndNormalize((Uri.fromFile(file)), "audio/*");
                            startActivity(intent);
                            break;
                        case OPEN_AS_VIDEO:
                            intent.setDataAndTypeAndNormalize((Uri.fromFile(file)), "video/*");
                            startActivity(intent);
                            break;
                        case OPEN_AS_IMAGE:
                            intent.setDataAndTypeAndNormalize((Uri.fromFile(file)), "image/*");
                            startActivity(intent);
                            break;
                        case OPEN_AS_OTHER:
                            intent.setDataAndTypeAndNormalize((Uri.fromFile(file)), "*/*");
                            startActivity(intent);
                        default:
                            break;
                        }
                    }
                });

        return builder.show();
    }
}
