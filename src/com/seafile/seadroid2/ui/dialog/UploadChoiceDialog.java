package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.fileschooser.MultiFileChooserActivity;
import com.seafile.seadroid2.mediachooser.MediaChooser;
import com.seafile.seadroid2.mediachooser.activity.HomeFragmentActivity;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.util.Utils;

public class UploadChoiceDialog extends DialogFragment {
    private Context ctx = SeadroidApplication.getAppContext();

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(getActivity()).
                setTitle(getResources().getString(R.string.pick_upload_type)).
                setTitleColor(getResources().getString(R.color.dialog_theme_color)).
                setDividerColor(getResources().getString(R.color.dialog_theme_color)).
                setItems(R.array.pick_upload_array, 
                        new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        switch (which) {
                        case 0:
                            Intent intent = new Intent(ctx, MultiFileChooserActivity.class);
                            getActivity().startActivityForResult(intent, BrowserActivity.PICK_FILES_REQUEST);
                            break;
                        case 1:
                            // photos & videos
                            MediaChooser.setSelectionLimit(20);
                            //MediaChooser.showOnlyImageTab();
                            Intent bIntent = new Intent(ctx, HomeFragmentActivity.class);
                            getActivity().startActivity(bIntent);
                            break;
                        case 2:
                            // thirdparty file chooser
                            Intent target = Utils.createGetContentIntent();
                            intent = Intent.createChooser(target, getString(R.string.choose_file));
                            getActivity().startActivityForResult(intent, BrowserActivity.PICK_FILE_REQUEST);
                            break;
                        default:
                            return;
                        }
                    }
                });
        return builder.show();
    }
}
