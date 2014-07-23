package com.seafile.seadroid2.ui;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;

import com.seafile.seadroid2.R;

public class UploadChoiceDialog extends DialogFragment
{

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState)
		{

			AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
			builder.setTitle(R.string.pick_upload_type);
			builder.setItems(R.array.pick_upload_array,
					new DialogInterface.OnClickListener()
					{
						@Override
						public void onClick(DialogInterface dialog, int which)
							{
								switch (which)
								{
								case 0:

									break;
								case 1:
									// photos

									break;
								default:
									return;
								}

							}
					});

			return builder.create();
		}

}
