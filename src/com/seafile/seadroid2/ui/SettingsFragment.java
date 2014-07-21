package com.seafile.seadroid2.ui;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;

import com.actionbarsherlock.app.SherlockFragment;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.fileschooser.AutoBackupFolderChooserActivity;
import com.seafile.seadroid2.fileschooser.MultiFileChooserActivity;

public class SettingsFragment extends SherlockFragment{
	private static final String DEBUG_TAG = "SettingsFragment";
	private BrowserActivity mActivity;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		// TODO Auto-generated method stub
		super.onCreate(savedInstanceState);
		
	}

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		mActivity = (BrowserActivity)activity;
		Log.d(DEBUG_TAG, "SettingsFragment Attached");
	}

	@Override
	public void onDetach() {
		mActivity = null;
		super.onDetach();
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		Log.d(DEBUG_TAG, "SettingsFragment onCreateView");
		View view = inflater.inflate(R.layout.fragment_menu, container, false);
		Button autoBackup = (Button) view.findViewById(R.id.btn_auto_img_backup);
		autoBackup.setOnClickListener(new OnClickListener() {
			
			@Override
			public void onClick(View v) {
				Intent intent = new Intent(mActivity.getApplicationContext(), AutoBackupFolderChooserActivity.class);
//				Toast.makeText(getActivity().getApplicationContext(), "Browse auto backup images folder", Toast.LENGTH_SHORT).show();
				mActivity.startActivityForResult(intent, BrowserActivity.PICK_AUTO_BACKUP_FOLDER_REQUEST);
				
			}
		});
		return view;
	}

}
