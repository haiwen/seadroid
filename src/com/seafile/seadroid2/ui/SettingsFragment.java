package com.seafile.seadroid2.ui;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragment;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.fileschooser.AutoBackupFolderChooserActivity;

public class SettingsFragment extends SherlockFragment {
	private static final String DEBUG_TAG = "SettingsFragment";
	private static final int icons[] = { R.drawable.icon_backup,
			R.drawable.icon_transfer, R.drawable.icon_history,
			R.drawable.icon_connect, R.drawable.icon_reward,
			R.drawable.icon_setting, R.drawable.icon_logout };

	private static final int strs[] = { R.string.auto_upload_photos,
			R.string.sync_method, R.string.sync_history,
			R.string.transfer_to_PC, R.string.get_free_storage,
			R.string.settings, R.string.sign_out };
	private BrowserActivity mActivity;
	private NavContext navContext;
	private ListView list;
	private SettingsListAdapter adapter;
	private SettingsListListener listener;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		// TODO Auto-generated method stub
		super.onCreate(savedInstanceState);

	}

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		mActivity = (BrowserActivity) activity;
		navContext = mActivity.getNavContext();
		Log.d(DEBUG_TAG, "SettingsFragment Attached");
	}

	public NavContext getNavContext() {
		return navContext;
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

		list = (ListView) view.findViewById(R.id.lv_settings_menu);
		adapter = new SettingsListAdapter(icons, strs, mActivity);
		listener = new SettingsListListener();
		list.setOnItemClickListener(listener);
		list.setAdapter(adapter);
		return view;
	}

	private class SettingsListListener implements OnItemClickListener {

		@Override
		public void onItemClick(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
			switch (arg2) {
			case 0:
				Intent intent = new Intent(mActivity.getApplicationContext(), AutoBackupFolderChooserActivity.class);
				mActivity.startActivityForResult(intent, BrowserActivity.PICK_AUTO_BACKUP_FOLDER_REQUEST);
				break;
			case 1:
				break;
			case 2:
				break;
			case 3:
				break;
			case 4:
				break;
			case 5:
				break;
			case 6:
				break;
				
			default:
				break;
			}
			//Toast.makeText(mActivity.getApplicationContext(), "You clicked on position : " + arg2 + " and id : " + arg3, Toast.LENGTH_LONG).show();
			/*intent.setType("vnd.android.cursor.dir/image");
			intent.setType("image/*");
			intent.setAction(Intent.ACTION_GET_CONTENT);
			intent.addCategory(Intent.CATEGORY_OPENABLE);
			startActivityForResult(Intent.createChooser(intent, "Choose image"), BrowserActivity.PICK_AUTO_BACKUP_FOLDER_REQUEST);
			 */
		}
	}
}
