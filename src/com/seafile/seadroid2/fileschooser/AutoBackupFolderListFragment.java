package com.seafile.seadroid2.fileschooser;

import java.util.List;
import java.util.Stack;

import android.os.Bundle;
import android.os.Environment;
import android.support.v4.app.ListFragment;
import android.support.v4.app.LoaderManager;
import android.support.v4.content.Loader;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.Utils;

public class AutoBackupFolderListFragment extends ListFragment implements
LoaderManager.LoaderCallbacks<List<SelectableFile>>{
    
    private static final String LOG_TAG = "AutoBackupFolderListFragment";
    private static final int LOADER_ID = 0;
    private AutoBackupFolderListAdapter mFileListAdapter;
    private String mPath;
	
    public static AutoBackupFolderListFragment newInstance(String path) {
        AutoBackupFolderListFragment fragment = new AutoBackupFolderListFragment();
        Bundle args = new Bundle();
        args.putString(MultiFileChooserActivity.PATH, path);
        fragment.setArguments(args);

        return fragment;
    }

    @Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		Log.d(LOG_TAG, "onCreate");
		mFileListAdapter = new AutoBackupFolderListAdapter(getActivity());
		mPath = getArguments() != null ? getArguments().getString(
				MultiFileChooserActivity.PATH) : Environment
				.getExternalStorageDirectory().getAbsolutePath();
	}
    
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        setEmptyText(getString(R.string.empty_folder));
        setListAdapter(mFileListAdapter);
        setListShown(false);
        getLoaderManager().initLoader(LOADER_ID, null, this);       
        super.onActivityCreated(savedInstanceState);
    }
    
    
    @Override
    public void onListItemClick(ListView l, View v, int position, long id) {
        /*AutoBackupFolderListAdapter adapter = (AutoBackupFolderListAdapter) l.getAdapter();
        if (adapter != null) {
            SelectableFile file = adapter.getItem(position);
            mPath = file.getAbsolutePath();
            file.toggleSelected();
            if (file.isFile()) {
            	AutoBackupFolderListAdapter.Viewholder viewHolder = (AutoBackupFolderListAdapter.Viewholder)v.getTag();
                viewHolder.checkBox.setChecked(file.isSelected());
            }
            ((AutoBackupFolderChooserActivity) getActivity()).onFileChecked(file);
        }*/
    }
    
    
    @Override
    public void onPause () {
        Log.d(LOG_TAG, "onPause");
        super.onPause();
    }

    @Override
    public Loader<List<SelectableFile>> onCreateLoader(int id, Bundle args) {
    	//data = Utils.listPath(mPath);
        return new AutoBackupFolderFileLoader(getActivity(), mPath, ((AutoBackupFolderChooserActivity) getActivity()).getSelectedFiles());
    }

    @Override
    public void onLoadFinished(Loader<List<SelectableFile>> loader, List<SelectableFile> data) {
        mFileListAdapter.setListItems(data);

        if (isResumed())
            setListShown(true);
        else
            setListShownNoAnimation(true);
        

    }

    @Override
    public void onLoaderReset(Loader<List<SelectableFile>> loader) {
        mFileListAdapter.clear();
    }
}
