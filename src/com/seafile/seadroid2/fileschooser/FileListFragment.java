package com.seafile.seadroid2.fileschooser;

import java.util.List;

import com.seafile.seadroid2.R;

import android.os.Bundle;
import android.os.Environment;
import android.support.v4.app.ListFragment;
import android.support.v4.app.LoaderManager;
import android.support.v4.content.Loader;
import android.util.Log;
import android.view.View;
import android.widget.ListView;

public class FileListFragment extends ListFragment implements
LoaderManager.LoaderCallbacks<List<SelectableFile>> {

    private static final String LOG_TAG = "FileListFragment";
    private static final int LOADER_ID = 0;

    private FileListAdapter mFileListAdapter;
    private String mPath;

    public static FileListFragment newInstance(String path) {
        FileListFragment fragment = new FileListFragment();
        Bundle args = new Bundle();
        args.putString(MultiFileChooserActivity.PATH, path);
        fragment.setArguments(args);

        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(LOG_TAG, "onCreate");
        mFileListAdapter = new FileListAdapter(getActivity());
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
        FileListAdapter adapter = (FileListAdapter) l.getAdapter();
        if (adapter != null) {
            SelectableFile file = adapter.getItem(position);
            mPath = file.getAbsolutePath();
            file.toggleSelected();
            if (file.isFile()) {
                FileListAdapter.Viewholder viewHolder = (FileListAdapter.Viewholder)v.getTag();
                viewHolder.checkBox.setChecked(file.isSelected());
            }
            ((MultiFileChooserActivity) getActivity()).onFileChecked(file);
        }
    }

    @Override
    public void onPause () {
        Log.d(LOG_TAG, "onPause");
        super.onPause();
    }

    @Override
    public Loader<List<SelectableFile>> onCreateLoader(int id, Bundle args) {
        return new FileLoader(getActivity(), mPath, ((MultiFileChooserActivity) getActivity()).getSelectedFiles());
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
