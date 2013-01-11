package com.seafile.seadroid.ui;

import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.ListView;

import com.actionbarsherlock.app.SherlockListFragment;
import com.seafile.seadroid.BrowserActivity;
import com.seafile.seadroid.data.*;

public class CacheFragment extends SherlockListFragment 
        implements SeafItemCheckableAdapter.OnCheckedChangeListener {

    private static final String DEBUG_TAG = "CachedFragment";

    private SeafItemCheckableAdapter adapter;
    View refresh = null;
    BrowserActivity mActivity = null;
    
    private DataManager getDataManager() {
        return mActivity.getDataManager();
    }
    
    public interface OnCachedFileSelectedListener {
        public void onCachedFileSelected(SeafCachedFile item);
    }
    
    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        mActivity = (BrowserActivity)activity;
    }
    
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {

        super.onActivityCreated(savedInstanceState);
        adapter = new SeafItemCheckableAdapter(getActivity());
        adapter.setOnCheckedChangeListener(this);
        setListAdapter(adapter);

        getListView().setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        
        // refresh the view (loading data)
        refreshView();
    }
    
    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }

    @Override
    public void onDetach() {
        mActivity = null;
        super.onDetach();
    }
    
    public boolean isItemSelected() {
        return adapter.getNumSelected() > 0;
    }
    
    public void refreshView() {
        List<SeafCachedFile> files = getDataManager().getCachedFiles();
        adapter.clear();
        for (SeafCachedFile cf : files) {
            adapter.add(cf);
        }
        adapter.notifyChanged();
        mActivity.invalidateOptionsMenu();
    }

    @Override 
    public void onListItemClick(ListView l, View v, int position, long id) {   
        SeafCachedFile cf = (SeafCachedFile)adapter.getItem(position);
        mActivity.onCachedFileSelected(cf);
    }

    @Override
    public void onCheckedChanged(SeafItem item, boolean isChecked) {
        mActivity.invalidateOptionsMenu();
    }

    public void deleteSelectedCacheItems() {
        List<SeafItem> items = adapter.getSelectedItems();
        for (SeafItem item : items) {
            SeafCachedFile cf = (SeafCachedFile)item;
            getDataManager().removeCachedFile(cf);
        }
        adapter.removeSelectedItems();
        mActivity.invalidateOptionsMenu();
    }

}
