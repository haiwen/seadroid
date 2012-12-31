package com.seafile.seadroid.ui;

import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.ListView;

import com.actionbarsherlock.app.SherlockListFragment;
import com.seafile.seadroid.BrowserActivity;
import com.seafile.seadroid.data.*;

public class CacheFragment extends SherlockListFragment {

    private static final String DEBUG_TAG = "CachedFragment";

    private SeafItemAdapter adapter;
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
        adapter = new SeafItemAdapter(getActivity());
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
    
    public void refreshView() {
        List<SeafCachedFile> files = getDataManager().getCachedFiles();
        adapter.clear();
        for (SeafCachedFile cf : files) {
            adapter.add(cf);
        }
        adapter.notifyChanged();
    }

    @Override 
    public void onListItemClick(ListView l, View v, int position, long id) {   
        SeafCachedFile cf = (SeafCachedFile)adapter.getItem(position);
        mActivity.onCachedFileSelected(cf);
    }

}
