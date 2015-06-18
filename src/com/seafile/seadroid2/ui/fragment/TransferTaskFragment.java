package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import com.actionbarsherlock.app.SherlockListFragment;
import com.actionbarsherlock.view.ActionMode;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferTaskInfo;
import com.seafile.seadroid2.ui.activity.TransferActivity;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;

import java.util.List;

/**
 * Base class for transfer task fragments
 *
 */
public abstract class TransferTaskFragment extends SherlockListFragment {
    private String DEBUG_TAG = "TransferTaskFragment";

    protected TransferTaskAdapter adapter;
    protected TransferActivity mActivity = null;
    protected ListView mTransferTaskListView;
    protected TextView emptyView;
    private View mListContainer;
    private View mProgressContainer;
    protected final Handler mTimer = new Handler();
    protected TransferService txService = null;
    private ActionMode mActionMode;

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        mActivity = (TransferActivity) activity;
    }

    public ActionMode getActionMode() {
        return mActionMode;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.transfer_task_fragment, container, false);
        mTransferTaskListView = (ListView) root.findViewById(android.R.id.list);
        // mTransferTaskListView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE_MODAL);
        mTransferTaskListView.setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
            @Override
            public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
                onListItemChecked(position);
                return true;
            }
        });

        mListContainer =  root.findViewById(R.id.listContainer);
        mProgressContainer = root.findViewById(R.id.progressContainer);
        emptyView = (TextView) root.findViewById(R.id.empty);
        return root;
    }

    private List<Integer> convertToTaskIds(long[] ids) {
        if (ids == null) return null;
        List<Integer> taskIds = Lists.newArrayList();
        for (int i = 0; i < ids.length; i++) {
            int position = (int) ids[i];
            TransferTaskInfo tti = adapter.getItem(position);
            taskIds.add(tti.taskID);
        }

        return taskIds;
    }
    /**
     * select all items
     */
    private void selectItems() {
        for (int position = 0; position < adapter.getCount(); position++) {
            mTransferTaskListView.setItemChecked(position, true);
        }

    }

    /**
     * deselect all items
     */
    private void deselectItems() {
        for (int position = 0; position < adapter.getCount(); position++) {
            mTransferTaskListView.setItemChecked(position, false);
        }

    }

    protected abstract void deleteSelectedItems(List<Integer> ids);

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

        showLoading(true);

    }

    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            // Toast.makeText(mActivity, "Stop loading animations", Toast.LENGTH_LONG).show();
            showLoading(false);

            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
            if (isNeedUpdateProgress()) {
                mTransferTaskListView.setVisibility(View.VISIBLE);
                emptyView.setVisibility(View.GONE);
                setUpTransferList();
                startTimer();
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

    protected abstract List<? extends TransferTaskInfo> getTransferTaskInfos();

    protected abstract void setUpTransferList();

    @Override
    public void onResume() {
        super.onResume();
        mTransferTaskListView.setVisibility(View.GONE);
        emptyView.setVisibility(View.VISIBLE);
    }

    @Override
    public void onStart() {
        super.onStart();
        // bind transfer service
        Intent bIntent = new Intent(mActivity, TransferService.class);
        mActivity.bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    protected abstract boolean isNeedUpdateProgress();

    @Override
    public void onStop() {
        super.onStop();
        stopTimer();
        if (txService != null) {
            mActivity.unbindService(mConnection);
            txService = null;
        }
    }

    // refresh list by mTimer
    private void startTimer() {
        Log.d(DEBUG_TAG, "timer started");
        mTimer.postDelayed(new Runnable() {

            @Override
            public void run() {
                adapter.setTransferTaskInfos(getTransferTaskInfos());
                adapter.notifyDataSetChanged();
                //Log.d(DEBUG_TAG, "timer post refresh signal " + System.currentTimeMillis());
                mTimer.postDelayed(this, 1 * 1000);
            }
        }, 1 * 1000);
    }

    public void stopTimer() {
        mTimer.removeCallbacksAndMessages(null);
    }

    private void showLoading(boolean show) {
        if (mActivity == null)
            return;

        if (show) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_in));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            mListContainer.setVisibility(View.INVISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_out));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_in));

            mProgressContainer.setVisibility(View.GONE);
            mListContainer.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void onListItemClick(ListView l, View v, int position, long id) {
        if (mActionMode == null) {
            // no items selected, so perform item click actions
        } else
            // add or remove selection for current list item
            onListItemChecked(position);
    }

    public void onListItemChecked(int position) {
        if (adapter == null)
            return;

        adapter.toggleSelection(position);
        boolean itemsChecked = adapter.getCheckedItemCount() > 0;

        Log.d(DEBUG_TAG, "itemsChecked "
                + itemsChecked
                + " getCheckedItemCount "
                + adapter.getCheckedItemCount());

        if (itemsChecked && mActionMode == null)
            // there are some selected items, start the actionMode
            mActionMode = getSherlockActivity().startActionMode(new ActionModeCallback());
        else if (!itemsChecked && mActionMode != null)
            // there no selected items, finish the actionMode
            mActionMode.finish();


        if (mActionMode != null) {
            Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            mActionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    adapter.getCheckedItemCount(),
                    adapter.getCheckedItemCount()));
        }

    }

    public void updateCAB(int position) {
        if (adapter == null)
            return;

        boolean itemsChecked = adapter.getCheckedItemCount() > 0;

        Log.d(DEBUG_TAG, "itemsChecked "
                + itemsChecked
                + " getCheckedItemCount "
                + adapter.getCheckedItemCount());

        if (itemsChecked && mActionMode == null)
            // there are some selected items, start the actionMode
            mActionMode = getSherlockActivity().startActionMode(new ActionModeCallback());
        else if (!itemsChecked && mActionMode != null)
            // there no selected items, finish the actionMode
            mActionMode.finish();


        if (mActionMode != null) {
            Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            mActionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    adapter.getCheckedItemCount(),
                    adapter.getCheckedItemCount()));
        }

    }

    private class ActionModeCallback implements ActionMode.Callback {

        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            // Inflate the menu for the CAB
            MenuInflater inflater = mode.getMenuInflater();
            inflater.inflate(R.menu.transfer_list_multi_choice_menu, menu);
            mActionMode = mode;
            return true;
        }

        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            // Here you can perform updates to the CAB due to
            // an invalidate() request
            return false;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            // Respond to clicks on the actions in the CAB
            switch (item.getItemId()) {
                case R.id.transfer_multi_choice_select_all:
                    selectItems();
                    return true;
                case R.id.transfer_multi_choice_deselect_all:
                    deselectItems();
                    mode.finish(); // Action picked, so close the CAB
                    return true;
                case R.id.transfer_multi_choice_delete:
                    /*
                     * The result is only valid if the
                     * choice mode has not been set to {@link #CHOICE_MODE_NONE} and the adapter
                     * has stable IDs. ({@link ListAdapter#hasStableIds()} == {@code true})
                     */
                    long[] ids = mTransferTaskListView.getCheckedItemIds();
                    if (ids != null)
                        deleteSelectedItems(convertToTaskIds(ids));
                    mode.finish(); // Action picked, so close the CAB
                    return true;
                default:
                    return false;
            }
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            // Here you can make any necessary updates to the activity when
            // the CAB is removed. By default, selected items are deselected/unchecked.
            mActionMode = null;
        }

    }

}
