package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.support.v4.app.ListFragment;
import android.support.v7.view.ActionMode;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;

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
public abstract class TransferTaskFragment extends ListFragment {
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
        mTransferTaskListView.setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
            @Override
            public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
                if (mActionMode == null) {
                    mActionMode = mActivity.startSupportActionMode(new ActionModeCallback());
                }

                return true;
            }
        });

        mListContainer =  root.findViewById(R.id.listContainer);
        mProgressContainer = root.findViewById(R.id.progressContainer);
        emptyView = (TextView) root.findViewById(R.id.empty);
        return root;
    }

    private List<Integer> convertToTaskIds(List<Integer> positions) {
        List<Integer> taskIds = Lists.newArrayList();
        for (int position : positions) {
            TransferTaskInfo tti = adapter.getItem(position);
            taskIds.add(tti.taskID);
        }

        return taskIds;
    }

    /**
     * deselect all items
     */
    public void deselectItems() {
        if (adapter == null) return;

        adapter.deselectAllItems();
        updateContextualActionBar();
    }

    protected abstract void deleteSelectedItems(List<Integer> ids);

    protected abstract void restartSelectedItems(List<Integer> ids);

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
        if (mActionMode != null) {
            // add or remove selection for current list item
            if (adapter == null) return;

            adapter.toggleSelection(position);
            updateContextualActionBar();
        }
    }

    /**
     *  update state of contextual action bar
     */
    public void updateContextualActionBar() {
        boolean itemsChecked = adapter.getCheckedItemCount() > 0;

        if (itemsChecked && mActionMode == null) {
            // there are some selected items, start the actionMode
            mActionMode = mActivity.startSupportActionMode(new ActionModeCallback());
            adapter.actionModeOn();
        }


        if (mActionMode != null) {
            // Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            mActionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    adapter.getCheckedItemCount(),
                    adapter.getCheckedItemCount()));
        }

    }

    /**
     * Represents a contextual mode of the user interface.
     * Action modes can be used to provide alternative interaction modes and replace parts of the normal UI until finished.
     * A Callback configures and handles events raised by a user's interaction with an action mode.
     */
    class ActionModeCallback implements ActionMode.Callback {
        private boolean allItemsSelected;

        public ActionModeCallback() {
        }

        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            // Inflate the menu for the contextual action bar (CAB)
            MenuInflater inflater = mode.getMenuInflater();
            inflater.inflate(R.menu.transfer_list_multi_choice_menu, menu);
            if (adapter == null) return true;

            adapter.actionModeOn();
            adapter.notifyDataSetChanged();

            return true;
        }

        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            // Here you can perform updates to the contextual action bar (CAB) due to
            // an invalidate() request
            return false;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            // Respond to clicks on the actions in the contextual action bar (CAB)
            final List<Integer> selectedIds = adapter.getSelectedIds();
            if (selectedIds.isEmpty()) {
                if (item.getItemId() != R.id.action_mode_select_all) {
                    mActivity.showShortToast(mActivity, R.string.action_mode_no_items_selected);
                    return true;
                }
            }

            switch (item.getItemId()) {
                case R.id.action_mode_delete:
                    List<Integer> ids = adapter.getSelectedIds();
                    if (ids != null) {
                        if (ids.size() == 0) {
                            mActivity.showShortToast(mActivity, R.string.action_mode_no_items_selected);
                            return true;
                        }

                        deleteSelectedItems(convertToTaskIds(ids));
                        deselectItems();
                    }
                    break;
                case R.id.action_mode_restart:
                    List<Integer> restartIds = adapter.getSelectedIds();
                    if (restartIds != null) {
                        if (restartIds.size() == 0) {
                            mActivity.showShortToast(mActivity, R.string.action_mode_no_items_selected);
                            return true;
                        }

                        restartSelectedItems(convertToTaskIds(restartIds));
                        deselectItems();
                    }
                    break;
                case R.id.action_mode_select_all:
                    if (!allItemsSelected) {
                        if (adapter == null) return true;

                        adapter.selectAllItems();
                        updateContextualActionBar();
                    } else {
                        if (adapter == null) return true;

                        adapter.deselectAllItems();
                        updateContextualActionBar();
                    }

                    allItemsSelected = !allItemsSelected;
                    break;
                default:
                    return false;
            }

            return true;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            if (adapter == null) return;

            adapter.deselectAllItems();
            adapter.actionModeOff();

            // Here you can make any necessary updates to the activity when
            // the contextual action bar (CAB) is removed. By default, selected items are deselected/unchecked.
            mActionMode = null;
        }

    }

}
