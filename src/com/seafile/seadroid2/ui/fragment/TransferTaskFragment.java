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
import android.view.ContextMenu;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import com.actionbarsherlock.app.SherlockListFragment;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.*;
import com.seafile.seadroid2.ui.activity.TransferActivity;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;

import java.util.List;

/**
 * Base class for transfer task fragments
 *
 * Created by Logan on 14/12/22.
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

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        mActivity = (TransferActivity) activity;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.transfer_task_fragment, container, false);
        mTransferTaskListView = (ListView) root.findViewById(android.R.id.list);
        mTransferTaskListView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);

        mListContainer =  root.findViewById(R.id.listContainer);
        mProgressContainer = root.findViewById(R.id.progressContainer);
        emptyView = (TextView) root.findViewById(R.id.empty);
        return root;
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

        registerForContextMenu(mTransferTaskListView);

        // Toast.makeText(mActivity, "Loading animations", Toast.LENGTH_LONG).show();
        showLoading(true);

        // bind transfer service
        Intent bIntent = new Intent(mActivity, TransferService.class);
        mActivity.bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
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
                Log.d(DEBUG_TAG, "timer post refresh signal " + System.currentTimeMillis());
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
    public void onCreateContextMenu(ContextMenu menu, View v,
                                    ContextMenu.ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
        android.view.MenuInflater inflater = mActivity.getMenuInflater();
        inflater.inflate(R.menu.transfer_task_menu, menu);

        ListView listView = (ListView)v;
        AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) menuInfo;
        TransferTaskInfo taskInfo = (TransferTaskInfo)listView.getItemAtPosition(info.position);

        android.view.MenuItem itemCancel = menu.findItem(R.id.cancel);
        android.view.MenuItem itemRetry = menu.findItem(R.id.retry);
        android.view.MenuItem itemRemove = menu.findItem(R.id.remove);
        android.view.MenuItem itemRemoveAllCancelled = menu.findItem(R.id.remove_all_cancelled);
        android.view.MenuItem itemRemoveAllFinished = menu.findItem(R.id.remove_all_finished);

        itemCancel.setVisible(false);
        itemRetry.setVisible(false);
        itemRemove.setVisible(false);
        itemRemoveAllCancelled.setVisible(false);
        itemRemoveAllFinished.setVisible(false);

        switch (taskInfo.state) {
            case INIT:
                itemCancel.setVisible(true);
                break;
            case TRANSFERRING:
                itemCancel.setVisible(true);
                break;
            case CANCELLED:
                itemRetry.setVisible(true);
                itemRemove.setVisible(true);
                itemRemoveAllCancelled.setVisible(true);
                break;
            case FAILED:
                itemRetry.setVisible(true);
                itemRemove.setVisible(true);
                break;
            case FINISHED:
                itemRemove.setVisible(true);
                itemRemoveAllFinished.setVisible(true);
                break;
        }
    }
}
