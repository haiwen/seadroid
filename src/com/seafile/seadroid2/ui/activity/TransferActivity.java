package com.seafile.seadroid2.ui.activity;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.OnPageChangeListener;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;
import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.ActionMode;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.notification.BaseNotificationProvider;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.ui.PagerSlidingTabStrip;
import com.seafile.seadroid2.ui.ViewPageInfo;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;
import com.seafile.seadroid2.ui.fragment.*;

import java.util.ArrayList;

public class TransferActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "TransferActivity";

    public final static String TRANSFER_FRAGMENT_TYPE = "transfer_fragment_type";
    public final static byte TYPE_DOWNLOAD = 0x0;
    public final static byte TYPE_UPLOAD = 0x1;

    private TransferTaskAdapter.TaskType whichTab = TransferTaskAdapter.TaskType.DOWNLOAD_TASK;
    private ViewPageFragmentAdapter mAdapter;
    private ViewPager pager;
    private PagerSlidingTabStrip mTabStrip;

    private Menu overFlowMenu = null;

    @Override
    protected void onNewIntent(Intent intent) {
        Bundle extras = intent.getExtras();
        if (extras != null) {
            if (extras.containsKey(BaseNotificationProvider.NOTIFICATION_MESSAGE_KEY)) {
                // extract the extra-data in the Notification
                String msg = extras.getString(BaseNotificationProvider.NOTIFICATION_MESSAGE_KEY);
                if (msg.equals(DownloadNotificationProvider.NOTIFICATION_OPEN_DOWNLOAD_TAB)) {
                    whichTab = TransferTaskAdapter.TaskType.DOWNLOAD_TASK;
                    //mTabStrip.setCurrentItem(0);
                } else if (msg.equals(BaseNotificationProvider.NOTIFICATION_OPEN_UPLOAD_TAB)) {
                    whichTab = TransferTaskAdapter.TaskType.UPLOAD_TASK;
                    //mTabStrip.setCurrentItem(1);
                }
            }
        }
    }

    private void setupTabAdapter() {
        Bundle downloadBundle = new Bundle();
        Bundle uploadBundle = new Bundle();
        downloadBundle.putByte(TRANSFER_FRAGMENT_TYPE, TYPE_DOWNLOAD);
        uploadBundle.putByte(TRANSFER_FRAGMENT_TYPE, TYPE_UPLOAD);
        mAdapter.addTab(getString(R.string.transfer_tabs_downloads), "download", DownloadTaskFragment.class, downloadBundle);
        mAdapter.addTab(getString(R.string.transfer_tabs_uploads), "upload", UploadTaskFragment.class, uploadBundle);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.transfer_list_layout);

        mTabStrip = (PagerSlidingTabStrip) findViewById(R.id.transfer_list_indicator);
        pager = (ViewPager) findViewById(R.id.transfer_list_pager);
        pager.setOffscreenPageLimit(2);
        mAdapter = new ViewPageFragmentAdapter(getSupportFragmentManager(), mTabStrip, pager);
        setupTabAdapter();
        mAdapter.notifyDataSetChanged();

        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayShowTitleEnabled(true);
        actionBar.setDisplayHomeAsUpEnabled(true);

        /** this is hacky to explicitly call onNewIntent()
         * because it was never called when start the TransferActivity
         * by notification bar */
        onNewIntent(getIntent());
    }

    public void onItemSelected() {
        // update CAB title
        if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK
                && getDownloadTaskFragment() != null) {
            getDownloadTaskFragment().updateCAB();
        } else if (whichTab == TransferTaskAdapter.TaskType.UPLOAD_TASK
                && getUploadTaskFragment() != null) {
            getUploadTaskFragment().updateCAB();
        }
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        switch (keyCode) {
        case KeyEvent.KEYCODE_MENU:
            if (overFlowMenu != null) {
                overFlowMenu.performIdentifierAction(R.id.transfer_overflow_menu, 0);
            }
        }
        return super.onKeyUp(keyCode, event);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.transfer_list_menu, menu);
        overFlowMenu = menu;
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        // MenuItem cancel = menu.findItem(R.id.cancel_transfer_tasks);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                return true;
            case R.id.cancel_transfer_tasks:
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().cancelAllDownloadTasks();

                } else getUploadTaskFragment().cancelUploadTasks();

                return true;
            case R.id.retry_failed_transfer_tasks:
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().retryAllFailedTasks();

                } else getUploadTaskFragment().retryAllFailedTasks();

                return true;
            case R.id.restart_cancelled_transfer_tasks:
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().restartAllCancelledTasks();

                } else getUploadTaskFragment().restartAllCancelledTasks();

                return true;
            case R.id.clear_failed_transfer_tasks:
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().removeAllFailedDownloadTasks();

                } else getUploadTaskFragment().removeAllFailedUploadTasks();

                return true;
            case R.id.clear_cancelled_transfer_tasks:
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().removeAllCancelledDownloadTasks();

                } else getUploadTaskFragment().removeAllCancelledUploadTasks();

                return true;
            case R.id.clear_finished_transfer_tasks:
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().removeAllFinishedDownloadTasks();

                } else getUploadTaskFragment().removeAllFinishedUploadTasks();

                return true;

        }
        return super.onOptionsItemSelected(item);
    }

    public DownloadTaskFragment getDownloadTaskFragment() {
        return (DownloadTaskFragment)getFragment(0);
    }

    public UploadTaskFragment getUploadTaskFragment() {
        return (UploadTaskFragment)getFragment(1);
    }

    public Fragment getFragment(int index) {
        return getSupportFragmentManager().findFragmentByTag(makeFragmentName(index));
    }

    private String makeFragmentName(int index) {
        return "android:switcher:" + R.id.transfer_list_pager + ":" + index;
    }

    class ViewPageFragmentAdapter extends FragmentStatePagerAdapter implements OnPageChangeListener {

        private final Context mContext;
        protected PagerSlidingTabStrip mPagerStrip;
        private final ViewPager mViewPager;
        private final ArrayList<ViewPageInfo> mTabs = new ArrayList<ViewPageInfo>();

        public ViewPageFragmentAdapter(FragmentManager fm,
                                       PagerSlidingTabStrip pageStrip, ViewPager pager) {
            super(fm);
            mContext = pager.getContext();
            mPagerStrip = pageStrip;
            mViewPager = pager;
            mViewPager.setAdapter(this);
            mPagerStrip.setViewPager(mViewPager);
        }

        public void addTab(String title, String tag, Class<?> clss, Bundle args) {
            ViewPageInfo viewPageInfo = new ViewPageInfo(title, tag, clss, args);
            addFragment(viewPageInfo);
        }

        public void addAllTab(ArrayList<ViewPageInfo> mTabs) {
            for (ViewPageInfo viewPageInfo : mTabs) {
                addFragment(viewPageInfo);
            }
        }

        private void addFragment(ViewPageInfo info) {
            if (info == null) {
                return;
            }

            // 加入tab title
            View v = LayoutInflater.from(mContext).inflate(
                    R.layout.sliding_tab_item, null, false);
            TextView title = (TextView) v.findViewById(R.id.tab_title);
            title.setText(info.title);
            mPagerStrip.addTab(v);

            mTabs.add(info);
            notifyDataSetChanged();
        }

        /**
         * 移除第一次
         */
        public void remove() {
            remove(0);
        }

        /**
         * 移除一个tab
         *
         * @param index
         *            备注：如果index小于0，则从第一个开始删 如果大于tab的数量值则从最后一个开始删除
         */
        public void remove(int index) {
            if (mTabs.isEmpty()) {
                return;
            }
            if (index < 0) {
                index = 0;
            }
            if (index >= mTabs.size()) {
                index = mTabs.size() - 1;
            }
            mTabs.remove(index);
            mPagerStrip.removeTab(index, 1);
            notifyDataSetChanged();
        }

        /**
         * 移除所有的tab
         */
        public void removeAll() {
            if (mTabs.isEmpty()) {
                return;
            }
            mPagerStrip.removeAllTab();
            mTabs.clear();
            notifyDataSetChanged();
        }

        @Override
        public int getCount() {
            return mTabs.size();
        }

        @Override
        public int getItemPosition(Object object) {
            return PagerAdapter.POSITION_NONE;
        }

        @Override
        public Fragment getItem(int position) {
            ViewPageInfo info = mTabs.get(position);
            return Fragment.instantiate(mContext, info.clss.getName(), info.args);
        }

        @Override
        public CharSequence getPageTitle(int position) {
            return mTabs.get(position).title;
        }

        @Override
        public void onPageScrolled(int i, float v, int i1) {}

        @Override
        public void onPageSelected(int position) {
            Log.d(DEBUG_TAG, "current tab index " + position);
            whichTab = (position == 0
                    ? TransferTaskAdapter.TaskType.DOWNLOAD_TASK
                    : TransferTaskAdapter.TaskType.UPLOAD_TASK);

            ActionMode mode = null;
            if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK
                    && getUploadTaskFragment() != null) {
                // slide from Upload tab to Download tab,
                // so hide the CAB of UploadTaskFragment
                mode = getUploadTaskFragment().getActionMode();
                getUploadTaskFragment().deselectItems();
            } else if (whichTab == TransferTaskAdapter.TaskType.UPLOAD_TASK
                    && getDownloadTaskFragment() != null) {
                // slide from Download tab to Upload tab,
                // so hide the CAB of DownloadTaskFragment
                mode = getDownloadTaskFragment().getActionMode();
                getDownloadTaskFragment().deselectItems();
            }

            if (mode != null)
                mode.finish();

            supportInvalidateOptionsMenu();
            //pager.setCurrentItem(position);
        }

        @Override
        public void onPageScrollStateChanged(int i) {}
    }

}