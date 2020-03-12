package com.seafile.seadroid2.ui.activity;

import android.content.Intent;
import android.os.Bundle;
import android.support.design.widget.TabLayout;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v7.view.ActionMode;
import android.support.v7.widget.Toolbar;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.notification.BaseNotificationProvider;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;
import com.seafile.seadroid2.ui.fragment.DownloadTaskFragment;
import com.seafile.seadroid2.ui.fragment.UploadTaskFragment;

public class TransferActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "TransferActivity";

    private TransferTaskAdapter.TaskType whichTab = TransferTaskAdapter.TaskType.DOWNLOAD_TASK;
    private TransferTabsAdapter tabsAdapter;
    private ViewPager pager;
    private TabLayout mTabLayout;

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
                    pager.setCurrentItem(0);
                } else if (msg.equals(BaseNotificationProvider.NOTIFICATION_OPEN_UPLOAD_TAB)) {
                    whichTab = TransferTaskAdapter.TaskType.UPLOAD_TASK;
                    pager.setCurrentItem(1);
                }
            }
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.transfer_list_layout);

        findViewById(R.id.view_toolbar_bottom_line).setVisibility(View.GONE);

        tabsAdapter = new TransferTabsAdapter(getSupportFragmentManager());

        pager = (ViewPager) findViewById(R.id.transfer_list_pager);
        pager.setAdapter(tabsAdapter);
        mTabLayout = (TabLayout) findViewById(R.id.sliding_tabs);
        mTabLayout.setTabsFromPagerAdapter(tabsAdapter);
        mTabLayout.setupWithViewPager(pager);
        mTabLayout.setOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                // Log.d(DEBUG_TAG, "current tab index " + position);
                whichTab = (tab.getPosition() == 0
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
                pager.setCurrentItem(tab.getPosition());
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.transfer_tasks);

        /** this is hacky to explicitly call onNewIntent()
         * because it was never called when start the TransferActivity
         * by notification bar */
        onNewIntent(getIntent());
    }

    public void onItemSelected() {
        // update CAB title
        if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK
                && getDownloadTaskFragment() != null) {
            getDownloadTaskFragment().updateContextualActionBar();
        } else if (whichTab == TransferTaskAdapter.TaskType.UPLOAD_TASK
                && getUploadTaskFragment() != null) {
            getUploadTaskFragment().updateContextualActionBar();
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
        getActionBarToolbar().inflateMenu(R.menu.transfer_list_menu);
        getActionBarToolbar().setOnMenuItemClickListener(this);
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        // MenuItem cancel = menu.findItem(R.id.cancel_transfer_tasks);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.cancel_transfer_tasks:
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().cancelAllDownloadTasks();

                } else getUploadTaskFragment().cancelUploadTasks();

                return true;
            case R.id.clear_all_transfer_tasks: // actually this only clear {@link TaskState#FINISHED}, {@link TaskState#FAILED} and {@link TaskState#CANCELLED} tasks.
                if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK) {
                    getDownloadTaskFragment().removeAllDownloadTasks();

                } else getUploadTaskFragment().removeAllUploadTasks();

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

    /**
     * Adapter for {@link ViewPager} to bind DownloadTaskFragment and UploadTaskFragment
     */
    public class TransferTabsAdapter extends FragmentPagerAdapter {

        private String downloadTabTitle;
        private String uploadTabTitle;

        public TransferTabsAdapter(FragmentManager fm) {
            super(fm);
            downloadTabTitle = getString(R.string.transfer_tabs_downloads);
            uploadTabTitle = getString(R.string.transfer_tabs_uploads);
        }

        private DownloadTaskFragment downloadsFragment = null;
        private UploadTaskFragment uploadsFragment = null;

        @Override
        public Fragment getItem(int position) {
            switch (position) {
            case 0:
                if (downloadsFragment == null) {
                    downloadsFragment = new DownloadTaskFragment();
                }
                return downloadsFragment;
            case 1:
                if (uploadsFragment == null) {
                    uploadsFragment = new UploadTaskFragment();
                }
                return uploadsFragment;
            default:
                return new Fragment();
            }
        }

        @Override
        public CharSequence getPageTitle(int position) {
            switch (position) {
            case 0:
                return downloadTabTitle;
            case 1:
                return uploadTabTitle;

            default:
                return null;
            }
        }

        @Override
        public int getCount() {
            return 2;
        }

    }

}