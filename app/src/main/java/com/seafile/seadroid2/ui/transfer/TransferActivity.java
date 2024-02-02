package com.seafile.seadroid2.ui.transfer;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.appcompat.view.ActionMode;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.Fragment;
import androidx.viewpager.widget.ViewPager;

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.TransferListLayoutBinding;
import com.seafile.seadroid2.notification.BaseNotificationProvider;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;

import java.util.ArrayList;
import java.util.List;

public class TransferActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private TransferListLayoutBinding binding;
    private final List<Fragment> fragments = new ArrayList<>();

    private TransferTaskAdapter.TaskType whichTab = TransferTaskAdapter.TaskType.DOWNLOAD_TASK;
    private Menu overFlowMenu = null;

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        Bundle extras = intent.getExtras();
        if (extras == null) {
            return;
        }

        if (!extras.containsKey(BaseNotificationProvider.NOTIFICATION_MESSAGE_KEY)) {
            return;
        }

        // extract the extra-data in the Notification
        String msg = extras.getString(BaseNotificationProvider.NOTIFICATION_MESSAGE_KEY);
        if (TextUtils.equals(msg, BaseNotificationProvider.NOTIFICATION_OPEN_DOWNLOAD_TAB)) {
            whichTab = TransferTaskAdapter.TaskType.DOWNLOAD_TASK;
            binding.pager.setCurrentItem(0);
        } else if (TextUtils.equals(msg, BaseNotificationProvider.NOTIFICATION_OPEN_UPLOAD_TAB)) {
            whichTab = TransferTaskAdapter.TaskType.UPLOAD_TASK;
            binding.pager.setCurrentItem(1);
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = TransferListLayoutBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        findViewById(R.id.view_toolbar_bottom_line).setVisibility(View.GONE);


        initTabLayout();
        initViewPager();

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(R.string.transfer_tasks);
        }

        /** this is hacky to explicitly call onNewIntent()
         * because it was never called when start the TransferActivity
         * by notification bar */
        onNewIntent(getIntent());
    }

    private void initTabLayout() {
        binding.slidingTabs.setTabIndicatorAnimationMode(TabLayout.INDICATOR_ANIMATION_MODE_ELASTIC);
        binding.slidingTabs.setSelectedTabIndicator(R.drawable.cat_tabs_rounded_line_indicator);
        binding.slidingTabs.setTabIndicatorFullWidth(false);
        binding.slidingTabs.setTabGravity(TabLayout.GRAVITY_CENTER);

        binding.slidingTabs.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                onTabLayoutSelected();
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });
    }

    private void onTabLayoutSelected() {
        whichTab = (binding.slidingTabs.getSelectedTabPosition() == 0
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
    }

    private void initViewPager() {
        fragments.clear();
        fragments.add(new DownloadTaskFragment());
        fragments.add(new UploadTaskFragment());

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragments);
        binding.pager.setAdapter(viewPager2Adapter);

        String downloadTabTitle = getString(R.string.transfer_tabs_downloads);
        String uploadTabTitle = getString(R.string.transfer_tabs_uploads);

        String[] tabArray = new String[]{
                downloadTabTitle, uploadTabTitle
        };

        TabLayoutMediator mediator = new TabLayoutMediator(binding.slidingTabs, binding.pager, new TabLayoutMediator.TabConfigurationStrategy() {
            @Override
            public void onConfigureTab(@NonNull TabLayout.Tab tab, int position) {
                tab.setText(tabArray[position]);
            }
        });

        mediator.attach();
    }

    public void onItemSelected() {
        // update CAB title
        if (whichTab == TransferTaskAdapter.TaskType.DOWNLOAD_TASK && getDownloadTaskFragment() != null) {
            getDownloadTaskFragment().updateContextualActionBar();
        } else if (whichTab == TransferTaskAdapter.TaskType.UPLOAD_TASK && getUploadTaskFragment() != null) {
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

        Toolbar toolbar = getActionBarToolbar();
        toolbar.inflateMenu(R.menu.transfer_list_menu);
        toolbar.setOnMenuItemClickListener(this);

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
        return (DownloadTaskFragment) getFragment(0);
    }

    public UploadTaskFragment getUploadTaskFragment() {
        return (UploadTaskFragment) getFragment(1);
    }

    public Fragment getFragment(int index) {
        return fragments.get(index);
    }
}