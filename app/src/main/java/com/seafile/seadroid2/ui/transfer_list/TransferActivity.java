
package com.seafile.seadroid2.ui.transfer_list;

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

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.TransferListLayoutBinding;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;

import java.util.ArrayList;
import java.util.List;

public class TransferActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private TransferListLayoutBinding binding;
    private final List<Fragment> fragments = new ArrayList<>();

    private Menu overFlowMenu = null;

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        Bundle extras = intent.getExtras();
        if (extras == null) {
            return;
        }

        if (!extras.containsKey(NotificationUtils.NOTIFICATION_MESSAGE_KEY)) {
            return;
        }

        // extract the extra-data in the Notification
        String msg = extras.getString(NotificationUtils.NOTIFICATION_MESSAGE_KEY);
        if (TextUtils.equals(msg, NotificationUtils.NOTIFICATION_OPEN_DOWNLOAD_TAB)) {
            binding.pager.setCurrentItem(0);
        } else if (TextUtils.equals(msg, NotificationUtils.NOTIFICATION_OPEN_UPLOAD_TAB)) {
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
        if (getUploadFragment() == null) {
            return;
        }

        if (getDownloadFragment() == null) {
            return;
        }

        ActionMode downloadActionMode = getDownloadFragment().getActionMode();
        ActionMode uploadActionMode = getUploadFragment().getActionMode();

        if (downloadActionMode == null && uploadActionMode == null) {
            return;
        }

        //
        if (downloadActionMode != null && uploadActionMode == null) {
            downloadActionMode.finish();
            getDownloadFragment().cancelSelectItems();
        }

        if (uploadActionMode != null && downloadActionMode == null) {
            uploadActionMode.finish();
            getUploadFragment().cancelSelectItems();
        }

        supportInvalidateOptionsMenu();
    }

    private void initViewPager() {
        fragments.clear();
        fragments.add(DownloadListFragment.newInstance());
        fragments.add(UploadListFragment.newInstance());

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragments);
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setOffscreenPageLimit(1);


        String[] tabs = getResources().getStringArray(R.array.transfer_list_titles);

        new TabLayoutMediator(binding.slidingTabs, binding.pager, false, new TabLayoutMediator.TabConfigurationStrategy() {
            @Override
            public void onConfigureTab(@NonNull TabLayout.Tab tab, int position) {
                tab.setText(tabs[position]);
            }
        }).attach();


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
        int whichTab = binding.slidingTabs.getSelectedTabPosition();

        if (item.getItemId() == R.id.cancel_transfer_tasks) {
            if (whichTab == 0) {
                getDownloadFragment().cancelAllTasks();
            } else {
                getUploadFragment().cancelAllTasks();
            }
        } else if (item.getItemId() == R.id.clear_all_transfer_tasks) {
            // clear all
            if (whichTab == 0) {
                getDownloadFragment().removeAllTasks();
            } else {
                getUploadFragment().removeAllTasks();
            }
        }

        return super.onOptionsItemSelected(item);
    }

    public DownloadListFragment getDownloadFragment() {
        return (DownloadListFragment) getFragment(0);
    }

    public UploadListFragment getUploadFragment() {
        return (UploadListFragment) getFragment(1);
    }

    public Fragment getFragment(int index) {
        return fragments.get(index);
    }
}