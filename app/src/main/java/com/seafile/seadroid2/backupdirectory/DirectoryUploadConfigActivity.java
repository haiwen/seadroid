package com.seafile.seadroid2.backupdirectory;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.OnPageChangeListener;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.cameraupload.CloudLibraryFragment;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.viewpagerindicator.LinePageIndicator;


/**
 * dir upload configuration helper
 */
public class DirectoryUploadConfigActivity extends BaseActivity {
    public String DEBUG_TAG = "DirectoryUploadConfigActivity";

    private ViewPager mViewPager;
    private LinePageIndicator mIndicator;
    private DirectorySelectionFragment mBucketsFragment;
    private DirCloudLibraryFragment mCloudLibFragment;
    private SettingsManager sm;
    private SeafRepo mSeafRepo;
    private Account mAccount;
    /**
     * handling data from configuration helper
     */
    private boolean isChooseBothPages;
    /**
     * handling data from cloud library page
     */
    private boolean isChooseLibPage;
    /**
     * handling data from local directory page
     */
    private boolean isChooseDirPage;
    private int mCurrentPosition;
    private UploadDirectoryDBHelper databaseHelper;
    private FileDirService fileDirService;
    private FileBean mFileBean;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        setContentView(R.layout.cuc_activity_layout);
        if (getSupportActionBar() != null)
            getSupportActionBar().hide();


        mViewPager = (ViewPager) findViewById(R.id.cuc_pager);
        FragmentManager fm = getSupportFragmentManager();
        mViewPager.setAdapter(new CameraUploadConfigAdapter(fm));
        mViewPager.setOffscreenPageLimit(5);
        mIndicator = (LinePageIndicator) findViewById(R.id.cuc_indicator);
        mIndicator.setViewPager(mViewPager);
        mIndicator.setOnPageChangeListener(pageChangeListener);
        sm = SettingsManager.instance();
        databaseHelper = UploadDirectoryDBHelper.getDatabaseHelper();
        Intent bindIntent = new Intent(this, FileDirService.class);
        bindService(bindIntent, mDirConnection, Context.BIND_AUTO_CREATE);

    }

    private ServiceConnection mDirConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName className, IBinder binder) {
            FileDirService.FileDirBinder fileDirBinder = (FileDirService.FileDirBinder) binder;
            fileDirService = fileDirBinder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName className) {
            fileDirService = null;
        }

    };

    /**
     * Page scroll listener.
     */
    private OnPageChangeListener pageChangeListener = new OnPageChangeListener() {

        @Override
        public void onPageScrollStateChanged(int scrollState) {
        }

        @Override
        public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
            mCurrentPosition = position;
        }

        @Override
        public void onPageSelected(int page) {
        }
    };

    public void saveDirUploadInfo(Account account, SeafRepo seafRepo) {
        mSeafRepo = seafRepo;
        mAccount = account;
    }

    public SeafRepo getSeafRepo() {
        return mSeafRepo;
    }

    public Account getAccount() {
        return mAccount;
    }

    public UploadDirectoryDBHelper getDatabaseHelper() {
        return databaseHelper;
    }

    public void setFileBean(FileBean fileBean) {
        mFileBean = fileBean;
    }

    public FileBean getFileBean() {
        return mFileBean;
    }

    public void saveSettings() {
        if (fileDirService != null) {
            fileDirService.uploadFile(mAccount, mFileBean.getFilePath());
        }
    }

    @Override
    public void onBackPressed() {
        if (mCurrentPosition == 0) {
            setResult(RESULT_CANCELED);
            super.onBackPressed();
        } else {
            // navigate to previous page when press back button
            mCurrentPosition -= 1;
            mIndicator.setCurrentItem(mCurrentPosition);
        }
    }

    public boolean isChooseLibPage() {
        return isChooseLibPage;
    }

    public boolean isChooseDirPage() {
        return isChooseDirPage;
    }

    public void saveDirDataPlanAllowed(boolean isAllowed) {
        sm.saveDirDataPlanAllowed(isAllowed);
    }

    class CameraUploadConfigAdapter extends FragmentStatePagerAdapter {

        public CameraUploadConfigAdapter(FragmentManager fm) {
            super(fm);
        }

        // This method controls which fragment should be shown on a specific screen.
        @Override
        public Fragment getItem(int position) {

            if (isChooseLibPage) {
                return position == 0 ? new CloudLibraryFragment() : null;
            }

            if (isChooseDirPage) {
                switch (position) {
                    case 0:
                        mBucketsFragment = new DirectorySelectionFragment();
                        return mBucketsFragment;
                    default:
                        return null;
                }

            }

            switch (position) {
                case 0:
                    return new DirConfigWelcomeFragment();
                case 1:
                    return new DirHowToUploadFragment();
                case 2:
                    mBucketsFragment = new DirectorySelectionFragment();
                    return mBucketsFragment;
                case 3:
                    mCloudLibFragment = new DirCloudLibraryFragment();
                    return mCloudLibFragment;
                case 4:
                    return new DirReadyToScanFragment();
                default:
                    return null;
            }
        }

        @Override
        public int getCount() {
            if (isChooseLibPage || isChooseDirPage)
                return 1;
            else
                return 5;
        }

    }

    @Override
    protected void onDestroy() {
        if (fileDirService != null) {
            unbindService(mDirConnection);
            fileDirService = null;
        }
        super.onDestroy();
    }
}
