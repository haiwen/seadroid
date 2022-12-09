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
import android.view.View;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.CameraSyncEvent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.fragment.SettingsFragment;
import com.viewpagerindicator.LinePageIndicator;

import org.greenrobot.eventbus.EventBus;
import org.litepal.LitePal;

import java.util.List;


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
     * handling data from local directory page
     */
    private boolean isChooseDirPage;
    private int mCurrentPosition;
    private UploadDirectoryDBHelper databaseHelper;
    private FileDirService fileDirService;
    private List<String> selectFolderPath;
    private AccountManager accountMgr;
    private Account currentAccount;
    private List<FolderBean> litePalSelectPath;
    private FolderBean mFolderBean;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        setContentView(R.layout.cuc_activity_layout);
        if (getSupportActionBar() != null)
            getSupportActionBar().hide();


        isChooseDirPage = getIntent().getBooleanExtra(SettingsFragment.FOLDER_UPLOAD_REMOTE_DIR, false);
        mViewPager = (ViewPager) findViewById(R.id.cuc_pager);
        FragmentManager fm = getSupportFragmentManager();
        mViewPager.setAdapter(new CameraUploadConfigAdapter(fm));
        mViewPager.setOffscreenPageLimit(5);
        mIndicator = (LinePageIndicator) findViewById(R.id.cuc_indicator);
        mIndicator.setViewPager(mViewPager);
        mIndicator.setOnPageChangeListener(pageChangeListener);
        sm = SettingsManager.instance();
        accountMgr = new AccountManager(this);
        currentAccount = accountMgr.getCurrentAccount();
        databaseHelper = UploadDirectoryDBHelper.getDatabaseHelper();
        Intent bindIntent = new Intent(this, FileDirService.class);
        bindService(bindIntent, mDirConnection, Context.BIND_AUTO_CREATE);

        LitePal.getDatabase();
        litePalSelectPath = LitePal.findAll(FolderBean.class);
        for (FolderBean fb : litePalSelectPath) {
            if (fb.getEmail().equals(currentAccount.email)) {
                mFolderBean = fb;
            }
        }
        if (isChooseDirPage) {
            mIndicator.setVisibility(View.GONE);
        }

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


    public UploadDirectoryDBHelper getDatabaseHelper() {
        return databaseHelper;
    }


    public FolderBean getLitePalFolder() {
        return mFolderBean;
    }

    public void setFileList(List<String> selectFileList) {
        this.selectFolderPath = selectFileList;

    }

    public List<String> getSelectFilePath() {
        return selectFolderPath;
    }

    public void saveSettings() {
        FolderBean folderBean;

        if (litePalSelectPath == null) {
            folderBean = new FolderBean();
            folderBean.setName("folderBean");
            folderBean.setEmail(mAccount.email);
            folderBean.setRepoID(mSeafRepo.id);
            folderBean.setRepoName(mSeafRepo.name);
            folderBean.setSelectFolder(selectFolderPath);
            folderBean.save();
        } else {

            if (mFolderBean == null) {
                folderBean = new FolderBean();
                folderBean.setName("folderBean");
                folderBean.setEmail(mAccount.email);
                folderBean.setRepoID(mSeafRepo.id);
                folderBean.setRepoName(mSeafRepo.name);
                folderBean.setSelectFolder(selectFolderPath);
                folderBean.save();
            } else {
                if (currentAccount.getEmail().equals(mAccount.email)) {
                    if (mFolderBean.getRepoID().equals(mSeafRepo.id)) {
                        Toast.makeText(this, "更新已有备份目录", Toast.LENGTH_SHORT).show();
                        folderBean = new FolderBean();
                    } else {
                        Toast.makeText(this, "更新已有备份资料库和目录", Toast.LENGTH_SHORT).show();
                        folderBean = new FolderBean();
                        folderBean.setRepoID(mSeafRepo.id);
                        folderBean.setRepoName(mSeafRepo.name);
                    }
                    folderBean.setSelectFolder(selectFolderPath);
                    folderBean.updateAll("name = ?", "folderBean");
                } else {
                    FolderBean folderBean1 = null;
                    for (FolderBean fb : litePalSelectPath) {
                        if (fb.getEmail().equals(mAccount.getEmail())) {
                            folderBean1 = fb;
                        }
                    }
                    if (folderBean1 != null) {
                        if (folderBean1.getRepoID().equals(mSeafRepo.getID())) {
                            Toast.makeText(this, "更新已有备份账户和目录", Toast.LENGTH_SHORT).show();
                            folderBean = new FolderBean();
                            folderBean.setEmail(mAccount.email);

                        } else {
                            Toast.makeText(this, "更新已有备份账户、资料库和目录", Toast.LENGTH_SHORT).show();
                            folderBean = new FolderBean();
                            folderBean.setEmail(mAccount.email);
                            folderBean.setRepoID(mSeafRepo.id);
                            folderBean.setRepoName(mSeafRepo.name);
                        }
                        folderBean.setSelectFolder(selectFolderPath);
                        folderBean.updateAll("name = ?", "folderBean");
                    } else {
                        folderBean = new FolderBean();
                        folderBean.setName("folderBean");
                        folderBean.setEmail(mAccount.email);
                        folderBean.setRepoID(mSeafRepo.id);
                        folderBean.setRepoName(mSeafRepo.name);
                        folderBean.setSelectFolder(selectFolderPath);
                        folderBean.save();
                    }

                }
            }

        }

        List<FolderBean> allData = LitePal.findAll(FolderBean.class);
        for (FolderBean fb : allData) {
            if (fb.getEmail().equals(currentAccount.email)) {
                if (fileDirService != null) {
                    fileDirService.uploadFile(mAccount, fb);
                }
            }
        }

    }

    @Override
    protected void onStop() {
        super.onStop();
        if (selectFolderPath != null) {
            EventBus.getDefault().post(new CameraSyncEvent("saveSet", selectFolderPath.size()));
        }
    }

    public void saveUpdateFolder() {

        FolderBean folderBean = null;
        if (mFolderBean != null) {
            folderBean = new FolderBean();
            folderBean.setSelectFolder(selectFolderPath);
            folderBean.updateAll("name = ?", "folderBean");
        }


        List<FolderBean> all = LitePal.findAll(FolderBean.class);
        for (FolderBean fb : all) {
            if (fb.getEmail().equals(currentAccount.email)) {
                if (fileDirService != null) {
                    fileDirService.uploadFile(currentAccount, fb);
                }
            }
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
            if (isChooseDirPage)
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
