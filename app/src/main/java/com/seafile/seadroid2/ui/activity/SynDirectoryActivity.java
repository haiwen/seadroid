package com.seafile.seadroid2.ui.activity;

import android.app.Activity;
import android.os.Bundle;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.support.v7.widget.Toolbar;
import android.view.KeyEvent;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.Toast;

import com.hjq.permissions.OnPermissionCallback;
import com.hjq.permissions.XXPermissions;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.backupdirectory.BeanListManager;
import com.seafile.seadroid2.backupdirectory.Constants;
import com.seafile.seadroid2.backupdirectory.FileBean;
import com.seafile.seadroid2.backupdirectory.FileListAdapter;
import com.seafile.seadroid2.backupdirectory.FileTools;
import com.seafile.seadroid2.backupdirectory.Mtools;
import com.seafile.seadroid2.backupdirectory.OnFileItemClickListener;
import com.seafile.seadroid2.backupdirectory.OnFileItemLongClickListener;
import com.seafile.seadroid2.backupdirectory.PermissionsTools;
import com.seafile.seadroid2.backupdirectory.SelectOptions;
import com.seafile.seadroid2.backupdirectory.TabbarFileBean;
import com.seafile.seadroid2.backupdirectory.TabbarFileListAdapter;
import com.seafile.seadroid2.backupdirectory.UriTools;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class SynDirectoryActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private RecyclerView mTabbarFileRecyclerView, mFileRecyclerView;
    private ImageButton imbChangeSdCard;
    private LinearLayout mLinlPathStatusbar;
    private SelectOptions mSelectOptions;
    private List<String> mSdCardList;
    private List<String> mShowFileTypes;
    private List<String> mSelectFileTypes;
    private int mSortType;
    private List<FileBean> mFileList;
    private List<TabbarFileBean> mTabbarFileList;
    private String mCurrentPath;
    private String INITIALDIR;
    private FileListAdapter mFileListAdapter;
    private TabbarFileListAdapter mTabbarFileListAdapter;
    public static boolean isShowMorechoose = false;
    private Button mFrameLayout_bottom;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_directory);
        initview();

    }

    private void initview() {
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.file_directory);
        mTabbarFileRecyclerView = (RecyclerView) findViewById(R.id.rcv_tabbar_files_list);
        mFileRecyclerView = (RecyclerView) findViewById(R.id.rcv_files_list);
        imbChangeSdCard = (ImageButton) findViewById(R.id.imb_select_sdcard);
        mLinlPathStatusbar = (LinearLayout) findViewById(R.id.linl_path_statusbar);
        mFrameLayout_bottom = (Button) findViewById(R.id.bt_choice);

        mFileRecyclerView.setLayoutManager(new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false));
        mFileListAdapter = new FileListAdapter(this, mFileList);
        mFileRecyclerView.setAdapter(mFileListAdapter);
        mTabbarFileRecyclerView.setLayoutManager(new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false));
        mTabbarFileListAdapter = new TabbarFileListAdapter(this, mTabbarFileList);
        mTabbarFileRecyclerView.setAdapter(mTabbarFileListAdapter);

        XXPermissions.with(this).permission("android.permission.MANAGE_EXTERNAL_STORAGE").request(new OnPermissionCallback() {

            @Override
            public void onGranted(List<String> permissions, boolean all) {
                if (all) {
                    initData();
                }
            }

            @Override
            public void onDenied(List<String> permissions, boolean never) {
                if (never) {
                    Toast.makeText(SynDirectoryActivity.this, "被永久拒绝授权，请手动授予存储权限", Toast.LENGTH_LONG).show();
                    XXPermissions.startPermissionActivity(SynDirectoryActivity.this, permissions);
                } else {
                    Toast.makeText(SynDirectoryActivity.this, "获取存储权限失败", Toast.LENGTH_LONG).show();
                }
            }
        });
        mFileListAdapter.setOnItemClickListener(new OnFileItemClickListener() {
            @Override
            public void click(int position) {

                FileBean item = mFileList.get(position);
                if (isShowMorechoose) {
                    if (!item.isDir()) return;

                    if (mSelectFileTypes.size() != 0) {
                        if (!mSelectFileTypes.contains(item.getFileExtension())) {
                            Mtools.toast(SynDirectoryActivity.this, "选择类型不符合");
                            return;
                        }
                    }
                    if (mSelectOptions == null || mSelectOptions.mSingle) {
                        BeanListManager.setCheckList(mFileList, false);
                        item.setChecked(true);
                    } else {
                        item.setChecked(!item.isChecked());
                    }
                    mFileListAdapter.notifyDataSetChanged();
                } else {

                    if (mSelectOptions.fileItemListener != null) {
                        boolean state = mSelectOptions.fileItemListener.onFileItemClick(mFileRecyclerView,
                                mCurrentPath,
                                mFileList,
                                getCallBackData(),
                                mTabbarFileListAdapter,
                                mFileListAdapter,
                                item
                        );
                        if (state) return;
                    }

                    if (item.isFile()) {

                        if (mSelectFileTypes.size() != 0) {
                            if (!mSelectFileTypes.contains(item.getFileExtension())) {
                                Mtools.toast(SynDirectoryActivity.this, "选择类型不符合");
                                return;
                            }
                        }

                    } else {
                        mCurrentPath = item.getFilePath();
                        refreshFileAndTabbar(BeanListManager.TypeAddTabbar);
                    }
                }


                Utils.utilsLogInfo(false, "=====0001==========" + position);


            }
        });
        mFileListAdapter.setOnLongClickLisenter(new OnFileItemLongClickListener() {
            @Override
            public void onLongClickLisenter(int position) {
                Utils.utilsLogInfo(false, "=====0002==========" + position);
                FileBean item = mFileList.get(position);
                if (mSelectOptions.fileItemListener != null) {//如果设置了fileItemListener
                    //fileitemLongClick回调
                    boolean state = mSelectOptions.fileItemListener.onLongFileItemClick(mFileRecyclerView,
                            mCurrentPath,
                            mFileList,
                            getCallBackData(),
                            mTabbarFileListAdapter,
                            mFileListAdapter,
                            item
                    );
                }

                if (mSelectFileTypes.size() != 0) {
                    if (!mSelectFileTypes.contains(item.getFileExtension())) {
                        Mtools.toast(SynDirectoryActivity.this, "选择类型不符合");
//                            return true;
                    }
                }
                moreChooseCheckBox(position);
            }
        });

        mFrameLayout_bottom.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                for (FileBean fb : mFileList) {
                    if (fb.isChecked()) {
                        Utils.utilsLogInfo(false, "=============" + fb.getFilePath());
                        Mtools.toast(getBaseContext(), "点击了选择" + fb.getFilePath());
                        SynDirectoryActivity.this.finish();
                    }
                }
            }
        });

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                this.finish();
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public void onPointerCaptureChanged(boolean hasCapture) {

    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    private void initData() {
//        PermissionsTools.getAllNeedPermissions(this,this.getContentResolver());
        mSelectOptions = SelectOptions.getResetInstance(this);
        mSdCardList = initRootPath(this, imbChangeSdCard);
        mShowFileTypes = Arrays.asList(mSelectOptions.getShowFileTypes());
        mSelectFileTypes = Arrays.asList(mSelectOptions.getSelectFileTypes());
        mSortType = mSelectOptions.getSortType();
        mFileList = new ArrayList<>();
        mTabbarFileList = new ArrayList<>();
        refreshFileAndTabbar(BeanListManager.TypeInitTabbar);
    }

    private List<String> initRootPath(Activity activity, ImageButton imb) {
        List<String> SdCardList = FileTools.getAllSdPaths(activity);
        mCurrentPath = mSelectOptions.rootPath;
        if (mCurrentPath == null) {
            if (SdCardList.isEmpty()) {
                mCurrentPath = Constants.DEFAULT_ROOTPATH;
            } else {
                mCurrentPath = SdCardList.get(0);
            }
        }
        if (!SdCardList.isEmpty() && SdCardList.size() > 1) {
            imb.setVisibility(View.VISIBLE);
        } else {
            imb.setVisibility(View.INVISIBLE);
        }
        INITIALDIR = mCurrentPath;
        return SdCardList;
    }

    private void refreshFileAndTabbar(int tabbarType) {
        if (PermissionsTools.isAndroid11() && FileTools.isAndroidDataPath(mCurrentPath)) {//判断是否在Android/data目录下
            UriTools.upDataFileBeanListByUri(this, UriTools.file2Uri(mCurrentPath), mFileList, mFileListAdapter, mShowFileTypes, mSortType);
            UriTools.upDataTabbarFileBeanListByUri(mTabbarFileList, mTabbarFileListAdapter, mCurrentPath, tabbarType, mSdCardList);//初始化数据
        } else {
            BeanListManager.upDataFileBeanListByAsyn(mFileList, mFileListAdapter, mCurrentPath, mShowFileTypes, mSortType);
            BeanListManager.upDataTabbarFileBeanList(mTabbarFileList, mTabbarFileListAdapter, mCurrentPath, tabbarType, mSdCardList);//初始化数据
        }
    }

    private List<String> mCallBackData = new ArrayList<>();

    public List<String> getCallBackData() {
        BeanListManager.clearList(mCallBackData);
        mCallBackData = BeanListManager.getCallBackData(mFileList);
        return mCallBackData;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            if (isShowMorechoose) {
                moreChooseCheckBox(-1);
                mFrameLayout_bottom.setVisibility(View.GONE);
                return true;
            }
            if (mCurrentPath.equals(INITIALDIR) || mSdCardList.contains(mCurrentPath)) {
                return super.onKeyDown(keyCode, event);
            } else {
                mCurrentPath = FileTools.getParentPath(mCurrentPath);
                refreshFileAndTabbar(BeanListManager.TypeDelTabbar);
                return true;
            }
        } else {

            return super.onKeyDown(keyCode, event);
        }
    }

    private void moreChooseCheckBox(int position) {
        isShowMorechoose = !isShowMorechoose;
        if (mFileListAdapter != null && isShowMorechoose == true) {
            mFrameLayout_bottom.setVisibility(View.VISIBLE);
        }
        BeanListManager.setCheckBoxVisible(mFileList, mFileListAdapter, isShowMorechoose);
        switch (position) {
            case -1:
                break;
            default:
                FileBean fileBean = mFileList.get(position);
                if (!fileBean.isChecked() && fileBean.isVisible()) {
                    fileBean.setChecked(true);
                }
                mFileListAdapter.notifyDataSetChanged();
        }


    }

}
