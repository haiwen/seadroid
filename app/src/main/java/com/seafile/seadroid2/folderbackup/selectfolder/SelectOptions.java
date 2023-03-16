package com.seafile.seadroid2.folderbackup.selectfolder;

import android.app.Fragment;
import android.app.FragmentManager;
import android.content.Context;
import android.graphics.Color;
import android.support.v4.content.ContextCompat;
import android.view.View;

import com.seafile.seadroid2.R;

import java.util.List;

public class SelectOptions {

    private static SelectOptions mSelectOptions;
    public Integer requestCode;
    public Integer frameLayoutID;
    private Context mContext;
    public String[] mShowFileTypes;
    public String[] mSelectFileTypes;
    public Integer mSortType;
    public Boolean mSingle;
    public Integer mMaxCount;
    public Fragment mToolbarFragment;
    public Fragment mMoreChooseFragment;
    public Boolean onlyShowImages;
    public Boolean onlyShowVideos;
    public Boolean needMoreOptions;
    public Boolean needMoreChoose;
    public String[] optionsName;
    public String[] MoreChooseItemName;
    public boolean[] optionsNeedCallBack;
    public boolean[] toolbarViewNeedCallBack;
    public boolean[] moreChooseItemNeedCallBack;
    public onToolbarListener[] toolbarListeners;
    public String rootPath;
    public String toolbarMainTitle;
    public String toolbarSubtitleTitle;
    public Integer toolbarBG;
    public Integer toolbarMainTitleColor;
    public Integer toolbarSubtitleColor;
    public Integer toolbarOptionColor;
    public Integer toolbarOptionSize;
    public FragmentManager fragmentManager;
    public Boolean showToolBarFragment;
    public Integer typeLoadCustomView;

    public static SelectOptions getInstance() {
        if (mSelectOptions == null) {
            mSelectOptions = new SelectOptions();
        }
        return mSelectOptions;
    }

    public static SelectOptions getResetInstance(Context context) {
        mSelectOptions = getInstance();
        mSelectOptions.mContext = context;
        mSelectOptions.reset();
        return mSelectOptions;
    }

    public Context getContext() {
        return mContext;
    }

    public String[] getShowFileTypes() {
        if (mShowFileTypes == null) {
            return new String[]{};
        }
        return mShowFileTypes;
    }

    public int getSortType() {
        if (mSortType == null) {
            return Constants.SORT_NAME_ASC;
        }
        return mSortType;
    }

    private void reset() {
        requestCode = 100;
        frameLayoutID = null;
        mShowFileTypes = null;
        mSelectFileTypes = null;
        mSortType = Constants.SORT_NAME_ASC;
        mSingle = true;
        mMaxCount = 1;
        mToolbarFragment = null;
        mMoreChooseFragment = null;
        onlyShowImages = false;
        onlyShowVideos = false;
        needMoreOptions = false;
        needMoreChoose = false;
        optionsName = null;
        MoreChooseItemName = null;
        optionsNeedCallBack = null;
        toolbarViewNeedCallBack = null;
        moreChooseItemNeedCallBack = null;
        toolbarListeners = null;
        rootPath = null;
        toolbarMainTitle = "";
        toolbarSubtitleTitle = null;
        toolbarBG = ContextCompat.getColor(mContext, R.color.fancy_orange);
        toolbarMainTitleColor = Color.WHITE;
        toolbarSubtitleColor = Color.WHITE;
        toolbarOptionColor = Color.WHITE;
        toolbarOptionSize = 18;
        fragmentManager = null;
        showToolBarFragment = true;
        typeLoadCustomView = Constants.TYPE_CUSTOM_VIEW_NULL;
    }

    public interface onToolbarListener {
        void onClick(View view, String currentPath, List<FileBean> fileBeanList,
                     List<String> callBackData, TabbarFileListAdapter tabbarAdapter,
                     FileListAdapter fileAdapter, List<FileBean> callBackFileBeanList);
    }
}
