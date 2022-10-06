package com.seafile.seadroid2.loopimages;

import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.PersistableBundle;
import android.support.annotation.NonNull;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.view.ViewPager;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.cameraupload.BucketsFragment;
import com.seafile.seadroid2.cameraupload.CameraUploadConfigActivity;
import com.seafile.seadroid2.cameraupload.CloudLibraryFragment;
import com.seafile.seadroid2.cameraupload.ConfigWelcomeFragment;
import com.seafile.seadroid2.cameraupload.HowToUploadFragment;
import com.seafile.seadroid2.cameraupload.ReadyToScanFragment;
import com.seafile.seadroid2.cameraupload.WhatToUploadFragment;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;
import com.seafile.seadroid2.ui.fragment.SettingsFragment;
import com.viewpagerindicator.LinePageIndicator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The configuration screen for the {@link LoopImagesWidget LoopImagesWidget} AppWidget.
 */
public class LoopImagesWidgetConfigureActivity extends BaseActivity {

    private static final String PREFS_NAME = "com.seafile.seadroid2.loopimages.LoopImagesWidget";
    private static final String PREF_PREFIX_KEY = "appwidget_";
    public static final int CHOOSE_LOOPIMAGES_REQUEST = 1;

    public static SettingsManager settingsMgr = null;
    private static AccountManager accountManager = null;
    private static List<Account> accountList = null;
    private static Map<String, Account> signatureAccount = null;

    private ViewPager mViewPager;
    private LinePageIndicator mIndicator;

    private boolean hasImage = false;
    private int mCurrentPosition;

    private int mAppWidgetId = AppWidgetManager.INVALID_APPWIDGET_ID;

    private static LoopImagesWidgetConfigureActivity existActivity = null;

//    View.OnClickListener mOnClickListener = new View.OnClickListener() {
//        public void onClick(View v) {
//            final Context context = LoopImagesWidgetConfigureActivity.this;
//
//            // When the button is clicked, store the string locally
//            String widgetText = mAppWidgetText.getText().toString();
//            saveTitlePref(context, mAppWidgetId, widgetText);
//
//            // It is the responsibility of the configuration activity to update the app widget
//            AppWidgetManager appWidgetManager = AppWidgetManager.getInstance(context);
//            LoopImagesWidget.updateAppWidget(context, appWidgetManager, mAppWidgetId);
//
//            // Make sure we pass back the original appWidgetId
//            Intent resultValue = new Intent();
//            resultValue.putExtra(AppWidgetManager.EXTRA_APPWIDGET_ID, mAppWidgetId);
//            setResult(RESULT_OK, resultValue);
//            finish();
//        }
//    };

    public LoopImagesWidgetConfigureActivity() {
        super();
    }

    public int getAppWidgetId(){
        return mAppWidgetId;
    }

//    // Write the prefix to the SharedPreferences object for this widget
//    static void saveTitlePref(Context context, int appWidgetId, String text) {
//        SharedPreferences.Editor prefs = context.getSharedPreferences(PREFS_NAME, 0).edit();
//        prefs.putString(PREF_PREFIX_KEY + appWidgetId, text);
//        prefs.apply();
//    }
//
//    // Read the prefix from the SharedPreferences object for this widget.
//    // If there is no preference saved, get the default from a resource
//    static String loadTitlePref(Context context, int appWidgetId) {
//        SharedPreferences prefs = context.getSharedPreferences(PREFS_NAME, 0);
//        String titleValue = prefs.getString(PREF_PREFIX_KEY + appWidgetId, null);
//        if (titleValue != null) {
//            return titleValue;
//        } else {
//            return context.getString(R.string.appwidget_text);
//        }
//    }

    static public SettingsManager getSettingsManager(){
        if(settingsMgr == null){
            settingsMgr = SettingsManager.instance();
        }
        return settingsMgr;
    }

    public void saveDataPlanAllowed(boolean isAllowed) {
        getSettingsManager().saveLoopImagesWidgetDataPlanAllowed(mAppWidgetId, isAllowed);
    }

    static public boolean getDataPlanAllowed(int mAppWidgetId) {
        return getSettingsManager().getLoopImagesWidgetDataPlanAllowed(mAppWidgetId);
    }

    static public DirInfo getDirInfoFromStringList(Context context, List<String> dirInfo){
        init(context);
        if(dirInfo.size() != 5){
            return null;
        }
        Account account = signatureAccount.get(dirInfo.get(0));
        return new DirInfo(account, dirInfo.get(1), dirInfo.get(2), dirInfo.get(3), dirInfo.get(4));
    }

    static public DirInfo getDirInfoFromString(Context context, String dirInfoStr){
        if(dirInfoStr == null || dirInfoStr.length() == 0){
            return null;
        }
        init(context);
        List<String> infoList = Arrays.asList(TextUtils.split(dirInfoStr, DirInfo.spliter));
        if(infoList.size() != 5){
            return null;
        }
        return getDirInfoFromStringList(context, infoList);
    }

    static public List<DirInfo> getDirInfo(Context context, int appWidgetId) {
        init(context);
        List<String> dirInfoStrs = getSettingsManager().getLoopImagesWidgetDirInfo(appWidgetId);
        List<DirInfo> dirInfos = new ArrayList<DirInfo>();
        for(String info: dirInfoStrs){
            dirInfos.add(getDirInfoFromString(context, info));
        }
        return dirInfos;
    }

    static public void deleteDirInfo(int appWidgetId) {
        getSettingsManager().deleteLoopImagesWidgetInfo(appWidgetId);
    }

    public static void init(Context context){
        if(settingsMgr == null) {
            settingsMgr = SettingsManager.instance();
        }

        if(accountManager == null) {
            accountManager = new AccountManager(context);
        }

        if(accountList == null) {
            accountList = accountManager.getAccountList();
        }

        if(signatureAccount == null) {
            signatureAccount = new HashMap<String, Account>();
            for(Account taccount: accountList){
                signatureAccount.put(taccount.getSignature(), taccount);
            }
        }
    }

    public LinePageIndicator getIndicator(){
        return mIndicator;
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        existActivity = null;
    }

    @Override
    public void onCreate(Bundle icicle) {
        super.onCreate(icicle);

        if(existActivity != null){
            existActivity.finish();
        }
        existActivity = this;
        // Set the result to CANCELED.  This will cause the widget host to cancel
        // out of the widget placement if the user presses the back button.
        setResult(RESULT_CANCELED);

        setContentView(R.layout.loop_images_configure_activity_layout);

        init(getApplicationContext());

        // Find the widget id from the intent.
        Intent intent = getIntent();

        Bundle extras = intent.getExtras();
        if (extras != null) {
            mAppWidgetId = extras.getInt(
                    AppWidgetManager.EXTRA_APPWIDGET_ID, AppWidgetManager.INVALID_APPWIDGET_ID);
            String dirInfoStr = extras.getString(LoopImagesWidget.DIR_INFO);
            String fileName = extras.getString(LoopImagesWidget.IMAGE_NAME);
            if(dirInfoStr != null && dirInfoStr.length() > 0 && fileName != null && fileName.length() > 0){
                hasImage = true;
            }
        }

        // If this activity was started with an intent without an app widget ID, finish with an error.
        if (mAppWidgetId == AppWidgetManager.INVALID_APPWIDGET_ID) {
            finish();
            return;
        }

        mViewPager = (ViewPager) findViewById(R.id.loopimages_configure_pager);
        FragmentManager fm = getSupportFragmentManager();
        mViewPager.setAdapter(new LoopImagesConfigAdapter(fm));
        mViewPager.setOffscreenPageLimit(6);

        mIndicator = (LinePageIndicator) findViewById(R.id.loopimages_configure_indicator);
        mIndicator.setViewPager(mViewPager);
        mIndicator.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {

            @Override
            public void onPageScrollStateChanged(int scrollState) {}

            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                mCurrentPosition = position;
            }

            @Override
            public void onPageSelected(int page){}
        });
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState, @NonNull PersistableBundle outPersistentState) {
        super.onSaveInstanceState(outState, outPersistentState);
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

    class LoopImagesConfigAdapter extends FragmentStatePagerAdapter {

        public LoopImagesConfigAdapter(FragmentManager fm) {
            super(fm);
        }

        // This method controls which fragment should be shown on a specific screen.
        @Override
        public Fragment getItem(int position) {
            // Assign the appropriate screen to the fragment object, based on which screen is displayed.
            switch (position) {
                case 0:
//                    return new ChosenLibraryFragment();
                    if(hasImage) {
                        return new ShowImageFragment();
                    }else{
                        return new ChosenLibraryFragment();
                    }
                case 1:
                    if(hasImage){
//                        mIndicator.setVisibility(View.VISIBLE);
                        return new ChosenLibraryFragment();
                    }else{
                        return null;
                    }
                default:
                    return null;
            }
        }

        @Override
        public int getCount() {
            if(hasImage){
                return 2;
            }
            return 1;
        }

    }

}