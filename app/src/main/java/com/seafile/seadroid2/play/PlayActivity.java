package com.seafile.seadroid2.play;

import android.annotation.TargetApi;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.support.v4.view.ViewCompat;
import android.transition.Transition;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;
import com.shuyu.gsyvideoplayer.utils.OrientationUtils;
import com.shuyu.gsyvideoplayer.video.base.GSYVideoPlayer;

import java.io.File;


/**
 * @Description: [ video play online activity ]
 * @Author: [Saud]
 * @CreateDate: [2017/7/22 2:09]
 * @UpDate: [2017/7/22 2:09]
 * @Version: [v1.0]
 */
public class PlayActivity extends BaseActivity implements VideoLinkStateListener {


    private SampleVideo videoPlayer;

    private Account mAccount;
    private String fileName;
    private String mRepoID;
    private String mFilePath;
    private SeafConnection sc;
    private String mFileLink;
    private boolean isTransition;
    private Transition transition;
    private OrientationUtils orientationUtils;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Utils.hideSystemNavigationBar(this);
        setContentView(R.layout.activity_play);
        Intent intent = getIntent();
        mAccount = intent.getParcelableExtra("account");
        fileName = intent.getStringExtra("fileName");
        mRepoID = intent.getStringExtra("repoID");
        mFilePath = intent.getStringExtra("filePath");
        VideoLinkTask task = new VideoLinkTask(mAccount, mRepoID, mFilePath, this);
        ConcurrentAsyncTask.execute(task);
        isTransition = getIntent().getBooleanExtra("TRANSITION", false);
    }

    private void init() {
        videoPlayer = (SampleVideo) findViewById(R.id.video_player);

        //Set the rotation
        orientationUtils = new OrientationUtils(this, videoPlayer);
        //Set the full screen button function, which use the screen instead of the full screen
        videoPlayer.getFullscreenButton().setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                orientationUtils.resolveByClick();
            }
        });

        videoPlayer.setUp(mFileLink, true, new File(getCacheDir().toString()), "");

        // videoPlayer.getFullscreenButton().setVisibility(View.GONE);
        videoPlayer.setNeedShowWifiTip(true);
        videoPlayer.setShowPauseCover(true);

        //set play title
        videoPlayer.getTitleTextView().setVisibility(View.VISIBLE);
        videoPlayer.getTitleTextView().setText(fileName);

        //set can touch change  wiget
        videoPlayer.setIsTouchWiget(true);

        //set backup button
        videoPlayer.getBackButton().setVisibility(View.VISIBLE);
        videoPlayer.getBackButton().setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                onBackPressed();
            }
        });

        //transition animations
        initTransition();
    }


    @Override
    protected void onPause() {
        super.onPause();
        videoPlayer.onVideoPause();
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (videoPlayer != null) {
            videoPlayer.onVideoResume();
        }
    }

    @TargetApi(Build.VERSION_CODES.KITKAT)
    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (orientationUtils != null)
            orientationUtils.releaseListener();
    }

    @Override
    public void onBackPressed() {
        //Return to normal
        if (orientationUtils.getScreenType() == ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE) {
            videoPlayer.getFullscreenButton().performClick();
            return;
        }
        //Release resources
        videoPlayer.setStandardVideoAllCallBack(null);
        GSYVideoPlayer.releaseAllVideos();
        if (isTransition && Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            super.onBackPressed();
        } else {
            new Handler().postDelayed(new Runnable() {
                @Override
                public void run() {
                    finish();
                    overridePendingTransition(R.anim.abc_fade_in, R.anim.abc_fade_out);
                }
            }, 500);
        }
    }


    @Override
    public void onSuccess(String fileLink) {
        mFileLink = fileLink;
        init();
    }

    @Override
    public void onError(String errMsg) {
        showShortToast(this, errMsg);
    }


    private void initTransition() {
        if (isTransition && Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            postponeEnterTransition();
            ViewCompat.setTransitionName(videoPlayer, "IMG_TRANSITION");
            addTransitionListener();
            startPostponedEnterTransition();
        } else {
            videoPlayer.startPlayLogic();
        }
    }

    @TargetApi(Build.VERSION_CODES.LOLLIPOP)
    private boolean addTransitionListener() {
        transition = getWindow().getSharedElementEnterTransition();
        if (transition != null) {
            transition.addListener(new OnTransitionListener() {
                @Override
                public void onTransitionEnd(Transition transition) {
                    super.onTransitionEnd(transition);
                    videoPlayer.startPlayLogic();
                    transition.removeListener(this);
                }
            });
            return true;
        }
        return false;
    }
}
