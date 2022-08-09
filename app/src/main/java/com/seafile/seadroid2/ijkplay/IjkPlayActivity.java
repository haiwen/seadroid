package com.seafile.seadroid2.ijkplay;

import android.content.Intent;
import android.os.Bundle;
import android.transition.Transition;
import android.util.Log;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.play.VideoLinkStateListener;
import com.seafile.seadroid2.play.VideoLinkTask;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import tv.danmaku.ijk.media.player.IMediaPlayer;

public class IjkPlayActivity extends BaseActivity implements VideoLinkStateListener {
    VideoPlayerIJK ijkPlayer;

    private Account mAccount;
    private String fileName;
    private String mRepoID;
    private String mFilePath;
    private SeafConnection sc;
    private String mFileLink;
    private boolean isTransition;
    private Transition transition;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Utils.hideSystemNavigationBar(this);
        setContentView(R.layout.activity_ijkplay);
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
        VideoPlayerIJK videoPlayerIJK = findViewById(R.id.ijk_player);
        videoPlayerIJK.setListener(new VideoPlayerListener() {
            @Override
            public void onBufferingUpdate(IMediaPlayer iMediaPlayer, int i) {
                Log.e("yuan", "onBufferingUpdate");
            }

            @Override
            public void onCompletion(IMediaPlayer iMediaPlayer) {
                Log.e("yuan", "onCompletion");

            }

            @Override
            public boolean onError(IMediaPlayer iMediaPlayer, int i, int i1) {
                Log.e("yuan", "onError");

                return false;
            }

            @Override
            public boolean onInfo(IMediaPlayer iMediaPlayer, int i, int i1) {
                Log.e("yuan", "onInfo");

                return false;
            }

            @Override
            public void onPrepared(IMediaPlayer iMediaPlayer) {
                Log.e("yuan", "onPrepared");


            }

            @Override
            public void onSeekComplete(IMediaPlayer iMediaPlayer) {
                Log.e("yuan", "onSeekComplete");


            }

            @Override
            public void onVideoSizeChanged(IMediaPlayer iMediaPlayer, int i, int i1, int i2, int i3) {
                Log.e("yuan", "onVideoSizeChanged");

            }
        });
        Log.e("seafile", "path=http://7xjmzj.com1.z0.glb.clouddn.com/20171026175005_JObCxCE2.mp4");

        videoPlayerIJK.setVideoPath(mFileLink);
        videoPlayerIJK.start();
    }

    @Override
    public void onSuccess(String fileLink) {
        mFileLink = fileLink;
        init();
    }

    @Override
    public void onError(String errMsg) {

    }

    @Override
    public void onPointerCaptureChanged(boolean hasCapture) {

    }
}