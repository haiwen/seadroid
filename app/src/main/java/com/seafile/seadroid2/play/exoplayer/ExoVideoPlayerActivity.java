package com.seafile.seadroid2.play.exoplayer;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.exoplayer2.DefaultLoadControl;
import com.google.android.exoplayer2.DefaultRenderersFactory;
import com.google.android.exoplayer2.ExoPlayerFactory;
import com.google.android.exoplayer2.SimpleExoPlayer;
import com.google.android.exoplayer2.audio.AudioAttributes;
import com.google.android.exoplayer2.source.ExtractorMediaSource;
import com.google.android.exoplayer2.source.MediaSource;
import com.google.android.exoplayer2.trackselection.DefaultTrackSelector;
import com.google.android.exoplayer2.ui.PlayerView;
import com.google.android.exoplayer2.upstream.DefaultDataSourceFactory;
import com.google.android.exoplayer2.util.Util;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.play.VideoLinkStateListener;
import com.seafile.seadroid2.play.VideoLinkTask;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

public class ExoVideoPlayerActivity extends BaseActivity implements VideoLinkStateListener {
    private SimpleExoPlayer player;
    private PlayerView playerView;

    private Account mAccount;
    private String fileName;
    private String mRepoID;
    private String mFilePath;
    private SeafConnection sc;
    private String mFileLink;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Utils.hideSystemNavigationBar(this);
        setContentView(R.layout.activity_video_player);

        Intent intent = getIntent();
        mAccount = intent.getParcelableExtra("account");
        fileName = intent.getStringExtra("fileName");
        mRepoID = intent.getStringExtra("repoID");
        mFilePath = intent.getStringExtra("filePath");
        VideoLinkTask task = new VideoLinkTask(mAccount, mRepoID, mFilePath, this);
        ConcurrentAsyncTask.execute(task);
    }

    @Override
    public void onSuccess(String fileLink) {
        mFileLink = fileLink;
        init();
    }

    private void init() {
        playerView = findViewById(R.id.player_view);

        if (player != null) {
            return;
        }


        player = ExoPlayerFactory.newSimpleInstance(
                new DefaultRenderersFactory(this),
                new DefaultTrackSelector(),
                new DefaultLoadControl());

        playerView.setPlayer(player);
        playerView.setShowBuffering(true);

        player.setAudioAttributes(AudioAttributes.DEFAULT);
        player.setPlayWhenReady(true);
        MediaSource mediaSource = getMediaSource(mFileLink);

        player.prepare(mediaSource, true, false);
    }

    @Override
    public void onError(String errMsg) {
        ToastUtils.showLong(errMsg);
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (player != null) {
            player.stop(false);
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (player != null) {
            player.stop(true);
        }
    }

    @Override
    protected void onDestroy() {
        if (player != null) {
            player.release();
        }

        super.onDestroy();
    }

    private MediaSource getMediaSource(String url) {
        String userAgent = Util.getUserAgent(this, AppUtils.getAppName());

        DefaultDataSourceFactory dataSourceFactory = new DefaultDataSourceFactory(this, userAgent);
        MediaSource videoSource = new ExtractorMediaSource.Factory(dataSourceFactory).createMediaSource(Uri.parse(url));
        return videoSource;
    }
}
