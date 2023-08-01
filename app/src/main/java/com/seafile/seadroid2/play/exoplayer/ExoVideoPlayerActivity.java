package com.seafile.seadroid2.play.exoplayer;

import android.content.Intent;
import android.os.Bundle;

import androidx.media3.common.AudioAttributes;
import androidx.media3.common.MediaItem;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.ui.PlayerView;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.play.VideoLinkStateListener;
import com.seafile.seadroid2.play.VideoLinkTask;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

public class ExoVideoPlayerActivity extends BaseActivity implements VideoLinkStateListener {
    private ExoPlayer player;
    private PlayerView playerView;

    private Account mAccount;
    private String fileName;
    private String mRepoID;
    private String mFilePath;
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


        player = new ExoPlayer.Builder(this).build();

        playerView.setPlayer(player);
        playerView.setShowBuffering(PlayerView.SHOW_BUFFERING_WHEN_PLAYING);

        player.setAudioAttributes(AudioAttributes.DEFAULT, true);
        player.setPlayWhenReady(true);
        player.setMediaSource(getMediaSource(mFileLink));
        player.prepare();

    }

    @Override
    public void onError(String errMsg) {
        ToastUtils.showLong(errMsg);
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (player != null) {
            player.stop();
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (player != null) {
            player.stop();
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
        HttpDataSource.Factory httpDataSourceFactory = new DefaultHttpDataSource.Factory()
                .setUserAgent(userAgent)
                .setConnectTimeoutMs(DefaultHttpDataSource.DEFAULT_CONNECT_TIMEOUT_MILLIS)
                .setReadTimeoutMs(DefaultHttpDataSource.DEFAULT_READ_TIMEOUT_MILLIS)
                .setAllowCrossProtocolRedirects(true);
        DataSource.Factory dataSourceFactory = new DefaultDataSource.Factory(
                this,
                httpDataSourceFactory
        );

        return new ProgressiveMediaSource.Factory(dataSourceFactory)
                .createMediaSource(MediaItem.fromUri(url));
    }
}
