package com.seafile.seadroid2.exoplayer;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.transition.Transition;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.ProgressBar;

import com.google.android.exoplayer2.DefaultLoadControl;
import com.google.android.exoplayer2.ExoPlaybackException;
import com.google.android.exoplayer2.ExoPlayer;
import com.google.android.exoplayer2.ExoPlayerFactory;
import com.google.android.exoplayer2.LoadControl;
import com.google.android.exoplayer2.PlaybackParameters;
import com.google.android.exoplayer2.SimpleExoPlayer;
import com.google.android.exoplayer2.extractor.DefaultExtractorsFactory;
import com.google.android.exoplayer2.extractor.ExtractorsFactory;
import com.google.android.exoplayer2.source.ExtractorMediaSource;
import com.google.android.exoplayer2.source.MediaSource;
import com.google.android.exoplayer2.source.TrackGroupArray;
import com.google.android.exoplayer2.text.Cue;
import com.google.android.exoplayer2.text.TextRenderer;
import com.google.android.exoplayer2.trackselection.AdaptiveTrackSelection;
import com.google.android.exoplayer2.trackselection.DefaultTrackSelector;
import com.google.android.exoplayer2.trackselection.TrackSelection;
import com.google.android.exoplayer2.trackselection.TrackSelectionArray;
import com.google.android.exoplayer2.trackselection.TrackSelector;
import com.google.android.exoplayer2.ui.SimpleExoPlayerView;
import com.google.android.exoplayer2.upstream.BandwidthMeter;
import com.google.android.exoplayer2.upstream.DataSource;
import com.google.android.exoplayer2.upstream.DefaultBandwidthMeter;
import com.google.android.exoplayer2.upstream.DefaultDataSourceFactory;
import com.google.android.exoplayer2.util.Util;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.play.VideoLinkStateListener;
import com.seafile.seadroid2.play.VideoLinkTask;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.songwenju.androidtools.util.LogUtil;

import java.util.Formatter;
import java.util.List;
import java.util.Locale;

public class ExoPlayActivity extends BaseActivity implements VideoLinkStateListener {
    private Account mAccount;
    private String fileName;
    private String mRepoID;
    private String mFilePath;
    private SeafConnection sc;
    private String mFileLink;
    private boolean isTransition;
    private Transition transition;
    private Context mContext;
    private ProgressBar mProgressBar;

    private SimpleExoPlayerView mExoPlayerView;
    private SimpleExoPlayer mSimpleExoPlayer;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                WindowManager.LayoutParams.FLAG_FULLSCREEN);
        super.onCreate(savedInstanceState);
        setContentView(R.layout.exo_play_activity);
        mContext = this;

        Intent intent = getIntent();
        mAccount = intent.getParcelableExtra("account");
        fileName = intent.getStringExtra("fileName");
        mRepoID = intent.getStringExtra("repoID");
        mFilePath = intent.getStringExtra("filePath");
        VideoLinkTask task = new VideoLinkTask(mAccount, mRepoID, mFilePath, this);
        ConcurrentAsyncTask.execute(task);
        isTransition = getIntent().getBooleanExtra("TRANSITION", false);

        mProgressBar = (ProgressBar) findViewById(R.id.progressBar);
        initPlayer();


    }

    /**
     * 初始化player
     */
    private void initPlayer() {
        //1. 创建一个默认的 TrackSelector
        BandwidthMeter bandwidthMeter = new DefaultBandwidthMeter();
        TrackSelection.Factory videoTackSelectionFactory =
                new AdaptiveTrackSelection.Factory(bandwidthMeter);
        TrackSelector trackSelector =
                new DefaultTrackSelector(videoTackSelectionFactory);
        LoadControl loadControl = new DefaultLoadControl();
        //2.创建ExoPlayer
        mSimpleExoPlayer = ExoPlayerFactory.newSimpleInstance(this, trackSelector, loadControl);
        //3.创建SimpleExoPlayerView
        mExoPlayerView = (SimpleExoPlayerView) findViewById(R.id.exoView);
        //4.为SimpleExoPlayer设置播放器
        mExoPlayerView.setPlayer(mSimpleExoPlayer);
    }

    private void playVideo(String url) {
        //测量播放过程中的带宽。 如果不需要，可以为null。
        DefaultBandwidthMeter bandwidthMeter = new DefaultBandwidthMeter();
        // 生成加载媒体数据的DataSource实例。
        DataSource.Factory dataSourceFactory
                = new DefaultDataSourceFactory(ExoPlayActivity.this,
                Util.getUserAgent(ExoPlayActivity.this, "useExoplayer"), bandwidthMeter);
        // 生成用于解析媒体数据的Extractor实例。
        ExtractorsFactory extractorsFactory = new DefaultExtractorsFactory();

        Uri playerUri = Uri.parse(url);
        // MediaSource代表要播放的媒体。
        MediaSource videoSource = new ExtractorMediaSource(playerUri, dataSourceFactory, extractorsFactory,
                null, null);
        //Prepare the player with the source.
        mSimpleExoPlayer.prepare(videoSource);
        //添加监听的listener
//        mSimpleExoPlayer.setVideoListener(mVideoListener);
        mSimpleExoPlayer.addListener(eventListener);
//        mSimpleExoPlayer.setTextOutput(mOutput);
        mSimpleExoPlayer.setPlayWhenReady(true);

    }


    TextRenderer.Output mOutput = new TextRenderer.Output() {
        @Override
        public void onCues(List<Cue> cues) {
            LogUtil.i(this, "MainActivity.onCues.");
        }
    };

    private SimpleExoPlayer.VideoListener mVideoListener = new SimpleExoPlayer.VideoListener() {
        @Override
        public void onVideoSizeChanged(int width, int height, int unappliedRotationDegrees, float pixelWidthHeightRatio) {
            LogUtil.i(this, "MainActivity.onVideoSizeChanged.width:" + width + ", height:" + height);

        }

        @Override
        public void onRenderedFirstFrame() {
            LogUtil.i(this, "MainActivity.onRenderedFirstFrame.");
        }
    };


    private ExoPlayer.EventListener eventListener = new ExoPlayer.EventListener() {

        @Override
        public void onTracksChanged(TrackGroupArray trackGroups, TrackSelectionArray trackSelections) {
            LogUtil.i(this, "onTracksChanged");
        }

        @Override
        public void onLoadingChanged(boolean isLoading) {
            LogUtil.i(this, "onLoadingChanged");
        }

        @Override
        public void onPlayerStateChanged(boolean playWhenReady, int playbackState) {
            LogUtil.i(this, "onPlayerStateChanged: playWhenReady = " + String.valueOf(playWhenReady)
                    + " playbackState = " + playbackState);
            switch (playbackState) {
                case ExoPlayer.STATE_ENDED:
                    LogUtil.i(this, "Playback ended!");
                    //Stop playback and return to start position
                    setPlayPause(false);
                    mSimpleExoPlayer.seekTo(0);
                    break;
                case ExoPlayer.STATE_READY:
                    mProgressBar.setVisibility(View.GONE);
                    LogUtil.i(this, "ExoPlayer ready! pos: " + mSimpleExoPlayer.getCurrentPosition()
                            + " max: " + stringForTime((int) mSimpleExoPlayer.getDuration()));
                    setProgress(0);
                    break;
                case ExoPlayer.STATE_BUFFERING:
                    LogUtil.i(this, "Playback buffering!");
                    mProgressBar.setVisibility(View.VISIBLE);
                    break;
                case ExoPlayer.STATE_IDLE:
                    LogUtil.i(this, "ExoPlayer idle!");
                    break;
            }
        }

        @Override
        public void onPlayerError(ExoPlaybackException error) {
            LogUtil.i(this, "onPlaybackError: " + error.getMessage());
        }


        @Override
        public void onPlaybackParametersChanged(PlaybackParameters playbackParameters) {
            LogUtil.i(this, "MainActivity.onPlaybackParametersChanged." + playbackParameters.toString());
        }
    };

    /**
     * Starts or stops playback. Also takes care of the Play/Pause button toggling
     *
     * @param play True if playback should be started
     */
    private void setPlayPause(boolean play) {
        mSimpleExoPlayer.setPlayWhenReady(play);
    }

    private String stringForTime(int timeMs) {
        StringBuilder mFormatBuilder;
        Formatter mFormatter;
        mFormatBuilder = new StringBuilder();
        mFormatter = new Formatter(mFormatBuilder, Locale.getDefault());
        int totalSeconds = timeMs / 1000;

        int seconds = totalSeconds % 60;
        int minutes = (totalSeconds / 60) % 60;
        int hours = totalSeconds / 3600;

        mFormatBuilder.setLength(0);
        if (hours > 0) {
            return mFormatter.format("%d:%02d:%02d", hours, minutes, seconds).toString();
        } else {
            return mFormatter.format("%02d:%02d", minutes, seconds).toString();
        }
    }


    @Override
    protected void onPause() {
        LogUtil.i(this, "MainActivity.onPause.");
        super.onPause();
        mSimpleExoPlayer.setPlayWhenReady(false);
    }

    @Override
    protected void onStop() {
        LogUtil.i(this, "MainActivity.onStop.");
        super.onStop();
    }

    @Override
    protected void onResume() {
        LogUtil.i(this, "MainActivity.onResume.");
        super.onResume();
        mSimpleExoPlayer.setPlayWhenReady(true);
    }

    @Override
    protected void onStart() {
        LogUtil.i(this, "MainActivity.onStart.");
        super.onStart();
    }

    @Override
    public void onSuccess(String fileLink) {
        mFileLink = fileLink;
        playVideo(fileLink);
    }

    @Override
    public void onError(String errMsg) {

    }

    @Override
    public void onPointerCaptureChanged(boolean hasCapture) {

    }
}








