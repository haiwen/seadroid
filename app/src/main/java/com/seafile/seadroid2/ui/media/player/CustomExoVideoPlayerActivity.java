package com.seafile.seadroid2.ui.media.player;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.Nullable;
import androidx.annotation.OptIn;
import androidx.lifecycle.Observer;
import androidx.media3.common.AudioAttributes;
import androidx.media3.common.C;
import androidx.media3.common.MediaItem;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.ui.TimeBar;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.BarUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.databinding.ActivityExoPlayerBinding;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.view.ExoPlayerView;

import java.util.Locale;

@UnstableApi
public class CustomExoVideoPlayerActivity extends BaseActivityWithVM<PlayerViewModel> {
    private static final String KEY_FULL_SCREEN = "isFullScreen";
    private static final String KEY_ITEM_INDEX = "startItemIndex";
    private static final String KEY_POSITION = "position";
    private static final String KEY_AUTO_PLAY = "auto_play";

    private ActivityExoPlayerBinding binding;

    private ExoPlayer exoPlayer;
    private ExoPlayerView exoPlayerView;

    private String fileName;
    private String fileId;
    private String repoId;
    private String filePath;

    private boolean hasFullScreen = false;
    private boolean startAutoPlay;
    private int startItemIndex;
    private long startPosition;

    public static void startThis(Context context, String fileName, String repoID, String filePath, String fileId) {
        Intent intent = new Intent(context, CustomExoVideoPlayerActivity.class);
        intent.putExtra("fileName", fileName);
        intent.putExtra("repoId", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("fileId", fileId);
        context.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityExoPlayerBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        //
        BarUtils.setNavBarVisibility(this, false);
        BarUtils.setStatusBarVisibility(this, false);

        Intent intent = getIntent();
        if (!intent.hasExtra("repoId")) {
            throw new IllegalArgumentException("no repoId param");
        }

        if (!intent.hasExtra("filePath")) {
            throw new IllegalArgumentException("no filePath param");
        }


        if (savedInstanceState != null) {
            fileName = savedInstanceState.getString("fileName");
            repoId = savedInstanceState.getString("repoId");
            filePath = savedInstanceState.getString("filePath");
            fileId = savedInstanceState.getString("fileId");

            startAutoPlay = savedInstanceState.getBoolean(KEY_AUTO_PLAY);
            hasFullScreen = savedInstanceState.getBoolean(KEY_FULL_SCREEN);
            startPosition = savedInstanceState.getLong(KEY_POSITION);
            startItemIndex = savedInstanceState.getInt(KEY_ITEM_INDEX);
        } else {

            fileName = intent.getStringExtra("fileName");
            repoId = intent.getStringExtra("repoId");
            filePath = intent.getStringExtra("filePath");
            fileId = intent.getStringExtra("fileId");


            clearStartPosition();
        }

        initUI();

        initViewModel();
    }

    @Override
    protected void onPostCreate(@Nullable Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);

        getViewModel().checkLocalAndOpen(repoId, filePath, fileId, true);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void initUI() {

        exoPlayerView = binding.playerView;

        binding.back.setOnClickListener(v -> {
            if (hasFullScreen) {
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
                hasFullScreen = false;
                return;
            }

            finish();
        });

        binding.fullscreen.setOnClickListener(v -> {
            if (hasFullScreen) {
                binding.fullscreen.setImageResource(R.drawable.ic_fullscreen_enter);
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
                hasFullScreen = false;
            } else {
                binding.fullscreen.setImageResource(R.drawable.ic_fullscreen_exit);
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
                hasFullScreen = true;
            }
        });

        binding.progressBar.addListener(new TimeBar.OnScrubListener() {
            @Override
            public void onScrubStart(TimeBar timeBar, long position) {

            }

            @Override
            public void onScrubMove(TimeBar timeBar, long position) {

            }

            @Override
            public void onScrubStop(TimeBar timeBar, long position, boolean canceled) {
                if (exoPlayer != null) {
                    exoPlayer.seekTo(position);
                }
            }
        });

        binding.playPause.setOnClickListener(v -> {
            if (exoPlayer == null) {
                return;
            }

            showControllerView();

            //end or pause or play
            if (exoPlayer.getPlaybackState() == Player.STATE_ENDED) {
                exoPlayer.seekTo(0);
                exoPlayer.setPlayWhenReady(true);
                setPlayIcon(false);
            } else if (exoPlayer.isPlaying()) {
                setPlayIcon(true);
                exoPlayer.pause();
            } else {
                setPlayIcon(false);
                exoPlayer.play();
            }
        });

        binding.playContainer.setOnClickListener(v -> {
            if (binding.progressContainer.getVisibility() == View.VISIBLE) {
                hideControllerViewImmediately();
            } else {
                showControllerView();
            }
        });
    }

    private void initViewModel() {
        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                Toasts.show(e.getMessage());
                resetAllView();
            }
        });

        getViewModel().getUrlLiveData().observe(this, new Observer<String>() {
            @Override
            public void onChanged(String url) {
                prepareForPlay(url);
            }
        });
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        updateStartPosition();

        outState.putLong(KEY_POSITION, startPosition);
        outState.putBoolean(KEY_AUTO_PLAY, startAutoPlay);
        outState.putBoolean(KEY_FULL_SCREEN, hasFullScreen);
        outState.putInt(KEY_ITEM_INDEX, startItemIndex);

        outState.putString("fileName", fileName);
        outState.putString("repoId", repoId);
        outState.putString("filePath", filePath);
        outState.putString("fileId", fileId);
    }

    private void setPlayIcon(boolean isPlay) {
        binding.playPause.setImageResource(isPlay ? R.drawable.ic_play_fill : R.drawable.ic_pause_fill);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void prepareForPlay(String url) {
        exoPlayer = new ExoPlayer.Builder(this).build();
        exoPlayerView.setPlaybackStateChanged(playbackState -> {
            switch (playbackState) {
                case Player.STATE_BUFFERING: //loading
                    binding.playLoading.setVisibility(View.VISIBLE);
                    binding.playPause.setVisibility(View.GONE);
                    break;
                case Player.STATE_READY:
                    binding.playLoading.setVisibility(View.GONE);
                    binding.playPause.setVisibility(View.VISIBLE);
                    break;
                case Player.STATE_ENDED:
                    setPlayIcon(/* isPlay */ true);
                    showControllerView();
                    break;
            }
        });

        exoPlayerView.setPlayer(exoPlayer);
        exoPlayerView.setProgressChangeListener(new ExoPlayerView.ProgressStateChangeListener() {
            @Override
            public void onProgressChanged(long currentPosition, long bufferedPosition, long duration) {
                binding.progressPosition.setText(getMinSecFormat(currentPosition));
                binding.progressDuration.setText(getMinSecFormat(duration));
                binding.progressBar.setDuration(duration);
                binding.progressBar.setPosition(currentPosition);

                //
                hideControllerView();
            }
        });

        exoPlayer.setAudioAttributes(AudioAttributes.DEFAULT, true);
        exoPlayer.setPlayWhenReady(startAutoPlay);
        boolean haveStartPosition = startItemIndex != C.INDEX_UNSET;
        if (haveStartPosition) {
            exoPlayer.seekTo(startItemIndex, startPosition);
        }
        exoPlayer.setMediaSource(getMediaSource(url), !haveStartPosition);
        exoPlayer.prepare();
    }

    private int countdown = 12;

    private void hideControllerView() {
        countdown--;
        if (countdown <= 0) {
            countdown = 0;
        }

        if (countdown == 0 && binding.progressContainer.getVisibility() == View.VISIBLE) {
            binding.progressContainer.setVisibility(View.GONE);
            binding.back.setVisibility(View.GONE);
            binding.playPause.setVisibility(View.GONE);
        }
    }

    private void resetAllView() {
        binding.playPause.setVisibility(View.GONE);
        binding.playLoading.setVisibility(View.GONE);
        binding.progressPosition.setText("00:00");
        binding.progressDuration.setText("00:00");
        binding.progressBar.setPosition(0);
    }

    private void hideControllerViewImmediately() {
        countdown = 0;

        binding.playPause.setVisibility(View.GONE);
        binding.progressContainer.setVisibility(View.GONE);
        binding.back.setVisibility(View.GONE);
    }

    private void showControllerView() {
        countdown = 12;
        binding.playPause.setVisibility(View.VISIBLE);
        binding.progressContainer.setVisibility(View.VISIBLE);
        binding.back.setVisibility(View.VISIBLE);
    }

    private void updateStartPosition() {
        if (exoPlayer != null) {
            startAutoPlay = exoPlayer.getPlayWhenReady();
            startItemIndex = exoPlayer.getCurrentMediaItemIndex();
            startPosition = Math.max(0, exoPlayer.getContentPosition());
        }
    }

    protected void clearStartPosition() {
        startAutoPlay = true;
        startItemIndex = C.INDEX_UNSET;
        startPosition = C.TIME_UNSET;
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (exoPlayer != null) {
            exoPlayer.pause();
            binding.playPause.setAlpha(0.8F);
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (exoPlayer != null) {
            exoPlayer.stop();
        }
    }

    @Override
    protected void onDestroy() {
        releasePlayer();
        super.onDestroy();
    }

    protected void releasePlayer() {
        if (exoPlayer != null) {
            updateStartPosition();
            exoPlayer.release();
            exoPlayer = null;
        }
    }

    @OptIn(markerClass = UnstableApi.class)
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

    private String getMinSecFormat(long millisecond) {
        long minutes = millisecond / 60 / 1000;
        long seconds = millisecond / 1000 % 60;
        return String.format(Locale.ROOT, "%02d:%02d", minutes, seconds);
    }
}
