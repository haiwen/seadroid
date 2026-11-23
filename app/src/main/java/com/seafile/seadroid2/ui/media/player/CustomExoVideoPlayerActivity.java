package com.seafile.seadroid2.ui.media.player;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.OrientationEventListener;
import android.view.View;

import androidx.annotation.NonNull;
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
import androidx.media3.datasource.okhttp.OkHttpDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.ui.TimeBar;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.BarUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.ActivityExoPlayerBinding;
import com.seafile.seadroid2.framework.http.UnsafeOkHttpClient;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.view.ExoPlayerView;

import java.util.Locale;

import okhttp3.OkHttpClient;

@UnstableApi
public class CustomExoVideoPlayerActivity extends BaseActivityWithVM<PlayerViewModel> {
    private static final String KEY_USER_ORIENTATION = "userRequestedOrientation";
    private static final String KEY_ITEM_INDEX = "startItemIndex";
    private static final String KEY_POSITION = "position";
    private static final String KEY_AUTO_PLAY = "auto_play";

    private ActivityExoPlayerBinding binding;

    private ExoPlayer exoPlayer;
    private ExoPlayerView exoPlayerView;

    private String fileName;
    private String repoId;
    private String filePath;

    private int userRequestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED;
    private OrientationEventListener orientationEventListener;
    private boolean startAutoPlay;
    private int startItemIndex;
    private long startPosition;

    public static void startThis(Context context, String fileName, String repoID, String filePath) {
        Intent intent = new Intent(context, CustomExoVideoPlayerActivity.class);
        intent.putExtra("fileName", fileName);
        intent.putExtra("repoId", repoID);
        intent.putExtra("filePath", filePath);
        context.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityExoPlayerBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

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

            startAutoPlay = savedInstanceState.getBoolean(KEY_AUTO_PLAY);
            userRequestedOrientation = savedInstanceState.getInt(KEY_USER_ORIENTATION, ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);
            startPosition = savedInstanceState.getLong(KEY_POSITION);
            startItemIndex = savedInstanceState.getInt(KEY_ITEM_INDEX);

            if (savedInstanceState.containsKey("playSpeed")) {
                float speed = savedInstanceState.getFloat("playSpeed");
                if (exoPlayer != null && speed != 0) {
                    exoPlayer.setPlaybackSpeed(speed);
                }
            }
        } else {
            fileName = intent.getStringExtra("fileName");
            repoId = intent.getStringExtra("repoId");
            filePath = intent.getStringExtra("filePath");
            binding.videoSpeed.setText("1x");

            clearStartPosition();
        }

        if (userRequestedOrientation != ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED) {
            setRequestedOrientation(userRequestedOrientation);
        } else {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_FULL_USER);
        }

        initOrientationListener();
        initUI();

        initViewModel();
    }

    private void initOrientationListener() {
        orientationEventListener = new OrientationEventListener(this) {
            @Override
            public void onOrientationChanged(int orientation) {
                if (orientation == ORIENTATION_UNKNOWN) {
                    return;
                }

                // When system auto-rotation is disabled, the player must not rotate with the device
                if (!isSystemAutoRotationEnabled()) {
                    return;
                }

                // When system auto-rotation is enabled: if the user previously manually specified an orientation via the fullscreen button,
                // reset to auto-rotation once the device is physically rotated to the opposite orientation
                if (userRequestedOrientation != ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED) {
                    boolean isDeviceLandscape = (orientation >= 60 && orientation <= 120) || (orientation >= 240 && orientation <= 300);
                    boolean isDevicePortrait = (orientation >= 330 || orientation <= 30) || (orientation >= 150 && orientation <= 210);

                    if (userRequestedOrientation == ActivityInfo.SCREEN_ORIENTATION_PORTRAIT && isDeviceLandscape) {
                        userRequestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED;
                        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_FULL_USER);
                    } else if (userRequestedOrientation == ActivityInfo.SCREEN_ORIENTATION_SENSOR_LANDSCAPE && isDevicePortrait) {
                        userRequestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED;
                        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_FULL_USER);
                    }
                }
            }
        };
    }

    private boolean isSystemAutoRotationEnabled() {
        try {
            return android.provider.Settings.System.getInt(
                    getContentResolver(),
                    android.provider.Settings.System.ACCELEROMETER_ROTATION,
                    0
            ) == 1;
        } catch (Exception e) {
            return false;
        }
    }

    @Override
    protected void onPostCreate(@Nullable Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);

        getViewModel().checkLocalAndOpen(repoId, filePath, true);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void initUI() {

        exoPlayerView = binding.playerView;

        // Close current page directly on back button click
        binding.back.setOnClickListener(v -> finish());

        // Toggle between landscape and portrait on fullscreen button click
        binding.fullscreen.setOnClickListener(v -> toggleFullscreen());
        updateFullscreenIcon(getResources().getConfiguration().orientation);

        binding.videoSpeed.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                changeVideoSpeed();
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
        outState.putInt(KEY_USER_ORIENTATION, userRequestedOrientation);
        outState.putInt(KEY_ITEM_INDEX, startItemIndex);

        outState.putString("fileName", fileName);
        outState.putString("repoId", repoId);
        outState.putString("filePath", filePath);

        if (exoPlayer != null) {
            float s = exoPlayer.getPlaybackParameters().speed;
            outState.putFloat("playSpeed", s);
        }
    }

    private void setPlayIcon(boolean isPlay) {
        binding.playPause.setImageResource(isPlay ? R.drawable.ic_play_fill : R.drawable.ic_pause_fill);
    }

    /**
     * Toggle fullscreen / non-fullscreen mode:
     * When currently in landscape (fullscreen), click to switch to portrait (non-fullscreen);
     * When currently in portrait (non-fullscreen), click to switch to landscape (fullscreen, SENSOR_LANDSCAPE).
     */
    private void toggleFullscreen() {
        int currentOrientation = getResources().getConfiguration().orientation;
        if (currentOrientation == Configuration.ORIENTATION_LANDSCAPE) {
            // Current orientation is landscape -> switch to portrait
            userRequestedOrientation = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
            updateFullscreenIcon(Configuration.ORIENTATION_PORTRAIT);
        } else {
            // Current orientation is portrait -> switch to landscape (SENSOR_LANDSCAPE)
            userRequestedOrientation = ActivityInfo.SCREEN_ORIENTATION_SENSOR_LANDSCAPE;
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR_LANDSCAPE);
            updateFullscreenIcon(Configuration.ORIENTATION_LANDSCAPE);
        }
    }

    /**
     * Sync fullscreen / exit fullscreen icon:
     * Landscape displays ic_fullscreen_exit, portrait displays ic_fullscreen_enter
     */
    private void updateFullscreenIcon(int orientation) {
        if (orientation == Configuration.ORIENTATION_LANDSCAPE) {
            binding.fullscreen.setImageResource(R.drawable.ic_fullscreen_exit);
        } else {
            binding.fullscreen.setImageResource(R.drawable.ic_fullscreen_enter);
        }
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        updateFullscreenIcon(newConfig.orientation);
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

    /**
     * Must be higher than 0. 1 is normal speed, 2 is twice as fast, 0.5 is half normal speed.
     */
    private void changeVideoSpeed() {
        float speed = exoPlayer.getPlaybackParameters().speed;
        float s = getSpecialVideoSpeed(speed);
        if (exoPlayer != null) {
            countdown = 12;
            exoPlayer.setPlaybackSpeed(s);
            binding.videoSpeed.setText(s + "x");
        }
    }

    /**
     * 0.5x -> 1x -> 1.5x -> 2x  -> 3x -> 0.5x
     */
    private float getSpecialVideoSpeed(float speed) {
        if (speed == 3f) return 0.5f;
        if (speed == 2f) return 3f;
        if (speed == 1.5f) return 2f;
        if (speed == 1f) return 1.5f;
        if (speed == 0.5f) return 1f;
        return speed;
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
    protected void onStart() {
        super.onStart();
        if (orientationEventListener != null && orientationEventListener.canDetectOrientation()) {
            orientationEventListener.enable();
        }
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
        if (orientationEventListener != null) {
            orientationEventListener.disable();
        }
        if (exoPlayer != null) {
            exoPlayer.stop();
        }
    }

    @Override
    public void onDestroy() {
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

        //new: use OkHttp
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        OkHttpClient okHttpClient = new UnsafeOkHttpClient(account).getOkClient();

        OkHttpDataSource.Factory okHttpFactory = new OkHttpDataSource.Factory(okHttpClient)
                .setUserAgent(userAgent);

        DataSource.Factory dataSourceFactory = new DefaultDataSource.Factory(
                this,
                okHttpFactory
        );

        return new ProgressiveMediaSource.Factory(dataSourceFactory)
                .createMediaSource(MediaItem.fromUri(url));
    }

    private String getMinSecFormat(long millisecond) {
        long minutes = millisecond / 60 / 1000;
        long seconds = millisecond / 1000 % 60;
        return String.format(Locale.getDefault(), "%02d:%02d", minutes, seconds);
    }
}
