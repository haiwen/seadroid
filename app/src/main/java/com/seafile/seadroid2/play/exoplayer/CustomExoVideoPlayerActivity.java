package com.seafile.seadroid2.play.exoplayer;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.appcompat.widget.AppCompatImageView;
import androidx.media3.common.AudioAttributes;
import androidx.media3.common.C;
import androidx.media3.common.MediaItem;
import androidx.media3.common.Player;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.ui.DefaultTimeBar;
import androidx.media3.ui.TimeBar;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.play.VideoLinkStateListener;
import com.seafile.seadroid2.play.VideoLinkTask;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.util.Locale;

public class CustomExoVideoPlayerActivity extends BaseActivity implements VideoLinkStateListener {
    private static final String KEY_FULL_SCREEN = "isFullScreen";
    private static final String KEY_ITEM_INDEX = "startItemIndex";
    private static final String KEY_POSITION = "position";
    private static final String KEY_AUTO_PLAY = "auto_play";

    private ExoPlayer player;
    private ExoPlayerView playerView;

    private TextView playerPosition, playerDuration;
    private DefaultTimeBar timeBar;
    private AppCompatImageView playerPlay;
    private ImageView playerFullscreen;
    private ImageView backView;
    private ProgressBar loadingProgressBar;
    private LinearLayout progressContainer;
    private LinearLayout backContainer;
    private FrameLayout playContainer;

    private Account mAccount;
    private String fileName;
    private String mRepoID;
    private String mFilePath;
    private String mFileLink;

    private boolean hasFullScreen = false;
    private boolean startAutoPlay;
    private int startItemIndex;
    private long startPosition;

    public static void startThis(Context context, String fileName, String repoID, String filePath, Account account) {
        Intent intent = new Intent(context, CustomExoVideoPlayerActivity.class);
        intent.putExtra("fileName", fileName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
        context.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Utils.hideSystemNavigationBar(this);
        setContentView(R.layout.activity_video_player2);

        Intent intent = getIntent();
        mAccount = intent.getParcelableExtra("account");
        fileName = intent.getStringExtra("fileName");
        mRepoID = intent.getStringExtra("repoID");
        mFilePath = intent.getStringExtra("filePath");
        VideoLinkTask task = new VideoLinkTask(mAccount, mRepoID, mFilePath, this);
        ConcurrentAsyncTask.execute(task);

        initUI();

        if (savedInstanceState != null) {
            startAutoPlay = savedInstanceState.getBoolean(KEY_AUTO_PLAY);
            hasFullScreen = savedInstanceState.getBoolean(KEY_FULL_SCREEN);
            startPosition = savedInstanceState.getLong(KEY_POSITION);
            startItemIndex = savedInstanceState.getInt(KEY_ITEM_INDEX);
        } else {
            clearStartPosition();
        }
    }

    private void initUI() {
        backView = findViewById(R.id.back);
        backContainer = findViewById(R.id.back_container);
        progressContainer = findViewById(R.id.progress_container);
        loadingProgressBar = findViewById(R.id.loading_progress);
        playerView = findViewById(R.id.player_view);
        playerPlay = findViewById(R.id.player_play);
        playerPosition = findViewById(R.id.progress_position);
        playerDuration = findViewById(R.id.progress_duration);
        timeBar = findViewById(R.id.progress);
        playerFullscreen = findViewById(R.id.fullscreen);
        playContainer = findViewById(R.id.play_container);

        playContainer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showControllerView();
            }
        });

        backView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (hasFullScreen) {
                    setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
                    hasFullScreen = false;
                    return;
                }

                finish();
            }
        });

        playerFullscreen.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (hasFullScreen) {
                    playerFullscreen.setImageResource(R.drawable.ic_fullscreen_enter);
                    setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
                    hasFullScreen = false;
                } else {
                    playerFullscreen.setImageResource(R.drawable.ic_fullscreen_exit);
                    setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
                    hasFullScreen = true;
                }
            }
        });
        timeBar.addListener(new TimeBar.OnScrubListener() {
            @Override
            public void onScrubStart(TimeBar timeBar, long position) {

            }

            @Override
            public void onScrubMove(TimeBar timeBar, long position) {

            }

            @Override
            public void onScrubStop(TimeBar timeBar, long position, boolean canceled) {
                if (player != null) {
                    player.seekTo(position);
                }
            }
        });

        playerPlay.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (player == null) {
                    return;
                }

                showControllerView();

                //end or pause or play
                if (player.getPlaybackState() == Player.STATE_ENDED) {
                    player.seekTo(0);
                    player.setPlayWhenReady(true);
                    setPlayIcon(false);
                } else if (player.isPlaying()) {
                    setPlayIcon(true);
                    player.pause();
                } else {
                    setPlayIcon(false);
                    player.play();
                }
            }
        });

        playContainer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (progressContainer.getVisibility() == View.VISIBLE) {
                    hideControllerViewImmediately();
                } else {
                    showControllerView();
                }
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
    }

    @Override
    public void onSuccess(String fileLink) {
        mFileLink = fileLink;
        init();
    }

    private void setPlayIcon(boolean isPlay) {
        playerPlay.setImageResource(isPlay ? R.drawable.ic_play_fill : R.drawable.ic_pause_fill);
    }

    private void init() {
        player = new ExoPlayer.Builder(this).build();
        playerView.setPlaybackStateChanged(new ExoPlayerView.PlaybackStateChangedListener() {
            @Override
            public void onCall(int playbackState) {
                switch (playbackState) {
                    case Player.STATE_BUFFERING: //loading
                        loadingProgressBar.setVisibility(View.VISIBLE);
                        playerPlay.setVisibility(View.GONE);
                        break;
                    case Player.STATE_READY:
                        loadingProgressBar.setVisibility(View.GONE);
                        playerPlay.setVisibility(View.VISIBLE);
                        break;
                    case Player.STATE_ENDED:

                        setPlayIcon(/* isPlay */ true);
                        showControllerView();
                        break;
                }
            }
        });

        playerView.setPlayer(player);
        playerView.setProgressChangeListener(new ExoPlayerView.ProgressStateChangeListener() {
            @Override
            public void onProgressChanged(long currentPosition, long bufferedPosition, long duration) {
                playerPosition.setText(getMinSecFormat(currentPosition));
                playerDuration.setText(getMinSecFormat(duration));
                timeBar.setDuration(duration);
                timeBar.setPosition(currentPosition);

                //
                hideControllerView();
            }
        });

        player.setAudioAttributes(AudioAttributes.DEFAULT, true);
        player.setPlayWhenReady(startAutoPlay);
        boolean haveStartPosition = startItemIndex != C.INDEX_UNSET;
        if (haveStartPosition) {
            player.seekTo(startItemIndex, startPosition);
        }
        player.setMediaSource(getMediaSource(mFileLink), !haveStartPosition);
        player.prepare();
    }

    private int countdown = 12;

    private void hideControllerView() {
        countdown--;
        if (countdown <= 0) {
            countdown = 0;
        }

        if (countdown == 0 && progressContainer.getVisibility() == View.VISIBLE) {
            progressContainer.setVisibility(View.GONE);
            backContainer.setVisibility(View.GONE);
            playerPlay.setVisibility(View.GONE);
        }
    }

    private void hideControllerViewImmediately() {
        countdown = 0;

        playerPlay.setVisibility(View.GONE);
        progressContainer.setVisibility(View.GONE);
        backContainer.setVisibility(View.GONE);
    }


    private void showControllerView() {
        countdown = 12;
        progressContainer.setVisibility(View.VISIBLE);
        backContainer.setVisibility(View.VISIBLE);
        playerPlay.setVisibility(View.VISIBLE);
    }

    private void updateStartPosition() {
        if (player != null) {
            startAutoPlay = player.getPlayWhenReady();
            startItemIndex = player.getCurrentMediaItemIndex();
            startPosition = Math.max(0, player.getContentPosition());
        }
    }

    protected void clearStartPosition() {
        startAutoPlay = true;
        startItemIndex = C.INDEX_UNSET;
        startPosition = C.TIME_UNSET;
    }


    @Override
    public void onError(String errMsg) {
        ToastUtils.showLong(errMsg);
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (player != null) {
            player.pause();
            playerPlay.setAlpha(0.8F);
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
        releasePlayer();
        super.onDestroy();
    }

    protected void releasePlayer() {
        if (player != null) {
            updateStartPosition();
            player.release();
            player = null;
        }
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

    private String getMinSecFormat(long millisecond) {
        long minutes = millisecond / 60 / 1000;
        long seconds = millisecond / 1000 % 60;
        return String.format(Locale.ROOT, "%02d:%02d", minutes, seconds);
    }
}
