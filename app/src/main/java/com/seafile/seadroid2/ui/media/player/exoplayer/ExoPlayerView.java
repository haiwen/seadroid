package com.seafile.seadroid2.ui.media.player.exoplayer;

import static androidx.media3.common.Player.COMMAND_SET_VIDEO_SURFACE;

import android.content.Context;
import android.os.Looper;
import android.os.Message;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.SurfaceView;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.VideoSize;
import androidx.media3.common.util.Assertions;

import com.seafile.seadroid2.R;

public class ExoPlayerView extends ConstraintLayout {

    private SurfaceView surfaceView;
    private View binding;
    private FrameLayout containerLayout;

    private final ComponentListener componentListener = new ComponentListener();
    private Player player;
    private boolean playerState;
    private ProgressStateChangeListener progressChangeListener;
    private PlaybackStateChangedListener playbackStateChanged;
    private PlayerErrorBackListener playerErrorBackListener;

    private final android.os.Handler progressHandler = new android.os.Handler(Looper.myLooper()) {
        @Override
        public void handleMessage(@NonNull Message msg) {
            super.handleMessage(msg);

            Player currentPlayer = player;
            if (currentPlayer != null && currentPlayer.getPlaybackState() == Player.STATE_READY && currentPlayer.isPlaying()) {
                long curPosition = currentPlayer.getContentPosition();
                long bufferedPosition = currentPlayer.getBufferedPosition();
                long duration = currentPlayer.getDuration();

                if (progressChangeListener != null) {
                    progressChangeListener.onProgressChanged(curPosition, bufferedPosition, duration);
                }

                if (curPosition >= duration) {
                    if (playbackStateChanged != null) {
                        playbackStateChanged.onCall(Player.STATE_ENDED);
                    }
                }

                //0.5s
                sendEmptyMessageDelayed(1, 250);
            }
        }
    };


    public ExoPlayerView(@NonNull Context context) {
        super(context);

        init();
    }

    public ExoPlayerView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);

        init();
    }

    public ExoPlayerView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        init();
    }


    private void init() {
        surfaceView = new SurfaceView(getContext());
        ViewGroup.LayoutParams vlp = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        surfaceView.setLayoutParams(vlp);

        binding = LayoutInflater.from(getContext()).inflate(R.layout.layout_exo_player_view, this, true);
        containerLayout = binding.findViewById(R.id.exo_content_frame);
        containerLayout.addView(surfaceView, 0);

        setVisibility(View.VISIBLE);
    }

    public void setPlayer(Player player) {
        Assertions.checkState(Looper.myLooper() == Looper.getMainLooper());
        Assertions.checkArgument(player.getApplicationLooper() == Looper.getMainLooper());

        if (this.player == player) {
            return;
        }

        if (this.player != null) {
            this.player.removeListener(componentListener);
            this.player.clearVideoSurfaceView(surfaceView);
        }

        this.player = player;

        if (this.player.isCommandAvailable(COMMAND_SET_VIDEO_SURFACE)) {
            this.player.setVideoSurfaceView(surfaceView);
            updateAspectRatio();
        }

        this.player.addListener(componentListener);
        updateErrorMessage();
    }

    private void updateAspectRatio() {
        if (player != null) {
            VideoSize videoSize = player.getVideoSize();
            if (videoSize.width > 0 && videoSize.height > 0) {
                ConstraintLayout.LayoutParams clp = (LayoutParams) containerLayout.getLayoutParams();
                int w = videoSize.width;
                int h = videoSize.height;
                clp.dimensionRatio = w + ":" + h;
                containerLayout.setLayoutParams(clp);
            }
        }
    }

    @Override
    public void setVisibility(int visibility) {
        super.setVisibility(visibility);
        surfaceView.setVisibility(visibility);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (player != null) {
            try {
                player.setVideoSurfaceView(null);
                player.stop();
                player.release();
                player.removeListener(componentListener);
                progressHandler.removeCallbacksAndMessages(null);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    protected void onWindowVisibilityChanged(int visibility) {
        super.onWindowVisibilityChanged(visibility);
        if (visibility == View.VISIBLE) {
            if (playerState) {
                if (player != null) {
                    player.play();
                }
            }
        } else {
            if (player != null) {
                playerState = player.isPlaying();
                if (player.isPlaying()) {
                    player.pause();
                }
            } else {
                playerState = false;
            }
        }
    }

    private void updateProgressState() {
        if (player != null) {
            progressHandler.removeCallbacksAndMessages(null);
            if (player.getPlaybackState() == Player.STATE_READY && player.isPlaying()) {
                progressHandler.sendEmptyMessage(1);
            }
        }
    }

    private void updateErrorMessage() {
        if (player != null) {
            PlaybackException exception = player.getPlayerError();
            if (exception != null && playerErrorBackListener != null) {
                playerErrorBackListener.onError(exception);
            }
        }
    }


    private class ComponentListener implements Player.Listener {
        @Override
        public void onVideoSizeChanged(VideoSize videoSize) {
            updateAspectRatio();
        }

        @Override
        public void onPlayerError(PlaybackException error) {
            updateErrorMessage();
        }

        @Override
        public void onPlaybackStateChanged(int playbackState) {

            if (playbackState == Player.STATE_READY && getVisibility() != View.VISIBLE) {
                setVisibility(View.VISIBLE);
            }

            updateProgressState();
            if (playbackStateChanged != null) {
                playbackStateChanged.onCall(playbackState);
            }
        }

        @Override
        public void onPlayWhenReadyChanged(boolean playWhenReady, int reason) {
            //播放暂停会回调该方法，播放时playWhenReady为true
            updateProgressState();
        }
    }

    public interface PlayerErrorBackListener {
        void onError(PlaybackException exception);
    }

    public void setPlayerErrorBackListener(PlayerErrorBackListener playerErrorBackListener) {
        this.playerErrorBackListener = playerErrorBackListener;
    }

    public interface PlaybackStateChangedListener {
        void onCall(@Player.State int state);
    }

    public void setPlaybackStateChanged(PlaybackStateChangedListener playbackStateChanged) {
        this.playbackStateChanged = playbackStateChanged;
    }

    public interface ProgressStateChangeListener {
        void onProgressChanged(long currentPosition, long bufferedPosition, long duration);
    }

    public void setProgressChangeListener(ProgressStateChangeListener listener) {
        progressChangeListener = listener;
    }

}
