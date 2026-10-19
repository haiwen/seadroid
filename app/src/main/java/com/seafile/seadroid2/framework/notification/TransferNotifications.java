package com.seafile.seadroid2.framework.notification;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.Looper;

import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.ui.transfer_list.TransferActivity;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * Direct, single-point transfer notification manager.
 * No intermediate layers, no background executor queues, no complex DTOs.
 */
public final class TransferNotifications {
    public enum StepState {PREPARING, SCANNING, TRANSFERRING, RESULT}

    public enum ResultState {SUCCESS, CANCELLED, FAILED}

    private static final class State {
        final NotificationKey key;
        final Map<String, ResultState> results = new HashMap<>();
        StepState stepState = StepState.PREPARING;
        ResultState result = ResultState.SUCCESS;
        String fileName = "";
        int percent = -1;
        long sequence = -1;
        long lastPublishTime;
        long sessionVersion;

        State(NotificationKey key) {
            this.key = key;
        }

        int succeeded() {
            int count = 0;
            for (ResultState o : results.values()) if (o == ResultState.SUCCESS) count++;
            return count;
        }

        int failed() {
            int count = 0;
            for (ResultState o : results.values()) if (o == ResultState.FAILED) count++;
            return count;
        }
    }

    private static final TransferNotifications INSTANCE = new TransferNotifications();

    public static TransferNotifications get() {
        return INSTANCE;
    }

    private Context context;
    private NotificationManager manager;
    private Handler mainHandler;
    private final Map<NotificationKey, State> states = new EnumMap<>(NotificationKey.class);

    private TransferNotifications() {
    }

    public synchronized void init(Context context) {
        if (this.context == null) {
            this.context = context.getApplicationContext();
            this.manager = this.context.getSystemService(NotificationManager.class);
            this.mainHandler = new Handler(Looper.getMainLooper());
            //
            NotificationUtils.initNotificationChannels(this.context);
        }
    }

    public synchronized void begin(NotificationKey key) {
        State state = new State(key);
        state.sessionVersion = System.currentTimeMillis();
        states.put(key, state);
    }

    public synchronized void scanning(NotificationKey key) {
        State state = states.get(key);
        if (state == null) return;
        state.stepState = StepState.SCANNING;
        state.percent = -1;
        state.fileName = "";
        publishNow(state);
    }

    public synchronized void prepared(NotificationKey key) {
        State state = states.get(key);
        if (state == null) return;
        state.stepState = StepState.PREPARING;
        state.percent = -1;
        publishNow(state);
    }

    public synchronized void startFile(NotificationKey key, String fileName) {
        State state = states.get(key);
        if (state == null) return;
        state.fileName = fileName == null ? "" : fileName;
        state.stepState = StepState.TRANSFERRING;
        state.percent = 0;
        state.sequence = -1;
        publishNow(state);
    }

    public synchronized void progress(NotificationKey key, long seq, int percent) {
        State state = states.get(key);
        if (state == null || state.stepState != StepState.TRANSFERRING || seq <= state.sequence) return;
        state.sequence = seq;
        state.percent = percent < 0 ? -1 : Math.max(state.percent, Math.min(100, percent));

        long now = System.currentTimeMillis();
        if (now - state.lastPublishTime >= 1000) {
            publishNow(state);
        }
    }

    public synchronized void fileFinished(NotificationKey key, String fileId, boolean success, boolean cancelled) {
        State state = states.get(key);
        if (state == null) return;
        ResultState result = success ? ResultState.SUCCESS : cancelled ? ResultState.CANCELLED : ResultState.FAILED;
        if (fileId != null) state.results.put(fileId, result);
        state.percent = -1;
        state.stepState = StepState.PREPARING;
        publishNow(state);
    }

    public synchronized void finish(NotificationKey key, ResultState result) {
        State state = states.get(key);
        if (state == null) return;
        state.stepState = StepState.RESULT;
        state.result = state.results.containsValue(ResultState.FAILED) ? ResultState.FAILED
                : (result == ResultState.SUCCESS && state.results.containsValue(ResultState.CANCELLED)) ? ResultState.CANCELLED : result;

        publishNow(state);

        // Auto remove success or cancelled notification after 10s
        if (state.result != ResultState.FAILED && mainHandler != null) {
            final long currentVersion = state.sessionVersion;
            mainHandler.postDelayed(() -> {
                synchronized (TransferNotifications.this) {
                    State current = states.get(key);
                    if (current == state && current.sessionVersion == currentVersion) {
                        cancelNotification(key);
                        states.remove(key);
                    }
                }
            }, 10000);
        }
    }

    private void cancelNotification(NotificationKey key) {
        if (manager == null) return;
        try {
            manager.cancel(key.id);
        } catch (RuntimeException ignored) {
        }
    }

    private void publishNow(State state) {
        state.lastPublishTime = System.currentTimeMillis();
        if (context == null || manager == null) return;

        try {
            if (!NotificationManagerCompat.from(context).areNotificationsEnabled()) {
                return;
            }

            android.app.NotificationChannel channel = manager.getNotificationChannel(NotificationUtils.FILE_TRANSFER_CHANNEL);
            if (channel != null && channel.getImportance() == NotificationManager.IMPORTANCE_NONE) {
                return;
            }

            String title = title(context, state.key);
            int stepStateRes = switch (state.stepState) {
                case SCANNING -> R.string.transfer_notice_scanning;
                case PREPARING -> R.string.transfer_notice_preparing;
                case TRANSFERRING -> state.key == NotificationKey.DOWNLOAD ? R.string.transfer_notice_downloading : R.string.transfer_notice_uploading;
                case RESULT -> switch (state.result) {
                    case FAILED -> R.string.transfer_notice_failed;
                    case CANCELLED -> R.string.transfer_notice_cancelled;
                    case SUCCESS -> state.succeeded() == 0 ? R.string.transfer_notice_empty : R.string.transfer_notice_success;
                };
            };
            title += " · " + context.getString(stepStateRes);

            String detail = state.stepState == StepState.TRANSFERRING
                    ? state.fileName + (state.percent < 0 ? "" : " (" + state.percent + "%)")
                    : context.getString(stepStateRes);

            String counts = context.getString(R.string.transfer_notice_counts, state.succeeded(), state.failed());

            Intent intent = new Intent(context, TransferActivity.class)
                    .setAction(context.getPackageName() + ".transfer." + state.key.name())
                    .addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP)
                    .putExtra(NotificationUtils.NOTIFICATION_MESSAGE_KEY,
                            state.key == NotificationKey.DOWNLOAD
                                    ? NotificationUtils.NOTIFICATION_OPEN_DOWNLOAD_TAB
                                    : NotificationUtils.NOTIFICATION_OPEN_UPLOAD_TAB);

            PendingIntent click = PendingIntent.getActivity(context, state.key.id, intent,
                    PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);

            boolean isResult = state.stepState == StepState.RESULT;

            NotificationCompat.Builder builder = new NotificationCompat.Builder(context, NotificationUtils.FILE_TRANSFER_CHANNEL)
                    .setSmallIcon(R.drawable.icon)
                    .setContentTitle(title)
                    .setContentText(detail)
                    .setSubText(counts)
                    .setContentIntent(click)
                    .setOnlyAlertOnce(true)
                    .setSilent(true)
                    .setPriority(NotificationCompat.PRIORITY_LOW)
                    .setCategory(NotificationCompat.CATEGORY_PROGRESS)
                    .setOngoing(!isResult)
                    .setAutoCancel(isResult);

            if (!isResult) {
                builder.setProgress(100, Math.max(0, state.percent), state.percent < 0);
            }

            manager.notify(state.key.id, builder.build());
        } catch (RuntimeException e) {
            android.util.Log.w("TransferNotifications", "Publish failed: " + state.key, e);
        }
    }

    private static String title(Context context, NotificationKey key) {
        return context.getString(switch (key) {
            case ALBUM -> R.string.settings_camera_upload_info_title;
            case FOLDER -> R.string.settings_folder_backup_info_title;
            case DOWNLOAD -> R.string.download;
            case MANUAL -> R.string.transfer_notice_manual;
            case SHARE -> R.string.transfer_notice_share;
            case LOCAL_UPDATE -> R.string.transfer_notice_local;
        });
    }
}
