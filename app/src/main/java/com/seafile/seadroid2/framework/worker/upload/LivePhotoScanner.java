package com.seafile.seadroid2.framework.worker.upload;

import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore;

import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.framework.model.livephoto.LivePhotoModel;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LivePhotoScanner extends Worker {

    public LivePhotoScanner(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {

        List<LivePhotoModel> scanList = scanLivePhotos(getApplicationContext());
        if (!scanList.isEmpty()) {
            scanList.forEach(livePhotoModel -> {
                SLogs.d("LivePhotoScan", livePhotoModel.toString());
            });
        } else {
            SLogs.d("LivePhotoScan ", "No live photos found.");

        }

        return Result.success();
    }

    private static final long TIME_TOLERANCE_MS = 3000; // 3秒容差

    public static List<LivePhotoModel> scanLivePhotos(Context context) {
        Map<String, Pair<String, Long>> imageMap = loadMediaMap(context, MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
        Map<String, Pair<String, Long>> videoMap = loadMediaMap(context, MediaStore.Video.Media.EXTERNAL_CONTENT_URI);

        List<LivePhotoModel> result = new ArrayList<>();

        for (String key : imageMap.keySet()) {
            Pair<String, Long> image = imageMap.get(key);
            if (image == null) continue;

            for (Pair<String, Long> video : videoMap.values()) {
                if (video == null) continue;
                long diff = Math.abs(video.second - image.second);
                String videoUriString = video.first;

                if (diff <= TIME_TOLERANCE_MS && videoUriString.toLowerCase().contains(key.toLowerCase())) {
                    result.add(new LivePhotoModel(
                            Uri.parse(image.first),
                            Uri.parse(video.first),
                            key,
                            image.second
                    ));
                    break;
                }
            }
        }

        return result;
    }

    private static Map<String, Pair<String, Long>> loadMediaMap(Context context, Uri contentUri) {
        Map<String, Pair<String, Long>> map = new HashMap<>();

        String[] projection = {
                MediaStore.MediaColumns._ID,
                MediaStore.MediaColumns.DISPLAY_NAME,
                MediaStore.MediaColumns.DATE_TAKEN
        };

        Cursor cursor = context.getContentResolver().query(
                contentUri,
                projection,
                null,
                null,
                MediaStore.MediaColumns.DATE_TAKEN + " DESC"
        );

        if (cursor != null) {
            int idIndex = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns._ID);
            int nameIndex = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DISPLAY_NAME);
            int dateIndex = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATE_TAKEN);

            while (cursor.moveToNext()) {
                long id = cursor.getLong(idIndex);
                String name = cursor.getString(nameIndex);
                long dateTaken = cursor.getLong(dateIndex);
                String key = name.contains(".") ? name.substring(0, name.lastIndexOf('.')) : name;

                Uri uri = ContentUris.withAppendedId(contentUri, id);
                map.put(key, new Pair<>(uri.toString(), dateTaken));
            }
            cursor.close();
        }

        return map;
    }
}
