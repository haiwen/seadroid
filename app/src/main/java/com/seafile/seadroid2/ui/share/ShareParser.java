package com.seafile.seadroid2.ui.share;

import android.content.ClipData;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

public class ShareParser {
    public static class ShareResult {
        public @Nullable String plainText; // text from EXTRA_TEXT
        public @NonNull List<Uri> uriList = new ArrayList<>(); // Uri list from EXTRA_STREAM or ClipData
    }

    /**
     * Parse and share content, including plain text, single file, and multiple files.
     */
    public static ShareResult parseSharedContent(@NonNull Intent intent, @NonNull Context context) {
        ShareResult result = new ShareResult();

        String type = intent.getType();
        if (type == null) return result;

        // 1. text EXTRA_TEXT
        if ("text/plain".equals(type)) {
            String sharedText = intent.getStringExtra(Intent.EXTRA_TEXT);
            if (!TextUtils.isEmpty(sharedText)) {
                result.plainText = sharedText;
                return result;
            }
        }

        // 2. single Uri
        Uri streamUri = intent.getParcelableExtra(Intent.EXTRA_STREAM);
        if (streamUri != null) {
            if (!result.uriList.contains(streamUri)) {
                result.uriList.add(streamUri);
            }
        }

        // 3. multiple Uri
        ClipData clipData = intent.getClipData();
        if (clipData != null && clipData.getItemCount() > 0) {
            for (int i = 0; i < clipData.getItemCount(); i++) {
                Uri uri = clipData.getItemAt(i).getUri();
                if (uri != null) {
                    if (!result.uriList.contains(streamUri)) {
                        result.uriList.add(uri);
                    }
                }
            }
        }

        return result;
    }
}