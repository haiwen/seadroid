/*
 * Copyright 2013 - learnNcode (learnncode@gmail.com)
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */


package com.seafile.seadroid2.mediachooser;

import android.app.AlertDialog;
import android.content.Context;
import android.view.LayoutInflater;
import com.seafile.seadroid2.R;

import java.io.File;


public class MediaChooserConstants {

    /**
     * folder name on which captured photo & video are saved on sd card.
     */
    public static String folderName = "Seafile";

    /**
     * No of item that can be selected. Default is 100.
     */
    public static int MAX_MEDIA_LIMIT = 100;

    /**
     * Selected media file count.
     */
    public static int SELECTED_MEDIA_COUNT = 0;

    public static boolean showCameraVideo = true;
    public static boolean showVideo = true;
    public static boolean showImage = true;

    public static int SELECTED_IMAGE_SIZE_IN_MB = 20;
    public static int SELECTED_VIDEO_SIZE_IN_MB = 20;


    public static final int BUCKET_SELECT_IMAGE_CODE = 1000;
    public static final int BUCKET_SELECT_VIDEO_CODE = 2000;

    public static final int MEDIA_TYPE_IMAGE = 1;
    public static final int MEDIA_TYPE_VIDEO = 2;

    public static final int CAPTURE_IMAGE_ACTIVITY_REQUEST_CODE = 100;
    public static final int CAPTURE_VIDEO_ACTIVITY_REQUEST_CODE = 200;

    public static final String MEDIA_SELECTED_LIST = "list";

    public static long ChekcMediaFileSize(File mediaFile, boolean isVideo) {

        /** Get length of file in bytes */
        long fileSizeInBytes = mediaFile.length();

        /** Convert the bytes to Kilobytes (1 KB = 1024 Bytes) */
        long fileSizeInKB = fileSizeInBytes / 1024;

        /** Convert the KB to MegaBytes (1 MB = 1024 KBytes) */
        long fileSizeInMB = fileSizeInKB / 1024;

        int requireSize = 0;
        if (isVideo) {
            requireSize = MediaChooserConstants.SELECTED_VIDEO_SIZE_IN_MB;
        } else {
            requireSize = MediaChooserConstants.SELECTED_IMAGE_SIZE_IN_MB;
        }
        if (fileSizeInMB <= requireSize) {
            return 0;
        }
        return fileSizeInMB;
    }


    public static AlertDialog.Builder getDialog(Context context) {
        final AlertDialog.Builder dialog = new AlertDialog.Builder(context);
        dialog.setCancelable(false);
        dialog.setTitle(context.getString(R.string.media_chooser_please_wait));
        LayoutInflater layoutInflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        dialog.setView(layoutInflater.inflate(R.layout.media_chooser_view_loading, null));
        return dialog;
    }

}
