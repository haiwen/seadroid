package com.seafile.seadroid2.ui.file_profile;

import android.content.res.Resources;
import android.os.Build;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.ColumnType;

public class ColumnTypeUtils {
    //    private int getIconByColumnType(String type, String key) {
//        return switch (type) {
//            case ColumnType.TEXT -> R.drawable.ic_single_line_text;
//            case ColumnType.COLLABORATOR -> R.drawable.ic_user_collaborator;
//            case ColumnType.IMAGE -> R.drawable.ic_picture;
//            case ColumnType.FILE -> R.drawable.ic_file_alt_solid;
//            case ColumnType.DATE -> R.drawable.ic_calendar_alt_solid;
//            case ColumnType.SINGLE_SELECT -> R.drawable.ic_single_election;
//            case ColumnType.DURATION -> R.drawable.ic_duration;
//            case ColumnType.MULTIPLE_SELECT -> R.drawable.ic_multiple_selection;
//            case ColumnType.CHECKBOX -> R.drawable.ic_check_square_solid;
//            case ColumnType.GEOLOCATION -> R.drawable.ic_location;
//            case ColumnType.EMAIL -> R.drawable.ic_email;
//            case ColumnType.LONG_TEXT -> R.drawable.ic_long_text;
//            case ColumnType.NUMBER -> R.drawable.ic_number;
//            case ColumnType.RATE -> R.drawable.ic_star_32;
//            case ColumnType.URL -> R.drawable.ic_url;
//            case ColumnType.LINK -> "_tags".equals(key) ? R.drawable.ic_tag : R.drawable.ic_links;
//
//            default -> R.drawable.ic_single_line_text;
//        };
//    }

    public static int getIconByColumnType(String type, String key) {
        return switch (type) {
            case ColumnType.LONG_TEXT -> R.drawable.icon_editor_long_text;
            case ColumnType.COLLABORATOR -> R.drawable.icon_user_collaborator;
            case ColumnType.IMAGE -> R.drawable.icon_editor_image;
            case ColumnType.FILE -> R.drawable.icon_files;
            case ColumnType.DATE -> R.drawable.icon_editor_date;
            case ColumnType.SINGLE_SELECT -> R.drawable.icon_editor_single_select;
            case ColumnType.DURATION -> R.drawable.icon_editor_time;
            case ColumnType.MULTIPLE_SELECT -> R.drawable.icon_editor_multiple_select;
            case ColumnType.CHECKBOX -> R.drawable.icon_editor_checkbox;
            case ColumnType.GEOLOCATION -> R.drawable.icon_editor_location;
            case ColumnType.EMAIL -> R.drawable.icon_email;
            case ColumnType.NUMBER -> R.drawable.icon_editor_number;
            case ColumnType.RATE -> R.drawable.icon_starred;
            case ColumnType.URL -> R.drawable.icon_editor_link;
            case ColumnType.LINK -> "_tags".equals(key) ? R.drawable.icon_editor_tag_filled : R.drawable.icon_editor_link_file;

            default -> R.drawable.icon_editor_text;
        };
    }


    public static int getResNameByKey(String key) {
        switch (key) {
            case "_description":
                return R.string.description;
            case "_file_modifier":
                return R.string._last_modifier;
            case "_file_mtime":
                return R.string._last_modified_time;
            case "_status":
                return R.string._file_status;
            case "_collaborators":
                return R.string._file_collaborators;
            case "_size":
                return R.string._size;
            case "_reviewer":
                return R.string._reviewer;
            case "_in_progress":
                return R.string._in_progress;
            case "_in_review":
                return R.string._in_review;
            case "_done":
                return R.string._done;
            case "_outdated":
                return R.string._outdated;
            case "_location":
                return R.string._location;
            case "_tags":
                return R.string._tags;
            case "_owner":
                return R.string._owner;
            case "_rate":
                return R.string._rate;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            return Resources.ID_NULL;
        }
        return 0;
    }
}
