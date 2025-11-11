package com.seafile.seadroid2.enums;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.model.sdoc.TextTypeModel;

public enum TextTypeEnum {
    undo, redo, paragraph, title, subtitle,
    header1, header2, header3, header4, header5, header6,
    unordered_list, ordered_list, check_list_item;

    public static boolean isTextType(TextTypeModel typeModel) {
        if (TextUtils.equals(TextTypeEnum.title.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.title.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.paragraph.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.header1.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.header2.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.header3.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.header4.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.header5.name(), typeModel.type)) {
            return true;
        } else if (TextUtils.equals(TextTypeEnum.header6.name(), typeModel.type)) {
            return true;
        }

        return false;
    }
}
