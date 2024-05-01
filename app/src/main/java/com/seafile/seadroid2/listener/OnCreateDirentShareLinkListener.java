package com.seafile.seadroid2.listener;

import androidx.annotation.Nullable;

import com.seafile.seadroid2.framework.data.model.objs.DirentShareLinkModel;

public interface OnCreateDirentShareLinkListener {
    void onCreateDirentShareLink(@Nullable DirentShareLinkModel linkModel);
}
