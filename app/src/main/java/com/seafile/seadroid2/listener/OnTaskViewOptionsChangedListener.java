package com.seafile.seadroid2.listener;

import com.seafile.seadroid2.framework.model.sdoc.OptionsTagModel;

import java.util.List;

public interface OnTaskViewOptionsChangedListener {
    void onChanged(List<OptionsTagModel> optionsModels);
}
