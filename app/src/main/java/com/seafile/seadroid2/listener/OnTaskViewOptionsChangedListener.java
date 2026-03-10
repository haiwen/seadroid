package com.seafile.seadroid2.listener;

import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;

import java.util.List;

public interface OnTaskViewOptionsChangedListener {
    void onChanged(List<OptionTagModel> optionsModels);
}
