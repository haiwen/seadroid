package com.seafile.seadroid2.view.webview.strategy;

import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.framework.model.sdoc.TextTypeModel;
import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;
import com.seafile.seadroid2.view.webview.OnWebDataCallback;

public class SdocEditorContentSelectStrategy implements IWebViewActionStrategy {
    OnWebDataCallback callback;

    public SdocEditorContentSelectStrategy(OnWebDataCallback callback) {
        this.callback = callback;
    }

    @Override
    public String route(String dataStr) {
        return null;
    }
}
