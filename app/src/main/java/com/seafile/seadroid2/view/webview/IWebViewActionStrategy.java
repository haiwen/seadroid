package com.seafile.seadroid2.view.webview;

public interface IWebViewActionStrategy {

    /**
     * return null means does not callback
     */
    String route(String paramsStr);
}
