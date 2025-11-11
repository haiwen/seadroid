package com.seafile.seadroid2.config;

public class WebViewActionConstant {
    public final static String APP_VERSION_GET = "app.version.get";
    public final static String APP_TOAST_SHOW = "app.toast.show";
    public final static String PAGE_FINISH = "page.finish";
    public final static String PAGE_STATUS_COLOR_SET = "page.status.color.set";
    public final static String PAGE_STATUS_HEIGHT_GET = "page.status.height.get";


    //This action is run when the content changes
    public final static String SDOC_EDITOR_CONTENT_SELECT = "sdoc.editor.content.select";
    //This action is run when the cursor position changes
    public final static String SDOC_EDITOR_OPERATION_EXECUTE = "sdoc.editor.operation.execute";


    public static class CallJsFunction {
        public final static String SDOC_OUTLINES_DATA_GET = "sdoc.outline.data.get";
        public final static String SDOC_OUTLINES_DATA_SELECT = "sdoc.outline.data.select";
        public final static String SDOC_EDITOR_DATA_EDIT = "sdoc.editor.data.edit";
        public final static String SDOC_TOOLBAR_MENU_TRIGGER = "sdoc.toolbar.menu.trigger";

        public static class CallJsFunctionRequestCode {
            public final static int REQ = 1001;
        }
    }

}
