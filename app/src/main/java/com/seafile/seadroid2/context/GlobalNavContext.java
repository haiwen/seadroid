package com.seafile.seadroid2.context;

import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;

public class GlobalNavContext {
    private static final NavContext _current_context = new NavContext();

    public static NavContext getCurrentNavContext() {
        return _current_context;
    }

    public static void push(BaseModel baseModel) {
        getCurrentNavContext().push(baseModel);
        post();
    }

    public static void pop() {
        getCurrentNavContext().pop();
        post();
    }

    /**
     * restore from the sp
     */
    public static void restore() {
        NavContext c = ContextStackPreferenceHelper.getNavContextStack();
        getCurrentNavContext().restoreFromSelf(c);
    }

    public static void switchToPath(RepoModel repoModel, String full_path) {
        getCurrentNavContext().switchToPath(repoModel, full_path);
        post();
    }

    private static void post() {
        BusHelper.getNavContextObserver().post(getCurrentNavContext());
    }

    /**
     * re-post: notify again
     */
    public static void ding() {
        post();
    }
}
