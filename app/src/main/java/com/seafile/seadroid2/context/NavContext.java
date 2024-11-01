package com.seafile.seadroid2.context;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.util.Utils;

import java.util.Stack;

public class NavContext {
    private final Stack<BaseModel> navStack = new Stack<>();

    // repoId = xxx, path = /
    public boolean inRepoRoot() {
        return navStack.size() == 1;
    }

    /**
     * @return true: it's in a repo, false: not
     */
    public boolean inRepo() {
        return !navStack.isEmpty();
    }

    public void clear() {
        if (!navStack.empty()) {
            navStack.clear();
        }
    }

    public void push(BaseModel model) {
        if (model instanceof RepoModel) {
            //clear
            navStack.clear();

            //push
            navStack.push(model);
        } else if (model instanceof DirentModel) {
            //stack
            navStack.push(model);
        } else {
            throw new IllegalArgumentException("model must be RepoMode or DirentsModel.");
        }
    }

    public void pop() {
        if (navStack.empty()) {
            return;
        }

        //stack
        navStack.pop();
    }

    public void navToPath(RepoModel repoModel, String full_path) {
        navStack.clear();

        navStack.push(repoModel);

        Stack<DirentModel> stack = new Stack<>();
        String[] slash = full_path.split("/");
//        boolean isDir = full_path.endsWith("/");

        if (slash.length > 1) {
            StringBuilder stringBuilder = new StringBuilder();
            for (String s : slash) {
                if (TextUtils.isEmpty(s)) {
                    continue;
                }

                stringBuilder.append("/").append(s);
                DirentModel dm = new DirentModel();
                dm.name = s;
                dm.repo_id = repoModel.repo_id;
                dm.repo_name = repoModel.repo_name;
                dm.full_path = stringBuilder.toString();
                dm.parent_dir = Utils.getParentPath(dm.full_path);
                dm.uid = dm.getUID();
                stack.push(dm);
            }
        }

        for (DirentModel model : stack) {
            navStack.push(model);
        }
    }

    /**
     * Get the dirent model at the top of the stack
     */
    public DirentModel getTopDirentModel() {
        if (navStack.empty() || navStack.size() == 1) {
            return null;
        }
        return (DirentModel) navStack.peek();
    }

    /**
     * Get the parents model of the current Dirent, maybe RepoModel
     */
    public boolean isParentHasWritePermission() {
        if (!inRepo()) {
            //repo list page should not have permission verification
            throw new IllegalArgumentException("Please check your code");
        }

        if (inRepoRoot()) {
            return getRepoModel().hasWritePermission();
        }

        BaseModel bd = navStack.elementAt(navStack.size() - 1);
        DirentModel d = (DirentModel) bd;
        if (d == null) {
            return false;
        }

        return d.hasWritePermission();
    }

    public RepoModel getRepoModel() {
        if (navStack.empty()) {
            return null;
        }

        return (RepoModel) navStack.get(0);
    }

    /**
     * Get the model at the top of the stack
     */
    public BaseModel getTopModel() {
        if (navStack.empty()) {
            return null;
        }
        return navStack.peek();
    }

    /**
     * @return /a/b/c/d/e/
     */
    public String getNavPath() {
        if (navStack.empty()) {
            return null;
        }

        if (navStack.size() == 1) {
            return "/";
        }

        DirentModel direntModel = getTopDirentModel();
        if (direntModel == null) {
            return "/";
        }

        String fullPath = direntModel.full_path;
        if (direntModel.isDir() && !fullPath.endsWith("/")) {
            fullPath += "/";
        }

        return fullPath;
    }

    /**
     * /a/b/c -> c
     */
    public String getLastPathName() {
        BaseModel baseModel = getTopModel();
        if (baseModel == null) {
            return null;
        }

        if (baseModel instanceof RepoModel) {
            return ((RepoModel) baseModel).repo_name;
        }

        if (baseModel instanceof DirentModel) {
            return ((DirentModel) baseModel).name;
        }

        return null;
    }
}
