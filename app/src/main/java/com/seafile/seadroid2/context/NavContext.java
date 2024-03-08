package com.seafile.seadroid2.context;

import android.text.TextUtils;

import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;

import java.util.Stack;

public class NavContext {
    private final Stack<BaseModel> navStack = new Stack<>();

    private String repoID = null;
    private String repoName = null;     // for display
    private String dirPath = null;
    private String dirID = null;

    //(repo or dir)'s permission, recommend
    private String permission = null;

    public NavContext() {
    }

    public void setRepoID(String repoID) {
        this.repoID = repoID;
    }

    public void setRepoName(String repoName) {
        this.repoName = repoName;
    }

    public void setDirPath(String path) {
        this.dirPath = path;
    }


    public String getRepoID() {
        return repoID;
    }

    public String getRepoName() {
        return repoName;
    }

    public boolean isRepoRoot() {
        return "/".equals(dirPath);
    }

    public String getDirPath() {
        return dirPath;
    }

    public String getDirPathName() {
        return dirPath.substring(dirPath.lastIndexOf("/") + 1);
    }

    public String getPermission() {
        return permission;
    }

    public void setPermission(String permission) {
        this.permission = permission;
    }

    public boolean inRepo() {
        return repoID != null;
    }

    ////////////////////////////new////////////////////////

    //repoId = xxx, path = /
    public boolean isInRepoRoot() {
        return navStack.size() == 1;
    }

    //repo list [age
    public boolean isInRepoList() {
        return navStack.size() == 0;
    }

    /**
     * Not on the Repo list page
     */
    public boolean isInRepo() {
        return navStack.size() >= 1;
    }

    public void clear() {
        if (!navStack.empty())
            navStack.clear();
    }

    public void push(BaseModel model) {
        if (model instanceof RepoModel) {
            //There is only one RepoModel

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
//                dm.type =
                dm.full_path = stringBuilder.toString();
                dm.uid = EncryptUtils.encryptMD5ToString(dm.full_path);

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
        if (navStack.empty()) {
            return null;
        }
        return (DirentModel) navStack.peek();
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

    public String getLastPathName() {
        String fullPath = getNavPath();
        if (TextUtils.isEmpty(fullPath)) {
            return null;
        }

        if (!fullPath.contains("/")) {
            return fullPath;
        }

        String[] slash = fullPath.split("/");
        if (slash.length == 0) {
            return null;
        }

        return slash[slash.length - 1];
    }

    public String getNameInCurPath() {
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

    //
    public boolean hasWritePermissionWithRepo() {

//        BaseModel baseModel = getTopModel();
//        if (baseModel == null) {
//            return false;
//        }
//
//        if (baseModel instanceof RepoModel) {
//            return ((RepoModel) baseModel).hasWritePermission();
//        }
//
//        if (baseModel instanceof DirentModel) {
//            return ((DirentModel) baseModel).hasWritePermission();
//        }

        return getRepoModel().hasWritePermission();
    }

}
