package com.seafile.seadroid2.util;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.text.TextUtils;

import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;

import com.blankj.utilcode.util.ClipboardUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.SupportDataManager;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.model.GroupItemModel;
import com.seafile.seadroid2.data.model.objs.DirentShareLinkModel;
import com.seafile.seadroid2.listener.OnCreateDirentShareLinkListener;
import com.seafile.seadroid2.ui.dialog_fragment.AppChoiceDialogFragment;
import com.seafile.seadroid2.ui.dialog.FetchFileDialog;
import com.seafile.seadroid2.ui.dialog_fragment.GetShareLinkPasswordDialogFragment;
import com.seafile.seadroid2.util.sp.Sorts;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Collectors;

import kotlin.Pair;

public class Objs {

    //repo
    public static List<BaseModel> parseRepoListForAdapter(List<RepoModel> list, String related_account, boolean isFilterEncrypted) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        for (int i = 0; i < list.size(); i++) {
            list.get(i).related_account = related_account;
        }

        if (isFilterEncrypted) {
            list = list.stream().filter(f -> !f.encrypted).collect(Collectors.toList());
        }

        List<BaseModel> newRvList = CollectionUtils.newArrayList();

        TreeMap<String, List<RepoModel>> treeMap = groupRepos(list);

        //mine
        List<RepoModel> mineList = treeMap.get("mine");
        if (!CollectionUtils.isEmpty(mineList)) {
            newRvList.add(new GroupItemModel(R.string.personal));
            for (RepoModel repoModel : mineList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(mineList);
            newRvList.addAll(sortedList);
        }

        //shared
        List<RepoModel> sharedList = treeMap.get("shared");
        if (!CollectionUtils.isEmpty(sharedList)) {
            newRvList.add(new GroupItemModel(R.string.shared));
            for (RepoModel repoModel : sharedList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(sharedList);
            newRvList.addAll(sortedList);
        }

        for (String key : treeMap.keySet()) {
            if (TextUtils.equals(key, "mine")) {
            } else if (TextUtils.equals(key, "shared")) {
            } else {
                List<RepoModel> groupList = treeMap.get(key);
                if (!CollectionUtils.isEmpty(groupList)) {
                    newRvList.add(new GroupItemModel(key));
                    for (RepoModel repoModel : groupList) {
                        repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
                    }

                    List<RepoModel> sortedList = sortRepos(groupList);
                    newRvList.addAll(sortedList);
                }
            }
        }

        return newRvList;
    }

    public static List<RepoModel> parseRepoListForDB(List<RepoModel> list, String related_account) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        for (int i = 0; i < list.size(); i++) {
            list.get(i).related_account = related_account;
        }

        List<RepoModel> newDbList = CollectionUtils.newArrayList();

        TreeMap<String, List<RepoModel>> treeMap = groupRepos(list);

        //mine
        List<RepoModel> mineList = treeMap.get("mine");
        if (!CollectionUtils.isEmpty(mineList)) {
            for (RepoModel repoModel : mineList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(mineList);
            newDbList.addAll(sortedList);
        }

        //shared
        List<RepoModel> sharedList = treeMap.get("shared");
        if (!CollectionUtils.isEmpty(sharedList)) {
            for (RepoModel repoModel : sharedList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(sharedList);
            newDbList.addAll(sortedList);
        }

        for (String key : treeMap.keySet()) {
            if (TextUtils.equals(key, "mine")) {
            } else if (TextUtils.equals(key, "shared")) {
            } else {
                List<RepoModel> groupList = treeMap.get(key);
                if (!CollectionUtils.isEmpty(groupList)) {
                    for (RepoModel repoModel : groupList) {
                        repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
                    }

                    List<RepoModel> sortedList = sortRepos(groupList);
                    newDbList.addAll(sortedList);
                }
            }
        }
        return newDbList;
    }

    /**
     * Whether the dbList is included in the netList.<br>
     * pair.first is need to delete.<br>
     * pair.second is need to add.<br>
     */
    public static Pair<List<RepoModel>, List<RepoModel>> diffRepos(List<RepoModel> netList, List<RepoModel> dbList) {

        if (CollectionUtils.isEmpty(netList) && CollectionUtils.isEmpty(dbList)) {
            return null;
        }

        //if netList is empty, delete all local data.
        if (CollectionUtils.isEmpty(netList)) {
            return new Pair<>(dbList, null);
        }

        //if dbList is empty, insert all net data into DB.
        if (CollectionUtils.isEmpty(dbList)) {
            return new Pair<>(null, netList);
        }

        List<String> repoIds = netList.stream().map(m -> m.repo_id).collect(Collectors.toList());
        List<RepoModel> deleteList = dbList.stream().filter(f -> !repoIds.contains(f.repo_id)).collect(Collectors.toList());
        List<RepoModel> addList = dbList.stream().filter(f -> repoIds.contains(f.repo_id)).collect(Collectors.toList());

        for (RepoModel nModel : netList) {
            for (RepoModel repoModel : addList) {
                if (TextUtils.equals(nModel.repo_id, repoModel.repo_id)) {
                    nModel.root = repoModel.root;
                    nModel.magic = repoModel.magic;
                    nModel.random_key = repoModel.random_key;
                    nModel.enc_version = repoModel.enc_version;
                    nModel.file_count = repoModel.file_count;
                    break;
                }
            }
        }

        return new Pair<>(deleteList, netList);
    }

    private static List<RepoModel> sortRepos(List<RepoModel> repos) {
        List<RepoModel> newRepos = new ArrayList<>();

        int sortType = Sorts.getSortType();
        switch (sortType) {
            case 0: // sort by name, ascending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return o1.repo_name.compareTo(o2.repo_name);
                    }
                }).collect(Collectors.toList());

                break;
            case 1: // sort by name, descending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return -o1.repo_name.compareTo(o2.repo_name);
                    }
                }).collect(Collectors.toList());
                break;
            case 2: // sort by last modified time, ascending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return o1.last_modified_long < o2.last_modified_long ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
            case 3: // sort by last modified time, descending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return o1.last_modified_long > o2.last_modified_long ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
        }
        return newRepos;
    }

    private static TreeMap<String, List<RepoModel>> groupRepos(List<RepoModel> repos) {
        TreeMap<String, List<RepoModel>> map = new TreeMap<String, List<RepoModel>>();
        for (RepoModel repo : repos) {
            if (TextUtils.equals(repo.type, "group")) {
                List<RepoModel> l = map.computeIfAbsent(repo.group_name, k -> Lists.newArrayList());
                l.add(repo);
            } else {
                List<RepoModel> l = map.computeIfAbsent(repo.type, k -> Lists.newArrayList());
                l.add(repo);
            }
        }
        return map;
    }

    //dirent

    /**
     * Resolve the dirents of the local database
     */
    public static List<BaseModel> parseLocalDirents(List<DirentModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        TreeMap<String, List<DirentModel>> treeMap = groupDirents(list);
        List<DirentModel> dirModels = treeMap.get("dir");
        List<DirentModel> fileModels = treeMap.get("file");

        List<DirentModel> newList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(dirModels)) {
            newList.addAll(sortDirents(dirModels));
        }

        if (!CollectionUtils.isEmpty(fileModels)) {
            newList.addAll(sortDirents(fileModels));
        }

        return new ArrayList<>(newList);
    }

    /**
     * Resolve to a list of local databases
     */
    public static List<DirentModel> parseDirentsForDB(List<DirentModel> list,
                                                      String dir_id,
                                                      String related_account,
                                                      String repo_id,
                                                      String repo_name) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        TreeMap<String, List<DirentModel>> treeMap = groupDirents(list);
        List<DirentModel> dirModels = treeMap.get("dir");
        List<DirentModel> fileModels = treeMap.get("file");

        List<DirentModel> newDbList = new ArrayList<>();
        long now = TimeUtils.getNowMills();
        if (!CollectionUtils.isEmpty(dirModels)) {
            for (int i = 0; i < dirModels.size(); i++) {
                //
                dirModels.get(i).last_sync_time = now;
                dirModels.get(i).dir_id = dir_id;
                dirModels.get(i).related_account = related_account;
                dirModels.get(i).repo_id = repo_id;
                dirModels.get(i).repo_name = repo_name;
                dirModels.get(i).full_path = dirModels.get(i).parent_dir + dirModels.get(i).name;
                dirModels.get(i).uid = dirModels.get(i).getUID();
            }
            newDbList.addAll(sortDirents(dirModels));
        }

        if (!CollectionUtils.isEmpty(fileModels)) {
            for (int i = 0; i < fileModels.size(); i++) {
                //
                fileModels.get(i).repo_id = repo_id;
                fileModels.get(i).repo_name = repo_name;
                fileModels.get(i).last_sync_time = now;
                fileModels.get(i).dir_id = dir_id;
                fileModels.get(i).related_account = related_account;
                fileModels.get(i).full_path = fileModels.get(i).parent_dir + fileModels.get(i).name;
                fileModels.get(i).uid = fileModels.get(i).getUID();
            }
            newDbList.addAll(sortDirents(fileModels));
        }

        return newDbList;
    }

    private static TreeMap<String, List<DirentModel>> groupDirents(List<DirentModel> list) {
        TreeMap<String, List<DirentModel>> map = new TreeMap<String, List<DirentModel>>();
        for (DirentModel repo : list) {
            List<DirentModel> l = map.computeIfAbsent(repo.type, k -> Lists.newArrayList());
            l.add(repo);
        }
        return map;
    }

    private static List<DirentModel> sortDirents(List<DirentModel> list) {
        List<DirentModel> newList = new ArrayList<>();

        int sortType = Sorts.getSortType();
        switch (sortType) {
            case 0: // sort by name, ascending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return o1.name.compareTo(o2.name);
                    }
                }).collect(Collectors.toList());

                break;
            case 1: // sort by name, descending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return -o1.name.compareTo(o2.name);
                    }
                }).collect(Collectors.toList());
                break;
            case 2: // sort by last modified time, ascending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return o1.mtime < o2.mtime ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
            case 3: // sort by last modified time, descending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return o1.mtime > o2.mtime ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
        }
        return newList;
    }

    /**
     * Each repo is placed under [account-dir]/[repo-name]. When a
     * file is downloaded, it's placed in its repo, with its full path.
     *
     * @param repoName
     * @param repoID
     * @param path
     */
    public static File getLocalFile(String repoName, String repoID, String path) throws RuntimeException {
        if (TextUtils.isEmpty(repoID)) {
            return null;
        }

//        String repoDir = getRepoDir(repoName, repoID);
//        if (TextUtils.isEmpty(repoDir)) {
//            return null;
//        }
//        String localPath = Utils.pathJoin(repoDir, path);

        //build valid file path and name
//        localPath = com.seafile.seadroid2.util.FileUtils.buildValidFilePathName(localPath);

//        File parentDir = new File(Utils.getParentPath(localPath));
//        if (!parentDir.exists()) {
//            // TODO should check if the directory creation succeeds
//            parentDir.mkdirs();
//        }

        return new File(path);
    }

    public static void showChooseAppDialog(Context context, FragmentManager fragmentManager, DirentShareLinkModel shareLinkModel, boolean isDir) {
        String title = context.getString(isDir ? R.string.share_dir_link : R.string.share_file_link);

        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");
        List<ResolveInfo> infos = Utils.getAppsByIntent(shareIntent);

        AppChoiceDialogFragment dialog = new AppChoiceDialogFragment();
        dialog.addCustomAction(0,
                ContextCompat.getDrawable(context, R.drawable.copy_link),
                context.getString(R.string.copy_link));
        dialog.init(title, infos, new AppChoiceDialogFragment.OnItemSelectedListener() {
            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                shareIntent.setClassName(packageName, className);
                shareIntent.putExtra(Intent.EXTRA_TEXT, shareLinkModel.link);
                context.startActivity(shareIntent);
                dialog.dismiss();
            }

            @Override
            public void onCustomActionSelected(AppChoiceDialogFragment.CustomAction action) {
                ClipboardUtils.copyText(shareLinkModel.link);
                ToastUtils.showLong(R.string.link_ready_to_be_pasted);
                dialog.dismiss();
            }
        });
        dialog.show(fragmentManager, AppChoiceDialogFragment.class.getSimpleName());
    }

    public static void showCreateEncryptShareLinkDialog(Context context, FragmentManager fragmentManager, DirentModel direntModel, boolean isAdvance) {
        GetShareLinkPasswordDialogFragment dialogFragment = new GetShareLinkPasswordDialogFragment();
        dialogFragment.init(direntModel.repo_id, direntModel.full_path, isAdvance);
        dialogFragment.setOnCreateDirentShareLinkListener(new OnCreateDirentShareLinkListener() {
            @Override
            public void onCreateDirentShareLink(DirentShareLinkModel linkModel) {
                if (linkModel == null) {
                    dialogFragment.dismiss();
                    return;
                }
                showChooseAppDialog(context, fragmentManager, linkModel, direntModel.isDir());
                dialogFragment.dismiss();
            }
        });
        dialogFragment.show(fragmentManager, GetShareLinkPasswordDialogFragment.class.getSimpleName());
    }


    /**
     * if dir will share dir link .
     * if local file ,will share file to wachat app.
     * if server file , it will download file and share file.
     */
    public static void shareToWeChat(Fragment context, DirentModel direntModel) {
        if (direntModel.isDir()) {
            shareDirToWeChat(context, direntModel);
        } else {
            shareFileToWeChat(context, direntModel);
        }
    }

    /**
     * share link to wechat
     */
    public static void shareDirToWeChat(Fragment context, DirentModel direntModel) {
        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");

        ResolveInfo weChatInfo = Utils.getWeChatIntent(shareIntent);
        if (weChatInfo == null) {
            ToastUtils.showLong(R.string.no_app_available);
            return;
        }

        String className = weChatInfo.activityInfo.name;
        String packageName = weChatInfo.activityInfo.packageName;
        shareIntent.setClassName(packageName, className);

        GetShareLinkPasswordDialogFragment dialogFragment = new GetShareLinkPasswordDialogFragment();
        dialogFragment.init(direntModel.repo_id, direntModel.full_path, false);
        dialogFragment.setOnCreateDirentShareLinkListener(new OnCreateDirentShareLinkListener() {
            @Override
            public void onCreateDirentShareLink(DirentShareLinkModel linkModel) {
                if (linkModel == null) {
                    dialogFragment.dismiss();
                    return;
                }

                shareIntent.putExtra(Intent.EXTRA_TEXT, linkModel.link);
                context.startActivity(shareIntent);
                dialogFragment.dismiss();
            }
        });
        dialogFragment.show(context.getChildFragmentManager(), GetShareLinkPasswordDialogFragment.class.getSimpleName());
    }

    public static void shareFileToWeChat(Fragment context, DirentModel direntModel) {

        File file = Objs.getLocalFile(direntModel.repo_name, direntModel.repo_id, direntModel.full_path);
        Uri uri = FileProvider.getUriForFile(context.requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        ResolveInfo weChatInfo = Utils.getWeChatIntent(sendIntent);
        if (weChatInfo == null) {
            ToastUtils.showLong(R.string.no_app_available);
            return;
        }

        String className = weChatInfo.activityInfo.name;
        String packageName = weChatInfo.activityInfo.packageName;
        sendIntent.setClassName(packageName, className);
        if (!Utils.isNetworkOn() && file.exists()) {
            context.startActivity(sendIntent);
            return;
        }

        fetchFileAndExport(context, sendIntent, direntModel.repo_name, direntModel.repo_id, direntModel.full_path, direntModel.size);
    }

    /**
     * Export a file.
     * 1. first ask the user to choose an app
     * 2. then download the latest version of the file
     * 3. start the choosen app
     */
    public static void exportFile(Fragment context, DirentModel direntModel) {

        final File file = SupportDataManager
                .getInstance()
                .getDataManager()
                .getLocalRepoFile(direntModel.repo_name, direntModel.repo_id, direntModel.full_path);
        Uri uri = FileProvider.getUriForFile(context.requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = Utils.getAppsByIntent(sendIntent);

        if (infos.isEmpty()) {
            ToastUtils.showLong(R.string.no_app_available);
            return;
        }

        AppChoiceDialogFragment dialog = new AppChoiceDialogFragment();
        dialog.init(context.getString(R.string.export_file), infos, new AppChoiceDialogFragment.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(AppChoiceDialogFragment.CustomAction action) {
            }

            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                sendIntent.setClassName(packageName, className);

                if (!Utils.isNetworkOn() && file.exists()) {
                    context.startActivity(sendIntent);
                    return;
                }
                fetchFileAndExport(context, sendIntent, direntModel.repo_name, direntModel.repo_id, direntModel.full_path, direntModel.size);
            }

        });
        dialog.show(context.getChildFragmentManager(), AppChoiceDialogFragment.class.getSimpleName());
    }

    public static void fetchFileAndExport(Fragment context, Intent intent, String repoName, String repoID, String fullPath, long fileSize) {
        FetchFileDialog fetchFileDialog = new FetchFileDialog();
        fetchFileDialog.init(repoName, repoID, fullPath, fileSize, new FetchFileDialog.FetchFileListener() {
            @Override
            public void onSuccess() {
                context.startActivity(intent);
            }

            @Override
            public void onDismiss() {
            }

            @Override
            public void onFailure(SeafException err) {
            }
        });
        fetchFileDialog.show(context.getChildFragmentManager(), FetchFileDialog.class.getSimpleName());
    }
}
