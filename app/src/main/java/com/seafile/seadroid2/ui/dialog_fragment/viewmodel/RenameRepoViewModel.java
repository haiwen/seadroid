package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.dirents.FileCreateModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.UnicodePathUtils;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import okhttp3.RequestBody;

public class RenameRepoViewModel extends BaseViewModel {
    private final MutableLiveData<String> actionLiveData = new MutableLiveData<>();
    public MutableLiveData<String> getActionLiveData() {
        return actionLiveData;
    }

    public void renameRepo(Account account, String oldRepoName, String newRepoName, String repoId) {
        final String normalizedOldRepoName = UnicodePathUtils.normalize(oldRepoName);
        final String normalizedNewRepoName = UnicodePathUtils.normalize(newRepoName);

        if (TextUtils.equals(normalizedOldRepoName, normalizedNewRepoName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("repo_name", normalizedNewRepoName);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<String> single = HttpManager.getCurrentHttp().execute(DialogService.class).renameRepo(repoId, bodyMap);
        Single<String> single2 = single.flatMap(new Function<String, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(String result) throws Exception {
                return Single.create(new SingleOnSubscribe<String>() {
                    @Override
                    public void subscribe(SingleEmitter<String> emitter) throws Exception {
                        if (emitter == null || emitter.isDisposed()) {
                            return;
                        }

                        File oldRepoFolder = DataManager.getLocalRepoDir(account, repoId, normalizedOldRepoName);

                        if (oldRepoFolder.exists()) {
                            File newRepoFolder = DataManager.getLocalRepoDir(account, repoId, normalizedNewRepoName);
                            Files.move(oldRepoFolder.toPath(), newRepoFolder.toPath(), StandardCopyOption.REPLACE_EXISTING);
                        }

                        //update db
                        AppDatabase.getInstance().fileCacheStatusDAO().updateRepoNameByRepoId(repoId, normalizedNewRepoName);
                        AppDatabase.getInstance().fileTransferDAO().updateRepoNameByRepoId(repoId, normalizedNewRepoName);
                        AppDatabase.getInstance().repoDao().updateRepoNameByRepoId(repoId, normalizedNewRepoName);
                        AppDatabase.getInstance().direntDao().updateRepoNameByRepoId(repoId, normalizedNewRepoName);
                        AppDatabase.getInstance().starredDirentDAO().updateRepoNameByRepoId(repoId, normalizedNewRepoName);

                        emitter.onSuccess(result);
                    }
                });
            }
        });

        addSingleDisposable(single2, new Consumer<String>() {
            @Override
            public void accept(String resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getActionLiveData().setValue(resultModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void renameDir(Account account, String repoId, String repoName, String oldFolderFullPath, String oldName, String newName) {
        final String normalizedRepoName = UnicodePathUtils.normalize(repoName);
        final String normalizedOldFolderFullPath = UnicodePathUtils.normalize(oldFolderFullPath);
        final String normalizedOldName = UnicodePathUtils.normalize(oldName);
        final String normalizedNewName = UnicodePathUtils.normalize(newName);

        if (TextUtils.equals(normalizedOldName, normalizedNewName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "rename");
        requestDataMap.put("newname", normalizedNewName);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<String> single = HttpManager.getCurrentHttp().execute(DialogService.class).renameDir(repoId, normalizedOldFolderFullPath, bodyMap);
        Single<String> single2 = single.flatMap(new Function<String, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(String result) throws Exception {
                return Single.create(new SingleOnSubscribe<String>() {
                    @Override
                    public void subscribe(SingleEmitter<String> emitter) throws Exception {
                        if (emitter == null || emitter.isDisposed()) {
                            return;
                        }

                        String parentPath = Utils.getParentPath(normalizedOldFolderFullPath);
                        String newFolderFullPath = Utils.pathJoin(parentPath, normalizedNewName);

                        File srcFile = DataManager.getLocalFileCachePath(account, repoId, normalizedRepoName, normalizedOldFolderFullPath);
                        File dstFile = DataManager.getLocalFileCachePath(account, repoId, normalizedRepoName, newFolderFullPath);
                        Path srcPath = srcFile.toPath();
                        if (srcFile.exists()) {
                            boolean r = srcFile.renameTo(dstFile);
                            SLogs.e("rename " + normalizedOldName + " to " + normalizedNewName + ": result: " + r);
                        }

                        String o = normalizedOldFolderFullPath;
                        if (o.endsWith("/")) {
                            o = o.substring(0, o.length() - 1);
                        }

                        //update db
                        List<DirentModel> specialDirentList = AppDatabase.getInstance().direntDao().getSpecialDirent(repoId, o, "dir");
                        if (CollectionUtils.isNotEmpty(specialDirentList)) {
                            DirentModel direntModel = specialDirentList.get(0);
                            AppDatabase.getInstance().direntDao().delete(direntModel);

                            String n = newFolderFullPath;
                            if (n.endsWith("/")) {
                                n = o.substring(0, o.length() - 1);
                            }
                            direntModel.full_path = n;
                            direntModel.name = normalizedNewName;
                            direntModel.uid = direntModel.getUID();

                            AppDatabase.getInstance().direntDao().insert(direntModel);
                        }

                        String oo = normalizedOldFolderFullPath;
                        if (!oo.endsWith("/")) {
                            oo = oo + "/";
                        }
                        List<DirentModel> direntList = AppDatabase.getInstance().direntDao().getListByParentPath(repoId, oo);
                        if (CollectionUtils.isNotEmpty(direntList)) {
                            for (DirentModel direntModel : direntList) {

                                AppDatabase.getInstance().direntDao().delete(direntModel);

                                direntModel.full_path = Utils.pathJoin(newFolderFullPath, direntModel.name);
                                direntModel.parent_dir = Utils.pathJoin(newFolderFullPath, "/");
                                direntModel.uid = direntModel.getUID();
                                AppDatabase.getInstance().direntDao().insert(direntModel);
                            }
                        }

                        List<FileCacheStatusEntity> list = AppDatabase.getInstance().fileCacheStatusDAO().getByParentPathSync(repoId, oo);
                        if (CollectionUtils.isNotEmpty(list)) {
                            for (FileCacheStatusEntity cacheEntity : list) {

                                //delete
                                AppDatabase.getInstance().fileCacheStatusDAO().delete(cacheEntity);

                                //insert
                                cacheEntity.modified_at = System.currentTimeMillis();
                                cacheEntity.setParent_path(newFolderFullPath);
                                cacheEntity.target_path = cacheEntity.target_path.replace(normalizedOldFolderFullPath, newFolderFullPath);
                                cacheEntity.full_path = Utils.pathJoin(newFolderFullPath, cacheEntity.file_name);
                                cacheEntity.uid = cacheEntity.genUID();
                                AppDatabase.getInstance().fileCacheStatusDAO().insert(cacheEntity);
                            }
                        }


                        emitter.onSuccess(result);
                    }
                });
            }
        });

        addSingleDisposable(single2, new Consumer<String>() {
            @Override
            public void accept(String resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getActionLiveData().setValue(resultModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void renameFile(Account account, String repoId, String repoName, String oldFullPath, String oldName, String newName) {
        final String normalizedRepoName = UnicodePathUtils.normalize(repoName);
        final String normalizedOldFullPath = UnicodePathUtils.normalize(oldFullPath);
        final String normalizedOldName = UnicodePathUtils.normalize(oldName);
        final String normalizedNewName = UnicodePathUtils.normalize(newName);

        if (TextUtils.equals(normalizedOldName, normalizedNewName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "rename");
        requestDataMap.put("newname", normalizedNewName);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<FileCreateModel> single = HttpManager.getCurrentHttp().execute(DialogService.class).renameFile(repoId, normalizedOldFullPath, bodyMap);
        Single<FileCreateModel> single2 = single.flatMap(new Function<FileCreateModel, SingleSource<FileCreateModel>>() {
            @Override
            public SingleSource<FileCreateModel> apply(FileCreateModel fileCreateModel) throws Exception {
                return Single.create(new SingleOnSubscribe<FileCreateModel>() {
                    @Override
                    public void subscribe(SingleEmitter<FileCreateModel> emitter) throws IOException {
                        if (emitter == null || emitter.isDisposed()) {
                            return;
                        }

                        String parentPath = normalizedOldFullPath.replace(normalizedOldName, "");
                        if (TextUtils.isEmpty(parentPath)) {
                            parentPath = "/";
                        }

                        String newFullPath = Utils.pathJoin(parentPath, normalizedNewName);

                        File srcFile = DataManager.getLocalFileCachePath(account, repoId, normalizedRepoName, normalizedOldFullPath);
                        File dstFile = DataManager.getLocalFileCachePath(account, repoId, normalizedRepoName, newFullPath);
                        Path srcPath = srcFile.toPath();
                        if (srcFile.exists()) {
                            java.nio.file.Files.move(srcPath, srcPath.resolveSibling(normalizedNewName), StandardCopyOption.REPLACE_EXISTING);
                        }

                        //no update "dirents" db here. because dirent list on the page will refresh automatically.

                        //update "file_cache_status" db here.
                        List<FileCacheStatusEntity> cacheList = AppDatabase.getInstance().fileCacheStatusDAO().getByTargetPathSync(account.getSignature(), srcFile.getAbsolutePath());
                        if (CollectionUtils.isNotEmpty(cacheList)) {
                            FileCacheStatusEntity cacheEntity = cacheList.get(0);

                            //delete
                            AppDatabase.getInstance().fileCacheStatusDAO().delete(cacheEntity);

                            //insert
                            cacheEntity.modified_at = System.currentTimeMillis();
                            cacheEntity.file_name = normalizedNewName;
                            cacheEntity.target_path = dstFile.getAbsolutePath();
                            cacheEntity.full_path = newFullPath;
                            cacheEntity.file_format = FileUtils.getFileExtension(dstFile);
                            cacheEntity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(cacheEntity.file_format);
                            cacheEntity.uid = cacheEntity.genUID();
                            AppDatabase.getInstance().fileCacheStatusDAO().insert(cacheEntity);
                        }

                        emitter.onSuccess(fileCreateModel);
                    }
                });
            }
        });


        addSingleDisposable(single2, new Consumer<FileCreateModel>() {
            @Override
            public void accept(FileCreateModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getActionLiveData().setValue(resultModel.error_msg);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }
}
