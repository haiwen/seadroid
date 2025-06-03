package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.model.dirents.FileCreateModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
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

        if (TextUtils.equals(oldRepoName, newRepoName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("repo_name", newRepoName);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<String> single = HttpIO.getCurrentInstance().execute(DialogService.class).renameRepo(repoId, bodyMap);
        Single<String> single2 = single.flatMap(new Function<String, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(String result) throws Exception {
                return Single.create(new SingleOnSubscribe<String>() {
                    @Override
                    public void subscribe(SingleEmitter<String> emitter) throws Exception {
                        if (emitter.isDisposed()){
                            return;
                        }

                        File oldRepoFolder = DataManager.getLocalRepoDir(account, repoId, oldRepoName);

                        if (oldRepoFolder.exists()) {
                            File newRepoFolder = DataManager.getLocalRepoDir(account, repoId, newRepoName);
                            Files.move(oldRepoFolder.toPath(), newRepoFolder.toPath(), StandardCopyOption.REPLACE_EXISTING);
                        }

                        //update db
                        AppDatabase.getInstance().fileCacheStatusDAO().updateRepoNameByRepoId(repoId, newRepoName);
                        AppDatabase.getInstance().fileTransferDAO().updateRepoNameByRepoId(repoId, newRepoName);
                        AppDatabase.getInstance().repoDao().updateRepoNameByRepoId(repoId, newRepoName);
                        AppDatabase.getInstance().direntDao().updateRepoNameByRepoId(repoId, newRepoName);
                        AppDatabase.getInstance().starredDirentDAO().updateRepoNameByRepoId(repoId, newRepoName);

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
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void renameDir(Account account, String repoId, String repoName, String oldFolderFullPath, String oldName, String newName) {
        if (TextUtils.equals(oldName, newName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "rename");
        requestDataMap.put("newname", newName);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<String> single = HttpIO.getCurrentInstance().execute(DialogService.class).renameDir(repoId, oldFolderFullPath, bodyMap);
        Single<String> single2 = single.flatMap(new Function<String, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(String result) throws Exception {
                return Single.create(new SingleOnSubscribe<String>() {
                    @Override
                    public void subscribe(SingleEmitter<String> emitter) throws Exception {
                        if (emitter.isDisposed()){
                            return;
                        }

                        String parentPath = Utils.getParentPath(oldFolderFullPath);
                        String newFolderFullPath = Utils.pathJoin(parentPath, newName);

                        File srcFile = DataManager.getLocalFileCachePath(account, repoId, repoName, oldFolderFullPath);
                        File dstFile = DataManager.getLocalFileCachePath(account, repoId, repoName, newFolderFullPath);
                        Path srcPath = srcFile.toPath();
                        if (srcFile.exists()) {
                            boolean r = srcFile.renameTo(dstFile);
                            SLogs.e("rename " + oldName + " to " + newName + ": result: " + r);
                        }

                        String o = oldFolderFullPath;
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
                            direntModel.name = newName;
                            direntModel.uid = direntModel.getUID();

                            AppDatabase.getInstance().direntDao().insert(direntModel);
                        }

                        String oo = oldFolderFullPath;
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
                                cacheEntity.target_path = cacheEntity.target_path.replace(oldFolderFullPath, newFolderFullPath);
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
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void renameFile(Account account, String repoId, String repoName, String oldFullPath, String oldName, String newName) {

        if (TextUtils.equals(oldName, newName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "rename");
        requestDataMap.put("newname", newName);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<FileCreateModel> single = HttpIO.getCurrentInstance().execute(DialogService.class).renameFile(repoId, oldFullPath, bodyMap);
        Single<FileCreateModel> single2 = single.flatMap(new Function<FileCreateModel, SingleSource<FileCreateModel>>() {
            @Override
            public SingleSource<FileCreateModel> apply(FileCreateModel fileCreateModel) throws Exception {
                return Single.create(new SingleOnSubscribe<FileCreateModel>() {
                    @Override
                    public void subscribe(SingleEmitter<FileCreateModel> emitter) throws IOException {
                        if (emitter.isDisposed()){
                            return;
                        }

                        String parentPath = oldFullPath.replace(oldName, "");
                        if (TextUtils.isEmpty(parentPath)) {
                            parentPath = "/";
                        }

                        String newFullPath = Utils.pathJoin(parentPath, newName);

                        File srcFile = DataManager.getLocalFileCachePath(account, repoId, repoName, oldFullPath);
                        File dstFile = DataManager.getLocalFileCachePath(account, repoId, repoName, newFullPath);
                        Path srcPath = srcFile.toPath();
                        if (srcFile.exists()) {
                            java.nio.file.Files.move(srcPath, srcPath.resolveSibling(newName), StandardCopyOption.REPLACE_EXISTING);
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
                            cacheEntity.file_name = newName;
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
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }
}
