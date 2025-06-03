package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.io.File;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;

public class CopyMoveViewModel extends BaseViewModel {
    private final MutableLiveData<ResultModel> ResultLiveData = new MutableLiveData<>();

    public MutableLiveData<ResultModel> getResultLiveData() {
        return ResultLiveData;
    }

    public void move(Account account, String dstParentDir, String dstRepoId, String dstRepoName, String srcParentDir, String srcRepoId, String srcRepoName, List<DirentModel> direntList) {
        if (CollectionUtils.isEmpty(direntList)) {
            return;
        }
        getRefreshLiveData().setValue(true);

        List<String> nameList = direntList.stream().map(m -> m.name).collect(Collectors.toList());

        Map<String, Object> requestDataMap = new HashMap<>();
        requestDataMap.put("dst_parent_dir", dstParentDir);
        requestDataMap.put("dst_repo_id", dstRepoId);
        requestDataMap.put("src_parent_dir", srcParentDir);
        requestDataMap.put("src_repo_id", srcRepoId);
        requestDataMap.put("src_dirents", nameList);

        Single<ResultModel> netSingle = HttpIO.getCurrentInstance().execute(DialogService.class).moveDirents(requestDataMap);
        Single<ResultModel> moveSingle = Single.create(new SingleOnSubscribe<ResultModel>() {
            @Override
            public void subscribe(SingleEmitter<ResultModel> emitter) throws Exception {
                if (emitter.isDisposed()){
                    return;
                }

                for (DirentModel direntModel : direntList) {
                    String srcFullPath = Utils.pathJoin(srcParentDir, direntModel.name);
                    String dstFullPath = Utils.pathJoin(dstParentDir, direntModel.name);

                    File srcFile = DataManager.getLocalFileCachePath(account, srcRepoId, srcRepoName, srcFullPath);
                    File dstFile = DataManager.getLocalFileCachePath(account, dstRepoId, dstRepoName, dstFullPath);
                    if (java.nio.file.Files.exists(srcFile.toPath())) {
                        Path path = java.nio.file.Files.move(srcFile.toPath(), dstFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                        SLogs.d("move dirent:" + path.toString());
                    } else {
                        SLogs.e("move dirent failed, because src file not exists: " + srcFile.getAbsolutePath());
                    }

                    if (direntModel.isDir()) {
                        String p = srcFullPath;
                        if (!p.endsWith("/")) {
                            p += "/";
                        }

                        List<FileCacheStatusEntity> cacheList = AppDatabase.getInstance().fileCacheStatusDAO().getByParentPathStartsWith(srcRepoId, p);
                        if (CollectionUtils.isNotEmpty(cacheList)) {
                            cacheList.forEach(cacheEntity -> {
                                //delete
                                AppDatabase.getInstance().fileCacheStatusDAO().delete(cacheEntity);

                                cacheEntity.repo_id = dstRepoId;
                                cacheEntity.repo_name = dstRepoName;
                                cacheEntity.target_path = cacheEntity.target_path.replace(srcFile.getAbsolutePath(), dstFile.getAbsolutePath());
                                cacheEntity.full_path = cacheEntity.full_path.replaceFirst(srcFullPath, dstFullPath);

                                String s = cacheEntity.getParent_path().replaceFirst(srcFullPath, dstFullPath);
                                cacheEntity.setParent_path(s);
                                cacheEntity.uid = cacheEntity.genUID();

                                //insert
                                AppDatabase.getInstance().fileCacheStatusDAO().insert(cacheEntity);
                            });

                        }

                    } else {
                        List<FileCacheStatusEntity> cacheList = AppDatabase.getInstance().fileCacheStatusDAO().getByTargetPathSync(account.getSignature(), srcFile.getAbsolutePath());
                        if (CollectionUtils.isNotEmpty(cacheList)) {
                            FileCacheStatusEntity cacheEntity = cacheList.get(0);
                            //delete
                            AppDatabase.getInstance().fileCacheStatusDAO().delete(cacheEntity);

                            cacheEntity.target_path = dstFile.getAbsolutePath();
                            cacheEntity.repo_id = dstRepoId;
                            cacheEntity.repo_name = dstRepoName;
                            cacheEntity.full_path = Utils.pathJoin(dstParentDir, direntModel.name);
                            cacheEntity.setParent_path(Utils.getParentPath(cacheEntity.full_path));
                            cacheEntity.uid = cacheEntity.genUID();

                            //insert
                            AppDatabase.getInstance().fileCacheStatusDAO().insert(cacheEntity);
                        }
                    }

                }

                ResultModel resultModel = new ResultModel();
                resultModel.success = true;

                emitter.onSuccess(resultModel);
            }
        });

        // chain
        Single<ResultModel> chainSingle = netSingle.flatMap(new Function<ResultModel, SingleSource<ResultModel>>() {
            @Override
            public SingleSource<ResultModel> apply(ResultModel resultModel) throws Exception {
                return moveSingle;
            }
        });

        addSingleDisposable(chainSingle, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);
                getResultLiveData().setValue(resultModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    public void copy(Account account, String dstParentDir, String dstRepoId, String dstRepoName, String srcParentDir, String srcRepoId, String srcRepoName, List<DirentModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        List<String> nameList = list.stream().map(m -> m.name).collect(Collectors.toList());

        Map<String, Object> requestDataMap = new HashMap<>();
        requestDataMap.put("dst_parent_dir", dstParentDir);
        requestDataMap.put("dst_repo_id", dstRepoId);
        requestDataMap.put("src_parent_dir", srcParentDir);
        requestDataMap.put("src_repo_id", srcRepoId);
        requestDataMap.put("src_dirents", nameList);

        Single<ResultModel> single = HttpIO.getCurrentInstance().execute(DialogService.class).copyDirents(requestDataMap);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);
                getResultLiveData().setValue(resultModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }
}
