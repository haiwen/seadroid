package com.seafile.seadroid2.data.repository;

import com.seafile.seadroid2.data.db.dao.DirentDAO;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.data.remote.api.RepoService;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import io.reactivex.schedulers.Schedulers;

public class DirentsRepository {
    private DirentDAO dao;
    private RepoService service;

    public DirentsRepository(RepoService service, DirentDAO dao) {
        this.service = service;
        this.dao = dao;
    }

    public Single<List<DirentModel>> getDirents(String repoId, String parent_dir) {
        // 先尝试从数据库中获取数据
        Single<List<DirentModel>> localData = dao.getAllByParentPath(repoId, parent_dir)
                .onErrorResumeNext(throwable -> Single.just(Collections.emptyList()));

        // 发起网络请求并保存到数据库
        Single<List<DirentModel>> remoteData = service.getDirents(repoId, parent_dir)
                .map(new Function<DirentWrapperModel, List<DirentModel>>() {
                    @Override
                    public List<DirentModel> apply(DirentWrapperModel direntWrapperModel) throws Exception {
                        return direntWrapperModel.dirent_list;
                    }
                })
                .doOnSuccess(new Consumer<List<DirentModel>>() {
                    @Override
                    public void accept(List<DirentModel> direntModels) throws Exception {

//                        dao.insertAll(dirents)
                    }
                });

        return Single.concat(localData, remoteData).first(Collections.emptyList());
    }

    public void insert(List<DirentModel> dirents) {
        Completable.fromCallable(new Callable<Boolean>() {
                    @Override
                    public Boolean call() throws Exception {
                        dao.insertAll(dirents);
                        return true;
                    }
                }).subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe();
    }

    public void deleteAll() {
        dao.deleteAll();
    }
}
