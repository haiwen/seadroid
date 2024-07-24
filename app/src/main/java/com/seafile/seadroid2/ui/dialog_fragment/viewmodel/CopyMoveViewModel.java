package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class CopyMoveViewModel extends BaseViewModel {
    private final MutableLiveData<ResultModel> ResultLiveData = new MutableLiveData<>();

    public MutableLiveData<ResultModel> getResultLiveData() {
        return ResultLiveData;
    }

    public void move(String dstParentDir, String dstRepoId, String srcParentDir, String srcRepoId, List<DirentModel> list) {
        getRefreshLiveData().setValue(true);

        List<String> nameList = list.stream().map(m -> m.name).collect(Collectors.toList());

        Map<String, Object> requestDataMap = new HashMap<>();
        requestDataMap.put("dst_parent_dir", dstParentDir);
        requestDataMap.put("dst_repo_id", dstRepoId);
        requestDataMap.put("src_parent_dir", srcParentDir);
        requestDataMap.put("src_repo_id", srcRepoId);
        requestDataMap.put("src_dirents", nameList);

        Single<ResultModel> single = HttpIO.getCurrentInstance().execute(DialogService.class).moveDirents(requestDataMap);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);
                getResultLiveData().setValue(resultModel);
            }
        });
    }

    public void copy(String dstParentDir, String dstRepoId, String srcParentDir, String srcRepoId, List<DirentModel> list) {
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
        });
    }
}
