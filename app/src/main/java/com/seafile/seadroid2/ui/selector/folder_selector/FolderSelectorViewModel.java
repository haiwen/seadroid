package com.seafile.seadroid2.ui.selector.folder_selector;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.checkbox.MaterialCheckBox;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.util.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;

public class FolderSelectorViewModel extends BaseViewModel {
    private final MutableLiveData<List<FileBean>> dataListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<FileBean>> getLocalFileListLiveData() {
        return dataListLiveData;
    }

    public void setSelectFilePathList(List<String> selectFilePath) {
        this.selectFilePath = selectFilePath;
    }

    public List<String> getSelectFilePathList() {
        return selectFilePath;
    }

    private List<String> selectFilePath = new ArrayList<>();

    public void removeSpecialPath(String filePath) {
        selectFilePath.removeIf(s -> TextUtils.equals(s, filePath));
    }

    public void addSpecialPath(String filePath) {
        selectFilePath.add(filePath);
    }

    public void loadLocalFileData(String path) {
        loadLocalFileData(path, Collections.emptyList());
    }

    public void loadLocalFileData(String path, List<String> filterPaths) {

        getRefreshLiveData().setValue(true);
        boolean isSkipHiddenFile = FolderBackupSharePreferenceHelper.isFolderBackupSkipHiddenFiles();

        Single<List<FileBean>> single = Single.create(new SingleOnSubscribe<List<FileBean>>() {
            @Override
            public void subscribe(SingleEmitter<List<FileBean>> emitter) throws Exception {
                if (emitter == null || emitter.isDisposed()) {
                    return;
                }

                File file = FileUtils.getFileByPath(path);
                if (file == null) {
                    emitter.onSuccess(Collections.emptyList());
                    return;
                }

                File[] files = file.listFiles();
                if (files == null) {
                    emitter.onSuccess(Collections.emptyList());
                    return;
                }

                List<FileBean> fileBeanList = new ArrayList<>();

                for (File value : files) {
                    if (!CollectionUtils.isEmpty(filterPaths)) {
                        boolean isFilter = false;
                        for (String filterPath : filterPaths) {
                            if (value.getAbsolutePath().startsWith(filterPath)) {
                                isFilter = true;
                                break;
                            }
                        }
                        if (isFilter) {
                            continue;
                        }
                    }

                    FileBean fileBean = new FileBean(value);
                    if (isSkipHiddenFile && value.isHidden()) {
                        continue;
                    }

                    int checkState = checkIsInBackupPathList(value.getAbsolutePath());
                    fileBean.setCheckedState(checkState);

                    fileBeanList.add(fileBean);
                }

                //sort
                sortFileBeanList(fileBeanList, Constants.SORT_NAME_ASC);

                emitter.onSuccess(fileBeanList);
            }
        });

        addSingleDisposable(single, new Consumer<List<FileBean>>() {
            @Override
            public void accept(List<FileBean> fileBeans) throws Exception {
                getRefreshLiveData().setValue(false);
                getLocalFileListLiveData().setValue(fileBeans);
            }
        });
    }

    //prepare: FILE SYNC FEAT
    private int checkIsInBackupPathList(String curPath) {

        if (CollectionUtils.isEmpty(getSelectFilePathList())) {
            return MaterialCheckBox.STATE_UNCHECKED;
        }

        for (String backupPath : getSelectFilePathList()) {
            if (curPath.equals(backupPath)) {
                return MaterialCheckBox.STATE_CHECKED;
            }

//            if (curPath.startsWith(backupPath)) {
//                return MaterialCheckBox.STATE_CHECKED;
//            }

//            if (backupPath.startsWith(curPath)) {
//                return MaterialCheckBox.STATE_INDETERMINATE;
//            }
        }

        // backup: /storage/emulated/0/Downloads
        return MaterialCheckBox.STATE_UNCHECKED;

    }


    private void sortFileBeanList(List<FileBean> fileBeanList, int sortType) {
        Collections.sort(fileBeanList, (file1, file2) -> {

            if (file1.isDir() && !file2.isDir())
                return -1;
            if (!file1.isDir() && file2.isDir())
                return 1;

            switch (sortType) {
                case Constants.SORT_NAME_ASC:
                    return file1.getFileName().compareToIgnoreCase(file2.getFileName());
                case Constants.SORT_NAME_DESC:
                    return file2.getFileName().compareToIgnoreCase(file1.getFileName());
                case Constants.SORT_TIME_ASC:
                    long diff = file1.getModifyTime() - file2.getModifyTime();
                    if (diff > 0)
                        return 1;
                    else if (diff == 0)
                        return 0;
                    else
                        return -1;
                case Constants.SORT_TIME_DESC:
                    diff = file2.getModifyTime() - file1.getModifyTime();
                    if (diff > 0)
                        return 1;
                    else if (diff == 0)
                        return 0;
                    else
                        return -1;
                case Constants.SORT_SIZE_ASC:
                    diff = file1.getSimpleSize() - file2.getSimpleSize();
                    if (diff > 0)
                        return 1;
                    else if (diff == 0)
                        return 0;
                    else
                        return -1;
                case Constants.SORT_SIZE_DESC:
                    diff = file2.getSimpleSize() - file1.getSimpleSize();
                    if (diff > 0)
                        return 1;
                    else if (diff == 0)
                        return 0;
                    else
                        return -1;
                default:
                    return 0;
            }
        });
    }
}
