package com.seafile.seadroid2.folderbackup.selectfolder;

import android.app.Activity;
import android.app.ProgressDialog;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.core.Observer;
import io.reactivex.rxjava3.disposables.Disposable;
import io.reactivex.rxjava3.functions.Function;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BeanListManager {
    public static final int TYPE_ADD_TAB_BAR = 0;
    public static final int TYPE_DEL_TAB_BAR = 1;
    public static final int TYPE_INIT_TAB_BAR = 2;

    public static void upDataFileBeanListByAsyn(Activity at, List<String> selectFilePath,
                                                List<FileBean> fileBeanList, FileListAdapter fileListAdapter,
                                                String path, List<String> fileTypes, int sortType) {

        if (fileBeanList == null) {
            fileBeanList = new ArrayList<>();
        } else if (fileBeanList.size() != 0) {
            fileBeanList.clear();
        }

        Observable.just(fileBeanList).map(new Function<List<FileBean>, List<FileBean>>() {
                    @Override
                    public List<FileBean> apply(List<FileBean> fileBeanList) throws Throwable {
                        FileBean fileBean;
                        File file = FileTools.getFileByPath(path);
                        File[] files = file.listFiles();
                        if (files != null) {
                            for (File value : files) {
                                fileBean = new FileBean(value.getAbsolutePath());
                                if (selectFilePath != null && selectFilePath.size() > 0) {
                                    if (selectFilePath.contains(fileBean.getFilePath())) {
                                        fileBean.setChecked(true);
                                    }
//                            for (String str : selectFilePath) {
//                                if (fileBean.getFilePath().equals(str)) {
//                                    fileBean.setChecked(true);
//                                }
//                            }
                                }
                                fileBeanList.add(fileBean);
                            }
                        }
                        sortFileBeanList(fileBeanList, sortType);
                        return fileBeanList;
                    }
                })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Observer<List<FileBean>>() {
                    public ProgressDialog pg;

                    @Override
                    public void onSubscribe(@NonNull Disposable d) {
                        pg = new ProgressDialog(at);
                        pg.show();
                    }

                    @Override
                    public void onNext(@NonNull List<FileBean> fileBeans) {
                        if (fileListAdapter != null) {
                            fileListAdapter.updateListData(fileBeans);
                            fileListAdapter.notifyDataSetChanged();
                        }
                    }

                    @Override
                    public void onError(@NonNull Throwable e) {
                        pg.dismiss();
                    }

                    @Override
                    public void onComplete() {
                        pg.dismiss();
                    }
                });
    }

    public static void sortFileBeanList(List<FileBean> fileBeanList, int sortType) {
        Collections.sort(fileBeanList, new Comparator<FileBean>() {
            @Override
            public int compare(FileBean file1, FileBean file2) {

                if (file1.isDir() && file2.isFile())
                    return -1;
                if (file1.isFile() && file2.isDir())
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
            }
        });
    }

    public static void getTabbarFileBeanList(List<TabBarFileBean> tabbarList,
                                             String path, List<String> allPathsList) {
        if (allPathsList.contains(path)) {
            tabbarList.add(0, new TabBarFileBean(path,
                    SeadroidApplication.getAppContext().getString(R.string.internal_storage)));
            return;
        }
    }

    public static List<TabBarFileBean> upDataTabbarFileBeanList(List<TabBarFileBean> tabbarList,
                                                                TabBarFileListAdapter tabbarAdapter,
                                                                String path, int type, List<String> allPathsList) {
        switch (type) {
            case TYPE_ADD_TAB_BAR:
                tabbarList.add(new TabBarFileBean(path));
                break;
            case TYPE_DEL_TAB_BAR:
                for (int i = tabbarList.size() - 1; i >= 0; i--) {
                    if (tabbarList.get(i).getFilePath().length() > path.length()) {
                        tabbarList.remove(i);
                    } else {
                        break;
                    }
                }
                break;
            case TYPE_INIT_TAB_BAR:
                if (tabbarList == null) {
                    tabbarList = new ArrayList<>();
                } else {
                    tabbarList.clear();
                }
                getTabbarFileBeanList(tabbarList, path, allPathsList);
                break;
        }
        if (tabbarAdapter != null) {
            tabbarAdapter.updateListData(tabbarList);
            tabbarAdapter.notifyDataSetChanged();
        }
        return tabbarList;
    }
}

