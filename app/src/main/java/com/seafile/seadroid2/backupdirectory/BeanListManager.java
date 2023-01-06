package com.seafile.seadroid2.backupdirectory;


import android.app.Activity;
import android.app.ProgressDialog;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.util.Utils;

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

    public static final int TypeAddTabbar = 0;
    public static final int TypeDelTabbar = 1;
    public static final int TypeInitTabbar = 2;

    public static void upDataFileBeanListByAsyn(Activity at, List<String> selectFilePath, List<FileBean> fileBeanList, FileListAdapter fileListAdapter, String path, List<String> fileTypes, int sortType) {

        if (fileBeanList == null) {
            fileBeanList = new ArrayList<>();
        } else if (fileBeanList.size() != 0) {
            fileBeanList.clear();
        }

        Observable
                .just(fileBeanList)
                .map(new Function<List<FileBean>, List<FileBean>>() {
                    @Override
                    public List<FileBean> apply(List<FileBean> fileBeanList) throws Throwable {
                        FileBean fileBean;
                        File file = FileTools.getFileByPath(path);
                        File[] files = file.listFiles();
                        if (files != null) {
                            for (int i = 0; i < files.length; i++) {
                                fileBean = new FileBean(files[i].getAbsolutePath(), false);
                                if (selectFilePath != null && selectFilePath.size() > 0) {
                                    for (String str : selectFilePath) {
                                        if (fileBean.getFilePath().equals(str)) {
                                            fileBean.setChecked(true);
                                        }
                                    }
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
                    public void onNext(@NonNull List<FileBean> fileBeanList) {
                        if (fileListAdapter != null) {
                            fileListAdapter.updateListData(fileBeanList);
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
            public int compare(FileBean o1, FileBean o2) {

                if (o1.isDir() && o2.isFile())
                    return -1;
                if (o1.isFile() && o2.isDir())
                    return 1;

                switch (sortType) {
                    case Constants.SORT_NAME_ASC:
                        return o1.getFileName().compareToIgnoreCase(o2.getFileName());
                    case Constants.SORT_NAME_DESC:
                        return o2.getFileName().compareToIgnoreCase(o1.getFileName());
                    case Constants.SORT_TIME_ASC:
                        long diff = o1.getModifyTime() - o2.getModifyTime();
                        if (diff > 0)
                            return 1;
                        else if (diff == 0)
                            return 0;
                        else
                            return -1;
                    case Constants.SORT_TIME_DESC:
                        diff = o2.getModifyTime() - o1.getModifyTime();
                        if (diff > 0)
                            return 1;
                        else if (diff == 0)
                            return 0;
                        else
                            return -1;
                    case Constants.SORT_SIZE_ASC:
                        diff = o1.getSimpleSize() - o2.getSimpleSize();
                        if (diff > 0)
                            return 1;
                        else if (diff == 0)
                            return 0;
                        else
                            return -1;
                    case Constants.SORT_SIZE_DESC:
                        diff = o2.getSimpleSize() - o1.getSimpleSize();
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

    public static void getTabbarFileBeanList(List<TabbarFileBean> tabbarList, String path, List<String> SdCardList) {
        if (SdCardList.contains(path)) {
            int i = SdCardList.indexOf(path);
            if (i == 0) {
                tabbarList.add(0, new TabbarFileBean(path, SeadroidApplication.getAppContext().getString(R.string.internal_storage), false, null));
            } else if (i > 0) {
                tabbarList.add(0, new TabbarFileBean(path, String.format("SD%d", i), false, null));
            } else {
                tabbarList.add(0, new TabbarFileBean(path, SeadroidApplication.getAppContext().getString(R.string.internal_storage_err), false, null));
            }
            return;
        }
        tabbarList.add(0, new TabbarFileBean(path, false));
        getTabbarFileBeanList(tabbarList, FileTools.getParentPath(path), SdCardList);
    }


    public static List<TabbarFileBean> upDataTabbarFileBeanList(List<TabbarFileBean> tabbarList, TabbarFileListAdapter tabbarAdapter, String path, int type, List<String> SdCardList) {
        switch (type) {
            case TypeAddTabbar:
                tabbarList.add(new TabbarFileBean(path, false));
                break;
            case TypeDelTabbar:
                for (int i = tabbarList.size() - 1; i >= 0; i--) {
                    if (tabbarList.get(i).getFilePath().length() > path.length()) {
                        tabbarList.remove(i);
                    } else {
                        break;
                    }
                }
                break;
            case TypeInitTabbar:
                if (tabbarList == null) {
                    tabbarList = new ArrayList<>();
                } else {
                    tabbarList.clear();
                }
                getTabbarFileBeanList(tabbarList, path, SdCardList);
                break;
        }

        if (tabbarAdapter != null) {
            tabbarAdapter.updateListData(tabbarList);
            tabbarAdapter.notifyDataSetChanged();
        }

        return tabbarList;
    }

}

