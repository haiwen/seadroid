package com.seafile.seadroid2.backupdirectory;


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

    public static void clearList(List list) {
        if (list != null && list.size() != 0) {
            list.clear();
        }
    }

    public static void setCheckList(List<FileBean> list, boolean var) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).setChecked(var);
        }
    }

    public static void upDataFileBeanListByAsyn(List<FileBean> fileBeanList, FileListAdapter fileListAdapter, String path, List<String> fileTypes, int sortType) {

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
                                if (fileTypes == null || fileTypes.size() == 0 || fileBean.isDir() || fileTypes.contains(fileBean.getFileExtension())) {
                                    fileBeanList.add(fileBean);
                                }
                            }
                        }
                        sortFileBeanList(fileBeanList, sortType);
                        return fileBeanList;
                    }
                })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Observer<List<FileBean>>() {
                    @Override
                    public void onSubscribe(@NonNull Disposable d) {

                    }

                    @Override
                    public void onNext(@NonNull List<FileBean> fileBeanList) {
                        if (fileListAdapter != null) {
                            fileListAdapter.updateListData(fileBeanList);
                            fileListAdapter.notifyDataSetChanged();
                            if (fileBeanList.size() == 0) {
//                                fileListAdapter.setEmptyView(R.layout.account_detail);
                            }
                        }
                    }

                    @Override
                    public void onError(@NonNull Throwable e) {

                    }

                    @Override
                    public void onComplete() {

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
                tabbarList.add(0, new TabbarFileBean(path, "内部存储", false, null));
            } else if (i > 0) {
                tabbarList.add(0, new TabbarFileBean(path, String.format("SD%d", i), false, null));
            } else {
                tabbarList.add(0, new TabbarFileBean(path, "错误163", false, null));
            }
            return;
        }
        tabbarList.add(0, new TabbarFileBean(path, false));
        getTabbarFileBeanList(tabbarList, FileTools.getParentPath(path), SdCardList);
    }

    public static void upDataTabbarFileBeanListByAsyn(List<TabbarFileBean> tabbarList, TabbarFileListAdapter tabbarAdapter, String path, int type, List<String> SdCardList) {

        Observable
                .just(tabbarList)
                .map(new Function<List<TabbarFileBean>, List<TabbarFileBean>>() {
                    @Override
                    public List<TabbarFileBean> apply(List<TabbarFileBean> tabbarLists) throws Throwable {
                        switch (type) {
                            case TypeAddTabbar:
                                tabbarLists.add(new TabbarFileBean(path, false));
                                break;
                            case TypeDelTabbar:
                                for (int i = tabbarLists.size() - 1; i >= 0; i--) {
                                    if (tabbarLists.get(i).getFilePath().length() > path.length()) {
                                        tabbarLists.remove(i);
                                    } else {
                                        break;
                                    }
                                }
                                break;
                            case TypeInitTabbar:
                                if (tabbarLists == null) {
                                    tabbarLists = new ArrayList<>();
                                    getTabbarFileBeanList(tabbarLists, path, SdCardList);
                                } else {
                                    tabbarLists.clear();
                                    getTabbarFileBeanList(tabbarLists, path, SdCardList);
                                }
                                break;
                        }

                        return null;
                    }
                })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Observer<List<TabbarFileBean>>() {
                    @Override
                    public void onSubscribe(@NonNull Disposable d) {

                    }

                    @Override
                    public void onNext(@NonNull List<TabbarFileBean> tabbarFileBeanList) {
                        if (tabbarAdapter != null) {
                            tabbarAdapter.updateListData(tabbarFileBeanList);
                            tabbarAdapter.notifyDataSetChanged();
                        }
                    }

                    @Override
                    public void onError(@NonNull Throwable e) {

                    }

                    @Override
                    public void onComplete() {

                    }
                });

    }

    public static List<TabbarFileBean> upDataTabbarFileBeanList(List<TabbarFileBean> tabbarList, TabbarFileListAdapter tabbarAdapter, String path, int type, List<String> SdCardList) {
        switch (type) {
            case TypeAddTabbar:
                tabbarList.add(new TabbarFileBean(path, false));
                break;
            case TypeDelTabbar:
                for (int i = tabbarList.size() - 1; i >= 0; i--) {
                    if (tabbarList.get(i).getFilePath().length() > path.length()) {//移除比当前路径还长的数据
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

    public static List<String> getCallBackData(List<FileBean> fileBeanList) {
        if (fileBeanList == null) {
            return null;
        }
        List<String> data = new ArrayList<>();
        for (int i = 0; i < fileBeanList.size(); i++) {
            if (fileBeanList.get(i).isChecked()) {
                data.add(fileBeanList.get(i).getFilePath());
            }
        }
        return data;
    }

    public static List<FileBean> getCallBackFileBeanList(List<FileBean> fileBeanList) {
        if (fileBeanList == null) {
            return null;
        }
        List<FileBean> data = new ArrayList<>();
        for (int i = 0; i < fileBeanList.size(); i++) {
            if (fileBeanList.get(i).isChecked()) {
                data.add(fileBeanList.get(i));
            }
        }
        return data;
    }

    public static void setCheckBoxVisible(List<FileBean> fileBeanList, FileListAdapter fileListAdapter, boolean state) {
        if (fileBeanList == null || fileListAdapter == null) {
            return;
        }
        if (state) {
            for (int i = 0; i < fileBeanList.size(); i++) {
                fileBeanList.get(i).setVisible(true);
            }
        } else {
            for (int i = 0; i < fileBeanList.size(); i++) {
                fileBeanList.get(i).setVisible(false);
                fileBeanList.get(i).setChecked(false);
            }
        }

        fileListAdapter.updateListData(fileBeanList);
        fileListAdapter.notifyDataSetChanged();
    }


}

