package com.seafile.seadroid2.ui.selector.folder_selector;

import android.app.Activity;
import android.app.ProgressDialog;
import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.util.FileTools;
import com.seafile.seadroid2.util.sp.SettingsManager;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import io.reactivex.Observable;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;
import io.reactivex.schedulers.Schedulers;


public class BeanListManager {
    public static final int TYPE_ADD_TAB_BAR = 0;
    public static final int TYPE_DEL_TAB_BAR = 1;
    public static final int TYPE_INIT_TAB_BAR = 2;

    public static void upDataFileBeanListByAsyn(Activity at, List<String> selectFilePath,
                                                List<FileBean> fileBeanList, FileListAdapter fileListAdapter,
                                                String path) {

        if (fileBeanList == null) {
            fileBeanList = new ArrayList<>();
        } else if (!fileBeanList.isEmpty()) {
            fileBeanList.clear();
        }

        Observable.just(fileBeanList).map(new Function<List<FileBean>, List<FileBean>>() {
                    @Override
                    public List<FileBean> apply(List<FileBean> fileBeanList) {

                        boolean isJumpHiddenFile = SettingsManager.getInstance().isFolderBackupJumpHiddenFiles();


                        File file = FileTools.getFileByPath(path);
                        File[] files = file.listFiles();
                        if (files != null) {
                            for (File value : files) {
                                FileBean fileBean = new FileBean(value.getAbsolutePath());

                                boolean isJump = false;
                                if (isJumpHiddenFile) {
                                    String fileName = fileBean.getFileName();
                                    if (!TextUtils.isEmpty(fileName) && fileName.startsWith(".")) {
                                        isJump = true;
                                    }
                                }

                                if (!isJump) {
                                    if (selectFilePath != null && selectFilePath.size() > 0) {
                                        if (selectFilePath.contains(fileBean.getFilePath())) {
                                            fileBean.setChecked(true);
                                        }
                                    }
                                    fileBeanList.add(fileBean);
                                }
                            }
                        }
//                        sortFileBeanList(fileBeanList, sortType);
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
//                            fileListAdapter.updateListData(fileBeans);
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

