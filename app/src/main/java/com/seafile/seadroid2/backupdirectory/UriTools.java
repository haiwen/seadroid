package com.seafile.seadroid2.backupdirectory;

import android.annotation.SuppressLint;
import android.app.Application;
import android.content.ContentResolver;
import android.content.ContentUris;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.os.storage.StorageManager;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import android.support.v4.provider.DocumentFile;
import android.text.TextUtils;
import android.util.Log;


import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;


public class UriTools {
    //根目录 :一般是/storage/emulated/0
    public static final String URI_ROOT = "content://com.android.externalstorage.documents/tree/primary%3AAndroid%2Fdata/document/primary";
    //Android/data目录
    public static final String URI_ANRROID_DATA = "content://com.android.externalstorage.documents/tree/primary%3AAndroid%2Fdata/document/primary%3AAndroid%2Fdata";
    //uri路径分割符
    public static final String URI_SEPARATOR = "%2F";
    //SDcard有通配符%s匹配sd卡名
    public static final String URI_SDCARD_ROOT_FRONT = "content://com.android.externalstorage.documents/tree/";
    public static final String URI_SDCARD_ROOT_LAST = "%3A";

    public static String getSdcardRootUriByPath(String path) {
        int last = path.lastIndexOf("/");
        String SDName = path.substring(last + 1);
        return URI_SDCARD_ROOT_FRONT + SDName;
    }

    public static List data2UriMimeTypeData(List<String> data) {
        List<String> backData = null;
        if (data != null) {
            backData = new ArrayList<>();
            Map<String, String> map = Constants.mimeTypeMap;

            for (String s : data) {
                if (map.containsKey(s)) {
                    if (!backData.contains(map.get(s))) {
                        backData.add(map.get(s));
                    }

                }
            }

        }
        return backData;
    }

    public static DocumentFile uri2DocumentFile(Context context, Uri uri) {
        DocumentFile documentFile = DocumentFile.fromSingleUri(context, uri);
        return documentFile;
    }

    public static DocumentFile uri2DocumentFile(Context context, String path) {
        return uri2DocumentFile(context, file2Uri(path));
    }

    public static DocumentFile uri2DocumentFile(Context context, File file) {
        return uri2DocumentFile(context, file2Uri(file.getAbsolutePath()));
    }

    public static void getTabbarFileBeanList(List<TabbarFileBean> tabbarList, String path, List<String> SdCardList) {
        if (SdCardList.contains(path)) {
            int i = SdCardList.indexOf(path);
            if (i == 0) {
                tabbarList.add(0, new TabbarFileBean(path, "内部存储", true, uri2DocumentFile(Commons.getApplicationByReflect().getBaseContext(), path)));
            } else if (i > 0) {
                tabbarList.add(0, new TabbarFileBean(path, String.format("SD%d", i), true, uri2DocumentFile(Commons.getApplicationByReflect().getBaseContext(), path)));
            } else {
                tabbarList.add(0, new TabbarFileBean(path, "错误163", true, uri2DocumentFile(Commons.getApplicationByReflect().getBaseContext(), path)));
            }
            return;
        }
        tabbarList.add(0, new TabbarFileBean(path, true, uri2DocumentFile(Commons.getApplicationByReflect().getBaseContext(), path)));
        getTabbarFileBeanList(tabbarList, FileTools.getParentPath(path), SdCardList);
    }

    public static List<TabbarFileBean> upDataTabbarFileBeanListByUri(List<TabbarFileBean> tabbarList, TabbarFileListAdapter tabbarAdapter, String path, int type, List<String> SdCardList) {

        switch (type) {
            case BeanListManager.TypeAddTabbar:
                tabbarList.add(new TabbarFileBean(path, true, uri2DocumentFile(Commons.getApplicationByReflect().getBaseContext(), path)));
                break;
            case BeanListManager.TypeDelTabbar:
                for (int i = tabbarList.size() - 1; i >= 0; i--) {
                    if (tabbarList.get(i).getFilePath().length() > path.length()) {//移除比当前路径还长的数据
                        tabbarList.remove(i);
                    } else {
                        break;
                    }
                }
                break;
            case BeanListManager.TypeInitTabbar:
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

    public static List<FileBean> upDataFileBeanListByUri(Context context, Uri uri, List<FileBean> fileBeanList, FileListAdapter fileListAdapter, List<String> fileTypes, int sortType) {

        if (fileBeanList == null) {
            fileBeanList = new ArrayList<>();
        } else if (fileBeanList.size() != 0) {
            fileBeanList.clear();
        }
        FileBean fileBean;

        if (context == null || uri == null) return null;
        DocumentFile rootDocumentFile = DocumentFile.fromSingleUri(context, uri);
        if (rootDocumentFile == null) return null;


        DocumentFile pickedDir = rootDocumentFile.fromTreeUri(context, uri);
        if (pickedDir == null) return null;
        for (DocumentFile i : pickedDir.listFiles()) {
            fileBean = new FileBean(UriTools.uri2File(i.getUri()), true, i);
            if (fileTypes == null || fileTypes.size() == 0 || fileBean.isDir() || fileTypes.contains(fileBean.getFileExtension())) {
                fileBeanList.add(fileBean);
            }
        }
        BeanListManager.sortFileBeanList(fileBeanList, sortType);
        if (fileListAdapter != null) {
            fileListAdapter.notifyDataSetChanged();
            if (fileBeanList.size() == 0) {

//                fileListAdapter.setEmptyView(R.layout.fragment_empty_files_list_mlh);

            }
        }

        return fileBeanList;

    }

    public static void handleDirSubfileByUri(ContentResolver contentResolver, Context context, Uri uri, int handleType, List<String> filerType) {
        if (context == null || uri == null) return;
        DocumentFile rootDocumentFile = DocumentFile.fromSingleUri(context, uri);
        if (rootDocumentFile == null) return;
        DocumentFile pickedDir = rootDocumentFile.fromTreeUri(context, uri);
        if (pickedDir == null) return;
        for (DocumentFile i : pickedDir.listFiles()) {
            if (i.isDirectory()) {
                handleDirSubfileByUri(contentResolver, context, i.getUri(), handleType, filerType);
            } else {

                if (filerType != null && !filerType.contains(i.getType())) {
                    continue;
                }

                switch (handleType) {
                    case 0:
                        copyFileByUri(contentResolver, i.getUri(), "/storage/emulated/0/" + i.getName());
                        break;
                    default:
                }
            }
        }
    }

    public static void handleDirSubfileByUri(ContentResolver contentResolver, Context context, Uri uri, int handleType) {
        handleDirSubfileByUri(contentResolver, context, uri, handleType, null);
    }

    private static boolean copyFileByUri(ContentResolver contentResolver, Uri sourceUri, String filePath) {
        InputStream in = null;
        OutputStream out = null;
        try {
            in = contentResolver.openInputStream(sourceUri);

            File destFile = new File(filePath);
            out = new FileOutputStream(destFile);

            if (out == null) {
                return false;
            }

            byte[] flush = new byte[1024];
            int len = -1;
            while ((len = in.read(flush)) != -1) {
                out.write(flush, 0, len);
            }

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                in.close();
                out.close();
            } catch (IOException e) {
                e.printStackTrace();
                return false;
            }
        }
        return true;
    }

    public static Uri file2Uri(String path) {
        if (path.matches(Constants.PATH_ANRROID_DATA + "(.*)")) {
            String lastPart = path.replace(Constants.PATH_ANRROID_DATA, "").replace(File.separator, URI_SEPARATOR);
            return Uri.parse(URI_ANRROID_DATA + lastPart);
        }
        return null;
    }

    public static Uri file2Uri(File file) {
        return file2Uri(file.getAbsolutePath());
    }

    public static String uri2File(Uri uri) {
        String s = uri.toString();
        if (s.matches(URI_ANRROID_DATA + "(.*)")) {
            String lastPart = s.replace(URI_ANRROID_DATA, "").replace(URI_SEPARATOR, File.separator);
            return Constants.PATH_ANRROID_DATA + lastPart;
        }
        return null;
    }

    public static File uri2File(Uri uri, Context context, Application app) {

        if (uri == null) {
            return null;
        } else {
            File file = uri2FileReal(uri, context, app);
            return file != null ? file : copyUri2Cache(uri, context, app);
        }
    }

    private static File copyUri2Cache(Uri uri, Context context, Application app) {
        Log.d("UriUtils", "copyUri2Cache() called");
        InputStream is = null;

        File var3;
        try {
            is = app.getContentResolver().openInputStream(uri);
            File file = new File(context.getCacheDir(), "" + System.currentTimeMillis());
            FileTools.writeFileFromIS(file, is, false);
            var3 = file;
            return var3;
        } catch (FileNotFoundException var13) {
            var13.printStackTrace();
            var3 = null;
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException var12) {
                    var12.printStackTrace();
                }
            }

        }

        return var3;
    }


    private static File uri2FileReal(Uri uri, Context context, Application app) {
        Log.d("UriUtils", uri.toString());
        String authority = uri.getAuthority();
        String scheme = uri.getScheme();
        String path = uri.getPath();
        if (Build.VERSION.SDK_INT >= 24 && path != null) {
            String[] externals = new String[]{"/external/", "/external_path/"};
            File file = null;
            String[] var6 = externals;
            int var7 = externals.length;

            for (int var8 = 0; var8 < var7; ++var8) {
                String external = var6[var8];
                if (path.startsWith(external)) {
                    file = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + path.replace(external, "/"));
                    if (file.exists()) {
                        Log.d("UriUtils", uri.toString() + " -> " + external);
                        return file;
                    }
                }
            }

            file = null;
            if (path.startsWith("/files_path/")) {
                file = new File(context.getFilesDir().getAbsolutePath() + path.replace("/files_path/", "/"));
            } else if (path.startsWith("/cache_path/")) {
                file = new File(context.getCacheDir().getAbsolutePath() + path.replace("/cache_path/", "/"));
            } else if (path.startsWith("/external_files_path/")) {
                file = new File(context.getExternalFilesDir((String) null).getAbsolutePath() + path.replace("/external_files_path/", "/"));
            } else if (path.startsWith("/external_cache_path/")) {
                file = new File(context.getExternalCacheDir().getAbsolutePath() + path.replace("/external_cache_path/", "/"));
            }

            if (file != null && file.exists()) {
                Log.d("UriUtils", uri.toString() + " -> " + path);
                return file;
            }
        }

        if ("file".equals(scheme)) {
            if (path != null) {
                return new File(path);
            } else {
                Log.d("UriUtils", uri.toString() + " parse failed. -> 0");
                return null;
            }
        } else if (Build.VERSION.SDK_INT >= 19 && DocumentsContract.isDocumentUri(context, uri)) {
            String id;
            String type;
            String[] split;
            if ("com.android.externalstorage.documents".equals(authority)) {
                id = DocumentsContract.getDocumentId(uri);
                split = id.split(":");
                type = split[0];
                if ("primary".equalsIgnoreCase(type)) {
                    return new File(Environment.getExternalStorageDirectory() + "/" + split[1]);
                } else {
                    @SuppressLint("WrongConstant") StorageManager mStorageManager = (StorageManager) app.getSystemService("storage");

                    try {
                        Class<?> storageVolumeClazz = Class.forName("android.os.storage.StorageVolume");
                        Method getVolumeList = mStorageManager.getClass().getMethod("getVolumeList");
                        Method getUuid = storageVolumeClazz.getMethod("getUuid");
                        Method getState = storageVolumeClazz.getMethod("getState");
                        Method getPath = storageVolumeClazz.getMethod("getPath");
                        Method isPrimary = storageVolumeClazz.getMethod("isPrimary");
                        Method isEmulated = storageVolumeClazz.getMethod("isEmulated");
                        Object result = getVolumeList.invoke(mStorageManager);
                        int length = Array.getLength(result);

                        for (int i = 0; i < length; ++i) {
                            Object storageVolumeElement = Array.get(result, i);
                            boolean mounted = "mounted".equals(getState.invoke(storageVolumeElement)) || "mounted_ro".equals(getState.invoke(storageVolumeElement));
                            if (mounted && (!(Boolean) isPrimary.invoke(storageVolumeElement) || !(Boolean) isEmulated.invoke(storageVolumeElement))) {
                                String uuid = (String) getUuid.invoke(storageVolumeElement);
                                if (uuid != null && uuid.equals(type)) {
                                    return new File(getPath.invoke(storageVolumeElement) + "/" + split[1]);
                                }
                            }
                        }
                    } catch (Exception var23) {
                        Log.d("UriUtils", uri.toString() + " parse failed. " + var23.toString() + " -> 1_0");
                    }

                    Log.d("UriUtils", uri.toString() + " parse failed. -> 1_0");
                    return null;
                }
            } else if (!"com.android.providers.downloads.documents".equals(authority)) {
                if ("com.android.providers.media.documents".equals(authority)) {
                    id = DocumentsContract.getDocumentId(uri);
                    split = id.split(":");
                    type = split[0];
                    Uri contentUri;
                    if ("image".equals(type)) {
                        contentUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
                    } else if ("video".equals(type)) {
                        contentUri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
                    } else {
                        if (!"audio".equals(type)) {
                            Log.d("UriUtils", uri.toString() + " parse failed. -> 1_2");
                            return null;
                        }

                        contentUri = MediaStore.Audio.Media.EXTERNAL_CONTENT_URI;
                    }

                    String selection = "_id=?";
                    String[] selectionArgs = new String[]{split[1]};
                    return getFileFromUri(contentUri, "_id=?", selectionArgs, "1_2", app);
                } else if ("content".equals(scheme)) {
                    return getFileFromUri(uri, "1_3", app);
                } else {
                    Log.d("UriUtils", uri.toString() + " parse failed. -> 1_4");
                    return null;
                }
            } else {
                id = DocumentsContract.getDocumentId(uri);
                if (TextUtils.isEmpty(id)) {
                    Log.d("UriUtils", uri.toString() + " parse failed(id is null). -> 1_1");
                    return null;
                } else if (id.startsWith("raw:")) {
                    return new File(id.substring(4));
                } else {
                    if (id.startsWith("msf:")) {
                        id = id.split(":")[1];
                    }

                    long availableId = 0L;

                    try {
                        availableId = Long.parseLong(id);
                    } catch (Exception var22) {
                        return null;
                    }

                    String[] contentUriPrefixesToTry = new String[]{"content://downloads/public_downloads", "content://downloads/all_downloads", "content://downloads/my_downloads"};
                    String[] var30 = contentUriPrefixesToTry;
                    int var34 = contentUriPrefixesToTry.length;

                    for (int var10 = 0; var10 < var34; ++var10) {
                        String contentUriPrefix = var30[var10];
                        Uri contentUri = ContentUris.withAppendedId(Uri.parse(contentUriPrefix), availableId);

                        try {
                            File file = getFileFromUri(contentUri, "1_1", app);
                            if (file != null) {
                                return file;
                            }
                        } catch (Exception var21) {
                        }
                    }

                    Log.d("UriUtils", uri.toString() + " parse failed. -> 1_1");
                    return null;
                }
            }
        } else if ("content".equals(scheme)) {
            return getFileFromUri(uri, "2", app);
        } else {
            Log.d("UriUtils", uri.toString() + " parse failed. -> 3");
            return null;
        }
    }


    private static File getFileFromUri(Uri uri, String code, Application app) {
        return getFileFromUri(uri, (String) null, (String[]) null, code, app);
    }

    private static File getFileFromUri(Uri uri, String selection, String[] selectionArgs, String code, Application app) {
        File fileDir;
        if ("com.google.android.apps.photos.content".equals(uri.getAuthority())) {
            if (!TextUtils.isEmpty(uri.getLastPathSegment())) {
                return new File(uri.getLastPathSegment());
            }
        } else {
            String path;
            if ("com.tencent.mtt.fileprovider".equals(uri.getAuthority())) {
                path = uri.getPath();
                if (!TextUtils.isEmpty(path)) {
                    fileDir = Environment.getExternalStorageDirectory();
                    return new File(fileDir, path.substring("/QQBrowser".length(), path.length()));
                }
            } else if ("com.huawei.hidisk.fileprovider".equals(uri.getAuthority())) {
                path = uri.getPath();
                if (!TextUtils.isEmpty(path)) {
                    return new File(path.replace("/root", ""));
                }
            }
        }

        Cursor cursor = app.getContentResolver().query(uri, new String[]{"_data"}, selection, selectionArgs, (String) null);
        if (cursor == null) {
            Log.d("UriUtils", uri.toString() + " parse failed(cursor is null). -> " + code);
            return null;
        } else {
            File var6;
            try {
                if (!cursor.moveToFirst()) {
                    Log.d("UriUtils", uri.toString() + " parse failed(moveToFirst return false). -> " + code);
                    fileDir = null;
                    return fileDir;
                }

                int columnIndex = cursor.getColumnIndex("_data");
                if (columnIndex <= -1) {
                    Log.d("UriUtils", uri.toString() + " parse failed(columnIndex: " + columnIndex + " is wrong). -> " + code);
                    var6 = null;
                    return var6;
                }

                var6 = new File(cursor.getString(columnIndex));
                return var6;
            } catch (Exception var10) {
                Log.d("UriUtils", uri.toString() + " parse failed. -> " + code);
                var6 = null;
            } finally {
                cursor.close();
            }

            return var6;
        }
    }

}
