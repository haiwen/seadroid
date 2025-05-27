/*
 * Copyright (C) 2014 Dariush Forouher
 *
 * Based on the example from https://developer.android.com/samples/StorageProvider/index.html
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as
 *  published by the Free Software Foundation, either version 3 of the
 *  License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.seafile.seadroid2.provider;

import android.accounts.OnAccountsUpdateListener;
import android.content.Context;
import android.content.res.AssetFileDescriptor;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.graphics.Bitmap;
import android.graphics.Point;
import android.net.Uri;
import android.os.Bundle;
import android.os.CancellationSignal;
import android.os.ParcelFileDescriptor;
import android.provider.DocumentsContract;
import android.provider.DocumentsContract.Document;
import android.provider.DocumentsContract.Root;
import android.provider.DocumentsProvider;
import android.text.TextUtils;

import androidx.annotation.StringRes;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.request.RequestOptions;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.GlideApp;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.RepoType;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.output.TeeOutputStream;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;

import io.reactivex.Single;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;
import io.reactivex.schedulers.Schedulers;

/**
 * DocumentProvider for the Storage Access Framework.
 * <p>
 * It depends on API level 19 and supports API level 21.
 * <p>
 * This Provider gives access to other Apps to browse, read and write all files
 * contained in Seafile repositories.
 */
public class SeafileProvider extends DocumentsProvider {
    private final String TAG = "SeafileProvider";

    public static final String PATH_SEPARATOR = "/";
    public static final String AUTHORITY_OF_DOCUMENTS = BuildConfig.APPLICATION_ID + ".documents";

    public static final Uri NOTIFICATION_URI = DocumentsContract.buildRootsUri(AUTHORITY_OF_DOCUMENTS);

    private static final String[] SUPPORTED_ROOT_PROJECTION = new String[]{
            Root.COLUMN_ROOT_ID,
            Root.COLUMN_FLAGS,
            Root.COLUMN_TITLE,
            Root.COLUMN_DOCUMENT_ID,
            Root.COLUMN_SUMMARY,
            Root.COLUMN_ICON
    };

    private static final String[] SUPPORTED_DOCUMENT_PROJECTION = new String[]{
            Document.COLUMN_DOCUMENT_ID,
            Document.COLUMN_MIME_TYPE,
            Document.COLUMN_DISPLAY_NAME,
            Document.COLUMN_LAST_MODIFIED,
            Document.COLUMN_FLAGS,
            Document.COLUMN_SIZE,
            Document.COLUMN_ICON,
            Document.COLUMN_SUMMARY
    };

    private android.accounts.AccountManager androidAccountManager;

    private final OnAccountsUpdateListener accountListener = new OnAccountsUpdateListener() {
        @Override
        public void onAccountsUpdated(android.accounts.Account[] accounts) {
            Context c = SeadroidApplication.getAppContext();
            c.getContentResolver().notifyChange(NOTIFICATION_URI, null);
        }
    };

    @Override
    public boolean onCreate() {

        androidAccountManager = android.accounts.AccountManager.get(getContext());

        androidAccountManager.addOnAccountsUpdatedListener(accountListener, null, true);

        return true;
    }

    @Override
    public Cursor queryRoots(String[] projection) throws FileNotFoundException {
        SLogs.d(TAG, "queryRoots()");

        String[] netProjection = netProjection(projection, SUPPORTED_ROOT_PROJECTION);
        MatrixCursor result = new MatrixCursor(netProjection);

        // add a Root for every signed in Seafile account we have.
        for (Account a : SupportAccountManager.getInstance().getAccountList()) {
            if (a.hasValidToken()) {
                includeRoot(result, a);
            }
        }

        // notification uri for the event, that the account list has changed
        result.setNotificationUri(getContext().getContentResolver(), NOTIFICATION_URI);

        return result;
    }

    /**
     * Create a MatrixCursor
     *
     * @param netProjection column list
     */
    private static MatrixCursor createCursor(String[] netProjection) {

        return new MatrixCursor(netProjection);
    }

    private Uri toNotifyUri(String parentDocumentId) {
        return DocumentsContract.buildChildDocumentsUri(AUTHORITY_OF_DOCUMENTS, parentDocumentId);
    }

    /**
     * query the children of a given parent document.
     */
    @Override
    public Cursor queryChildDocuments(String parentDocumentId, String[] projection, String sortOrder) throws FileNotFoundException {

        SLogs.d(TAG, "queryChildDocuments()", parentDocumentId);

        if (TextUtils.isEmpty(parentDocumentId)) {
            throw throwFileNotFoundException(R.string.saf_bad_mime_type);
        }

        Account account = DocumentIdParser.getAccountFromId(parentDocumentId);
        if (account == null) {
            throw throwFileNotFoundException(R.string.saf_account_not_found_exception);
        }

        String[] netProjection = netProjection(projection, SUPPORTED_DOCUMENT_PROJECTION);
        String repoId = DocumentIdParser.getRepoIdFromId(parentDocumentId);
        String path = DocumentIdParser.getPathFromId(parentDocumentId);

        final Uri notifyUri = toNotifyUri(parentDocumentId);

        if (repoId.isEmpty()) {
            // in this case the user is asking for a list of repositories

            MatrixCursor matrixCursor = createCursor(netProjection);

            List<RepoModel> repos = AppDatabase.getInstance().repoDao().getListByAccountSync(account.getSignature());
            if (!CollectionUtils.isEmpty(repos)) {
                for (RepoModel repoModel : repos) {
                    includeRepo(matrixCursor, account, repoModel);
                }
            }

            int dataStatus = SeadroidApplication.getDocumentCache().get(parentDocumentId);
            SLogs.d(TAG, "queryChildDocuments()", "repo -> dataStatus=" + dataStatus);

            if (dataStatus == -1) {
                // tell the client that more entries will arrive shortly.
                Bundle bundle = new Bundle();
                bundle.putBoolean(DocumentsContract.EXTRA_LOADING, true);
                String loadingText = SeadroidApplication.getAppString(R.string.pull_to_refresh_refreshing_label);
                bundle.putString(DocumentsContract.EXTRA_INFO, loadingText);
                matrixCursor.setExtras(bundle);

                fetchReposAsync(account, notifyUri, matrixCursor);
            }

            includeStarredDir(matrixCursor, account);

            return matrixCursor;

        } else if (DocumentIdParser.isStarredDir(parentDocumentId)) {
            // the user is asking for the list of starred files

            MatrixCursor matrixCursor = createCursor(netProjection);
            List<StarredModel> starredList = AppDatabase.getInstance().starredDirentDAO().getListByAccountSync(account.getSignature());
            if (!CollectionUtils.isEmpty(starredList)) {
                for (StarredModel starredModel : starredList) {
                    includeStarredFileDirent(matrixCursor, account, starredModel);
                }
            }

            int dataStatus = SeadroidApplication.getDocumentCache().get(parentDocumentId);
            SLogs.d(TAG, "queryChildDocuments()", "starred -> parentDocumentId= " + parentDocumentId + ", dataStatus=" + dataStatus);

            if (dataStatus == -1) {
                // tell the client that more entries will arrive shortly.
                Bundle bundle = new Bundle();
                bundle.putBoolean(DocumentsContract.EXTRA_LOADING, true);
                String loadingText = SeadroidApplication.getAppString(R.string.pull_to_refresh_refreshing_label);
                bundle.putString(DocumentsContract.EXTRA_INFO, loadingText);
                matrixCursor.setExtras(bundle);

                fetchStarredAsync(account, notifyUri, matrixCursor);
            }


            return matrixCursor;

        } else {
            // in this case, the repository is known. the user wants the entries of a specific
            // directory in the given repository.

            // the android API asks us to be quick, so just use the cache.
            List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoId);
            if (CollectionUtils.isEmpty(repoModels)) {
                throw throwFileNotFoundException(R.string.repo_not_found);
            }

            final RepoModel repoModel = repoModels.get(0);
            if (repoModel == null) {
                throw throwFileNotFoundException(R.string.repo_not_found);
            }


            // todo
            // maybe we can check if the user has decrypted it or not. if yes, then the data can be read and returned directly.

            // old
            // encrypted repos are not supported (we can't ask the user for the passphrase)
            if (repoModel.encrypted) {
                throw throwFileNotFoundException(R.string.saf_write_diretory_exception);
            }

            MatrixCursor matrixCursor = createCursor(netProjection);

            if (!path.endsWith("/")) {
                path += "/";
            }

            List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByParentPathSync(repoId, path);
            if (!CollectionUtils.isEmpty(direntModels)) {
                if (path.endsWith("/")) {
                    path = StringUtils.substringBeforeLast(path, "/");
                }

                for (DirentModel direntModel : direntModels) {
                    includeDirent(matrixCursor, account, repoModel, direntModel);
                }
            }


            int dataStatus = SeadroidApplication.getDocumentCache().get(parentDocumentId);
            SLogs.d(TAG, "queryChildDocuments()", "dirent-> parentDocumentId= " + parentDocumentId + ", dataStatus=" + dataStatus);

            if (dataStatus == -1) {
                // tell the client that more entries will arrive shortly.
                Bundle bundle = new Bundle();
                bundle.putBoolean(DocumentsContract.EXTRA_LOADING, true);
                String loadingText = SeadroidApplication.getAppString(R.string.pull_to_refresh_refreshing_label);
                bundle.putString(DocumentsContract.EXTRA_INFO, loadingText);
                matrixCursor.setExtras(bundle);

                fetchDirentAsync(account, notifyUri, repoModel, path, matrixCursor);
            }

            return matrixCursor;
        }
    }

    /**
     * query the meta data of a file/folder.
     */
    @Override
    public Cursor queryDocument(String documentId, String[] projection) throws FileNotFoundException {

        SLogs.d(TAG, "queryDocument()", documentId);

        if (TextUtils.isEmpty(documentId)) {
            throw throwFileNotFoundException(R.string.saf_bad_mime_type);
        }

        String[] netProjection = netProjection(projection, SUPPORTED_DOCUMENT_PROJECTION);
        MatrixCursor result = new MatrixCursor(netProjection);

        Account account = DocumentIdParser.getAccountFromId(documentId);
        if (account == null) {
            throw throwFileNotFoundException(R.string.saf_account_not_found_exception);
        }

        String repoId = DocumentIdParser.getRepoIdFromId(documentId);
        String path = DocumentIdParser.getPathFromId(documentId);

        if (repoId.isEmpty()) {
            // the user has asked for the base document_id for a root

            includeDocIdRoot(result, account);
            return result;
        }

        // the android API asks us to be quick, so just use the cache.
        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoId);
        if (CollectionUtils.isEmpty(repoModels)) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }

        RepoModel repoModel = repoModels.get(0);
        if (repoModel == null) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }

        if (DocumentIdParser.isStarredDir(documentId)) {
            includeStarredDir(result, account);
        } else if (path.equals(PATH_SEPARATOR)) {
            // this is the base of the repository. this is special, as we give back the information
            // about the repository itself, not some directory in it.
            includeRepo(result, account, repoModel);
        } else {
//             the general case. a query about a file/directory in a repository.

//             again we only use cached info in this function. that shouldn't be an issue
//             as very likely there has been a SeafileProvider.queryChildDocuments() call just moments earlier.
            List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByFullPathSync(repoId, path);
            if (!CollectionUtils.isEmpty(direntModels)) {
                // the file is in the dirent of the parent directory
                DirentModel direntModel = direntModels.get(0);
                includeDirent(result, account, repoModel, direntModel);
            }
        }

        return result;
    }

    @Override
    public boolean isChildDocument(String parentId, String documentId) {
        return documentId.startsWith(parentId);
    }

    @Override
    public ParcelFileDescriptor openDocument(final String documentId, final String mode, final CancellationSignal signal) throws FileNotFoundException {
        SLogs.d(TAG, "openDocument()", documentId);
        if (TextUtils.isEmpty(documentId) || TextUtils.isEmpty(mode)) {
            throw throwFileNotFoundException(R.string.saf_bad_mime_type);
        }

        if (!NetworkUtils.isConnected())
            throw throwFileNotFoundException(R.string.network_error);

        Account account = DocumentIdParser.getAccountFromId(documentId);
        if (account == null) {
            throw throwFileNotFoundException(R.string.saf_account_not_found_exception);
        }

        String path = DocumentIdParser.getPathFromId(documentId);
        if (TextUtils.isEmpty(path)) {
            throw throwFileNotFoundException(R.string.saf_bad_mime_type);
        }

        String repoId = DocumentIdParser.getRepoIdFromId(documentId);
        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoId);
        if (CollectionUtils.isEmpty(repoModels)) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }

        final RepoModel repoModel = repoModels.get(0);
        if (repoModel == null) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }

        final boolean isWrite = (mode.indexOf('w') != -1);
        if (isWrite) {
            //write
            return writeDocumentAsync(documentId, mode, account, repoModel, path, signal);
        } else {
            //read
            return readDocumentAsync(documentId, mode, account, repoModel, path, signal);
        }
    }

    private ParcelFileDescriptor writeDocumentAsync(final String documentId, final String mode, Account account, RepoModel repoModel, String path, final CancellationSignal signal) throws FileNotFoundException {
        if (documentId.endsWith("/")) {
            // 返回一个黑洞的 ParcelFileDescriptor，系统会写进去但实际丢弃
            return ParcelFileDescriptor.open(new File("/dev/null"), ParcelFileDescriptor.MODE_WRITE_ONLY);
        }

        if (!repoModel.hasWritePermission()) {
            throw throwFileNotFoundException(R.string.saf_write_diretory_exception);
        }

        String displayName = Utils.getFileNameFromPath(path);
        try {
            // 创建管道：readFd 给系统写入，writeFd 你来读取并上传
            ParcelFileDescriptor[] pipe = ParcelFileDescriptor.createPipe();
            ParcelFileDescriptor readFd = pipe[0];
            ParcelFileDescriptor writeFd = pipe[1];

            CompletableFuture.runAsync(() -> {
                try (InputStream in = new FileInputStream(readFd.getFileDescriptor())) {
                    OpenDocumentWriter.uploadStreamToCloud(account, repoModel.repo_id, repoModel.repo_name, path, displayName, in, signal);
                } catch (Exception e) {
                    SLogs.d(TAG, "Failed to open document, upload failed", e.getMessage());
                } finally {
                    try {
                        readFd.close();
                    } catch (IOException ignored) {
                    }
                }
            });

            return writeFd;
        } catch (IOException e) {
            throw new FileNotFoundException("Failed to open document with id " + documentId + " and mode " + mode);
        }
    }

    private ParcelFileDescriptor readDocumentAsync(final String documentId, final String mode, Account account, RepoModel repoModel, String path, final CancellationSignal signal) throws FileNotFoundException {
        // Check whether the remote data exists. If it doesn't, throw an exception,
        // because we have already inserted it into the local database in queryChildDocuments().
        List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByFullPathSync(repoModel.repo_id, path);
        if (CollectionUtils.isEmpty(direntModels)) {
            throw new FileNotFoundException("could not find file");
        }

        DirentModel direntModel = direntModels.get(0);

        // Check if the local cache database exists. If the file has not been downloaded,
        // it will not exist in the cache database. In that case, throw an exception.
        List<FileCacheStatusEntity> caches = AppDatabase.getInstance().fileCacheStatusDAO().getByFullPathSync(repoModel.repo_id, path);
        if (!CollectionUtils.isEmpty(caches)) {

            FileCacheStatusEntity cacheStatusEntity = caches.get(0);

            //check if local file is same as remote file
            if (TextUtils.equals(direntModel.id, cacheStatusEntity.file_id)) {
                //check if local file exists
                File file = DataManager.getLocalFileCachePath(account, repoModel.repo_id, repoModel.repo_name, path);
                if (file.exists()) {
                    try {
                        return makeParcelFileDescriptor(file, mode);
                    } catch (IOException e) {
                        SLogs.d(TAG, "openDocument()", "could not open file");
                        SLogs.e(e);
                        throw new FileNotFoundException();
                    }
                }
            }
        }


        //local don't exists
        if (!NetworkUtils.isConnected()) {
            throw throwFileNotFoundException(R.string.network_error);
        }

        try {
            ParcelFileDescriptor[] pipe = ParcelFileDescriptor.createReliableSocketPair();
            final ParcelFileDescriptor toSystemFd = pipe[0];  // 返回给系统，交给第三方 App，用于"读取数据"
            final ParcelFileDescriptor mySideFd = pipe[1]; // 你来写入下载内容，自己使用，用于"写入数据"

            CompletableFuture.runAsync(() -> {

                File destinationFile = DataManager.getLocalFileCachePath(account, repoModel.repo_id, repoModel.repo_name, path);

                try (OutputStream pipeOut = new FileOutputStream(mySideFd.getFileDescriptor());
                     OutputStream fileOut = new FileOutputStream(destinationFile);
                     TeeOutputStream teeOut = new TeeOutputStream(pipeOut, fileOut)) {

                    OpenDocumentReader.streamDownloadToPipe(account, repoModel.repo_id, repoModel.repo_name, path, direntModel.id, teeOut, destinationFile, signal, new OpenDocumentReader.ProgressListener() {

                        @Override
                        public void onComplete() {
                            SLogs.d(TAG, "openDocument()", "download complete");
                        }

                        @Override
                        public void onError(Exception e) {

                            String msg = e.getMessage();
                            if (msg != null && (
                                    msg.contains("Software caused connection abort") ||
                                            msg.contains("Broken pipe") ||
                                            msg.contains("EBADF") ||
                                            msg.contains("stream was reset")
                            )) {
                                SLogs.d(TAG, "openDocument(): Pipe closed by reader, safe to ignore");
                            } else {
                                SLogs.d(TAG, "openDocument(): Error occurred: " + e.getMessage());
                            }
                            SLogs.e(e);
                        }
                    });

                } catch (IOException e) {
                    try {
                        SLogs.d(TAG, "download failed: " + e.getMessage());
                        SLogs.e(e);
                        throw new FileNotFoundException("Failed to open document, download failed: " + e.getMessage());
                    } catch (FileNotFoundException ignored) {
                    }
                } finally {
                    try {
                        mySideFd.close();
                    } catch (IOException ignored) {
                    }
                }
            });

            return toSystemFd;
        } catch (IOException e) {
            throw new FileNotFoundException("Failed to open document with id " + documentId + " and mode " + mode);
        }
    }

    private ParcelFileDescriptor readDocumentSync(final String documentId, final String mode, Account account, RepoModel repoModel, String path, final CancellationSignal signal) throws FileNotFoundException {
        // Check whether the remote data exists. If it doesn't, throw an exception,
        // because we have already inserted it into the local database in queryChildDocuments().
        List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByFullPathSync(repoModel.repo_id, path);
        if (CollectionUtils.isEmpty(direntModels)) {
            throw new FileNotFoundException("could not find file");
        }

        DirentModel direntModel = direntModels.get(0);

        // Check if the local cache database exists. If the file has not been downloaded,
        // it will not exist in the cache database. In that case, throw an exception.
        List<FileCacheStatusEntity> caches = AppDatabase.getInstance().fileCacheStatusDAO().getByFullPathSync(repoModel.repo_id, path);
        if (!CollectionUtils.isEmpty(caches)) {

            FileCacheStatusEntity cacheStatusEntity = caches.get(0);

            //check if local file is same as remote file
            if (TextUtils.equals(direntModel.id, cacheStatusEntity.file_id)) {
                //check if local file exists
                File file = DataManager.getLocalFileCachePath(account, repoModel.repo_id, repoModel.repo_name, path);
                if (file.exists()) {
                    try {
                        return makeParcelFileDescriptor(file, mode);
                    } catch (IOException e) {
                        SLogs.d(TAG, "openDocument()", "could not open file");
                        SLogs.e(e);
                        throw new FileNotFoundException();
                    }
                }
            }
        }


        //local don't exists
        if (!NetworkUtils.isConnected()) {
            throw throwFileNotFoundException(R.string.network_error);
        }

        final AtomicBoolean downloadResult = new AtomicBoolean(false);
        File destinationFile = DataManager.getLocalFileCachePath(account, repoModel.repo_id, repoModel.repo_name, path);

        CompletableFuture<Void> downloadFuture = CompletableFuture.runAsync(() -> {

            try (OutputStream fileOut = new FileOutputStream(destinationFile)) {


                OpenDocumentReader.streamDownloadToPipe(account, repoModel.repo_id, repoModel.repo_name, path, direntModel.id, fileOut, destinationFile, signal, new OpenDocumentReader.ProgressListener() {

                    @Override
                    public void onComplete() {
                        downloadResult.set(true);
                        SLogs.d(TAG, "openDocument()", "download complete");
                        // 下载完成后，通知系统文件已准备好
//                            getContext().getContentResolver().notifyChange(toNotifyUri(documentId), null);
                    }

                    @Override
                    public void onError(Exception e) {
                        downloadResult.set(true);
                        String msg = e.getMessage();
                        if (msg != null && (
                                msg.contains("Software caused connection abort") ||
                                        msg.contains("Broken pipe") ||
                                        msg.contains("EBADF") ||
                                        msg.contains("stream was reset")
                        )) {
                            SLogs.d(TAG, "openDocument(): Pipe closed by reader, safe to ignore");
                        } else {
                            SLogs.d(TAG, "openDocument(): Error occurred: " + e.getMessage());
                        }
                        SLogs.e(e);
                    }
                });

            } catch (IOException e) {
                SLogs.d(TAG, "openDocument()", "could not open file");
                SLogs.e(e);
            }
        });

        downloadFuture.join();
        if (!downloadResult.get()) {
            throw new FileNotFoundException("Error downloading file: " + destinationFile.getName());
        }

        try {
            return makeParcelFileDescriptor(destinationFile, mode);
        } catch (IOException e) {
            SLogs.d(TAG, "openDocument()", "could not open file");
            SLogs.e(e);
            throw new FileNotFoundException("could not open file");
        }
    }

    /**
     * Create ParcelFileDescriptor from the given file.
     *
     * @param file the file
     * @param mode the mode the file shoall be opened with.
     * @return a ParcelFileDescriptor
     * @throws IOException
     */
    private ParcelFileDescriptor makeParcelFileDescriptor(File file, String mode) throws IOException {
        final int accessMode = ParcelFileDescriptor.parseMode(mode);
        return ParcelFileDescriptor.open(file, accessMode);
    }


    @Override
    public AssetFileDescriptor openDocumentThumbnail(String documentId, Point sizeHint, CancellationSignal signal) throws FileNotFoundException {
        SLogs.d(TAG, "openDocumentThumbnail()", documentId);

        if (TextUtils.isEmpty(documentId)) {
            throw throwFileNotFoundException(R.string.saf_bad_mime_type);
        }

        Account account = DocumentIdParser.getAccountFromId(documentId);
        if (account == null) {
            throw throwFileNotFoundException(R.string.saf_account_not_found_exception);
        }

        String path = DocumentIdParser.getPathFromId(documentId);
        String repoId = DocumentIdParser.getRepoIdFromId(documentId);

        if (repoId.isEmpty()) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoId);
        if (CollectionUtils.isEmpty(repoModels)) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }

        final RepoModel repoModel = repoModels.get(0);
        if (repoModel == null) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }


        String mimeType = Utils.getFileMimeType(documentId);
        if (!mimeType.startsWith("image/")) {
            throw throwFileNotFoundException(R.string.saf_bad_mime_type);
        }

        File file = DataManager.getLocalFileCachePath(account, repoModel.repo_id, repoModel.repo_name, path);
        if (file.exists()) {
            ParcelFileDescriptor pfd = ParcelFileDescriptor.open(file, ParcelFileDescriptor.MODE_READ_ONLY);
            return new AssetFileDescriptor(pfd, 0, file.length());
        }

        //local don't exists
        if (!NetworkUtils.isConnected()) {
            throw throwFileNotFoundException(R.string.network_error);
        }

        try {
            ParcelFileDescriptor[] pipe = ParcelFileDescriptor.createPipe();
            ParcelFileDescriptor readFd = pipe[0];
            ParcelFileDescriptor writeFd = pipe[1];

            // do thumbnail download in another thread to avoid possible network access in UI thread

            CompletableFuture<Void> completableFuture = CompletableFuture.runAsync(new Runnable() {
                @Override
                public void run() {
                    try (FileOutputStream fileStream = new FileOutputStream(writeFd.getFileDescriptor())) {
                        String pathEnc = URLEncoder.encode(path, "UTF-8");
                        String urlPath = account.getServer() + String.format("api2/repos/%s/thumbnail/?p=%s&size=%s", repoId, pathEnc, sizeHint.x);

                        SLogs.d(TAG, "openDocumentThumbnail()", "urlPath = " + urlPath);

                        RequestOptions requestOptions = new RequestOptions()
                                .diskCacheStrategy(DiskCacheStrategy.ALL);

                        Bitmap bitmap = GlideApp.with(getContext())
                                .asBitmap()
                                .apply(requestOptions)
                                .load(urlPath)
                                .centerCrop()
                                .submit(sizeHint.x, sizeHint.y)
                                .get();
                        if (bitmap != null) {
                            bitmap.compress(Bitmap.CompressFormat.PNG, 50, fileStream);
                        }

                    } catch (Exception e) {
                        SLogs.e(e);
                    } finally {
                        IOUtils.closeQuietly(writeFd);
                    }
                }
            });


            if (signal != null) {
                signal.setOnCancelListener(new CancellationSignal.OnCancelListener() {
                    @Override
                    public void onCancel() {
                        SLogs.d(TAG, "openDocumentThumbnail()", "cancelling download");
                        completableFuture.cancel(true);
                        IOUtils.closeQuietly(writeFd);
                    }
                });
            }

            return new AssetFileDescriptor(readFd, 0, AssetFileDescriptor.UNKNOWN_LENGTH);

        } catch (IOException e) {
            throw new FileNotFoundException();
        }
    }

    @Override
    public String createDocument(String parentDocumentId, String mimeType, String displayName) throws FileNotFoundException {
        SLogs.d(TAG, "createDocument()", "parentDocumentId: " + parentDocumentId, "mimeType: " + mimeType, "displayName: " + displayName);

        if (TextUtils.isEmpty(parentDocumentId) || TextUtils.isEmpty(mimeType) || TextUtils.isEmpty(displayName)) {
            throw throwFileNotFoundException(R.string.saf_bad_mime_type);
        }

        if (!NetworkUtils.isConnected())
            throw throwFileNotFoundException(R.string.network_error);

        Account account = DocumentIdParser.getAccountFromId(parentDocumentId);
        if (account == null) {
            throw throwFileNotFoundException(R.string.saf_account_not_found_exception);
        }

        String parentPath = DocumentIdParser.getPathFromId(parentDocumentId);
        if (TextUtils.isEmpty(parentPath)) {
            throw throwFileNotFoundException(R.string.saf_upload_path_not_available);
        }

        String repoId = DocumentIdParser.getRepoIdFromId(parentDocumentId);
        List<RepoModel> repos = AppDatabase.getInstance().repoDao().getRepoByIdSync(repoId);
        if (CollectionUtils.isEmpty(repos)) {
            throw throwFileNotFoundException(R.string.repo_not_found);
        }

        RepoModel repo = repos.get(0);
        if (repo == null || !repo.hasWritePermission()) {
            throw throwFileNotFoundException(R.string.saf_write_diretory_exception);
        }

        if (mimeType.equals(Document.MIME_TYPE_DIR)) {
            SLogs.d(TAG, "createDocument()", "mimeType is Document.MIME_TYPE_DIR, will create dir in cloud");
            Map<String, String> requestDataMap = new HashMap<>();
            requestDataMap.put("operation", "mkdir");

            String result = HttpIO.getInstanceByAccount(account).execute(DialogService.class).createDirSync(repoId, parentPath, requestDataMap);
            if (TextUtils.equals("success", result)) {
                SLogs.d(TAG, "createDocument()", "create dir success");
            }

            String r = DocumentIdParser.buildId(account, repoId, Utils.pathJoin(parentPath, displayName));
            return Utils.pathJoin(r, "/");
        }

        String buildId = DocumentIdParser.buildId(account, repoId, Utils.pathJoin(parentPath, displayName));
        SLogs.d(TAG, "createDocument()", "buildId = " + buildId);
        return buildId;
    }

    private FileNotFoundException throwFileNotFoundException(@StringRes int resId) {
        if (getContext() != null) {
            String s = getContext().getString(resId);
            SLogs.e(TAG, "throwFileNotFoundException()", s);
            return new FileNotFoundException(s);
        }
        SLogs.e(TAG, "throwFileNotFoundException()");
        return new FileNotFoundException();
    }

    /**
     * Add a cursor entry for the account root.
     * <p>
     * We don't know much about it.
     *
     * @param result  the cursor to write the row into.
     * @param account the account to add.
     */
    private void includeRoot(MatrixCursor result, Account account) {

        String docId = DocumentIdParser.buildId(account);
        String rootId = DocumentIdParser.buildRootId(account);

        SLogs.d(TAG, "includeRoot()", "docId = " + docId);
        SLogs.d(TAG, "includeRoot()", "rootId = " + rootId);

        final MatrixCursor.RowBuilder row = result.newRow();

        row.add(Root.COLUMN_ROOT_ID, rootId);
        row.add(Root.COLUMN_DOCUMENT_ID, docId);
        row.add(Root.COLUMN_ICON, R.mipmap.ic_launcher);
        row.add(Root.COLUMN_FLAGS, Root.FLAG_SUPPORTS_IS_CHILD | Root.FLAG_SUPPORTS_CREATE);
        row.add(Root.COLUMN_TITLE, account.getServerHost());
        row.add(Root.COLUMN_SUMMARY, account.getEmail());
    }

    /**
     * Add a cursor entry for the account base document_id.
     *
     * @param result  the cursor to write the row into.
     * @param account the account to add.
     */
    private void includeDocIdRoot(MatrixCursor result, Account account) {
        String docId = DocumentIdParser.buildId(account);

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, account.getServerHost());
        row.add(Document.COLUMN_LAST_MODIFIED, null);
        row.add(Document.COLUMN_FLAGS, 0);
        row.add(Document.COLUMN_ICON, R.mipmap.ic_launcher);
        row.add(Document.COLUMN_SIZE, null);
        row.add(Document.COLUMN_MIME_TYPE, Document.MIME_TYPE_DIR);
    }

    /**
     * Add a seafile repo to the cursor.
     *
     * @param result    the cursor to write the row into.
     * @param account   the account that contains the repo.
     * @param repoModel the repo to add.
     */
    private void includeRepo(MatrixCursor result, Account account, RepoModel repoModel) {

        String docId = DocumentIdParser.buildRepoDocId(account, repoModel.repo_id);

        int flags = 0;
        if (repoModel.hasWritePermission()) {
            flags |= Document.FLAG_DIR_SUPPORTS_CREATE;
        }

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, repoModel.repo_name);
        row.add(Document.COLUMN_LAST_MODIFIED, repoModel.last_modified_long);
        row.add(Document.COLUMN_FLAGS, flags);
        row.add(Document.COLUMN_ICON, repoModel.getIcon());
        row.add(Document.COLUMN_SIZE, repoModel.size);

        if (RepoType.TYPE_MINE.equals(repoModel.type)) {
            row.add(Document.COLUMN_SUMMARY, R.string.personal);
        } else if (RepoType.TYPE_SHARED.equals(repoModel.type)) {
            row.add(Document.COLUMN_SUMMARY, R.string.shared);
        } else if (RepoType.TYPE_GROUP.equals(repoModel.type)) {
            row.add(Document.COLUMN_SUMMARY, repoModel.group_name);
        } else {
            row.add(Document.COLUMN_SUMMARY, null);
        }

        if (repoModel.encrypted) {
            row.add(Document.COLUMN_MIME_TYPE, null); // undocumented: will grey out the entry
        } else {
            row.add(Document.COLUMN_MIME_TYPE, Document.MIME_TYPE_DIR);
        }
    }

    private void includeStarredDir(MatrixCursor result, Account account) {

        String docMagicId = DocumentIdParser.buildMagicStarredDirId(account);

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docMagicId);

        row.add(Document.COLUMN_DISPLAY_NAME, SeadroidApplication.getAppString(R.string.tabs_starred));
        row.add(Document.COLUMN_ICON, R.drawable.star_normal);
        row.add(Document.COLUMN_FLAGS, 0);

        row.add(Document.COLUMN_MIME_TYPE, Document.MIME_TYPE_DIR);
    }

    /**
     * add a dirent to the cursor.
     *
     * @param result the cursor to write the row into.
     * @param entry  the seafile dirent to add
     */
    private void includeDirent(MatrixCursor result, Account account, RepoModel repoModel, DirentModel entry) {

        String docId = DocumentIdParser.buildId(account, repoModel.repo_id, entry.full_path, entry.isDir());

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, entry.name);
        row.add(Document.COLUMN_SIZE, entry.size);
        row.add(Document.COLUMN_SUMMARY, null);
        row.add(Document.COLUMN_LAST_MODIFIED, entry.mtime * 1000);

        String mimeType; // if mimeType is null, undocumented: will grey out the entry
        if (entry.isDir()) {
            mimeType = DocumentsContract.Document.MIME_TYPE_DIR;
        } else {
            mimeType = Utils.getFileMimeType(docId);
        }

        int flags = 0;
        // only offer a thumbnail if the file is an image
        if (mimeType.startsWith("image/")) {
            flags |= Document.FLAG_SUPPORTS_THUMBNAIL;
        }

        if (repoModel.hasWritePermission()) {
            if (entry.isDir()) {
                flags |= Document.FLAG_DIR_SUPPORTS_CREATE;
            } else {
                flags |= Document.FLAG_SUPPORTS_WRITE;
            }
        }
        row.add(Document.COLUMN_FLAGS, flags);
        row.add(Document.COLUMN_MIME_TYPE, mimeType);

    }

    /**
     * add a dirent to the cursor.
     *
     * @param result the cursor to write the row into.
     */
    private void includeStarredFileDirent(MatrixCursor result, Account account, StarredModel starredModel) {
        String docId = DocumentIdParser.buildId(account, starredModel.repo_id, starredModel.path);


        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);

        String displayName;
        if (starredModel.isRepo()) {
            displayName = starredModel.repo_name;

            if (starredModel.repo_encrypted) {
                row.add(Document.COLUMN_ICON, R.drawable.baseline_repo_encrypted_24);
            } else {
                row.add(Document.COLUMN_ICON, R.drawable.baseline_repo_24);
            }
        } else {
            displayName = starredModel.obj_name;
        }

        if (starredModel.deleted) {
            String deleted = SeadroidApplication.getAppString(R.string.deleted);
            displayName = "(" + deleted + ") " + displayName;
        }

        row.add(Document.COLUMN_DISPLAY_NAME, displayName);

        String mimeType; // if mimeType is null, undocumented: will grey out the entry
        if (starredModel.is_dir) {
            mimeType = DocumentsContract.Document.MIME_TYPE_DIR;
        } else {
            mimeType = Utils.getFileMimeType(docId);
        }

        int flags = 0;
        // only offer a thumbnail if the file is an image
        if (!starredModel.repo_encrypted && mimeType.startsWith("image/")) {
            flags |= Document.FLAG_SUPPORTS_THUMBNAIL;
        }
        row.add(Document.COLUMN_FLAGS, flags);

        if (starredModel.repo_encrypted || starredModel.deleted || TextUtils.isEmpty(starredModel.obj_name)) {
            row.add(Document.COLUMN_MIME_TYPE, null); // undocumented: will grey out the entry
        } else {
            row.add(Document.COLUMN_MIME_TYPE, mimeType);
        }

    }

    /**
     * Fetches a dirent (list of entries of a directory) from Seafile asynchronously.
     * <p>
     * This will return nothing. It will only signal the client over the MatrixCursor. The client
     * will then recall DocumentProvider.queryChildDocuments() again.
     *
     * @param account   account
     * @param repoModel
     * @param path      the path of the directory.
     * @param result    Cursor object over which to signal the client.
     */
    private void fetchDirentAsync(Account account, Uri notifyUri, RepoModel repoModel, String path, MatrixCursor result) {
        result.setNotificationUri(getContext().getContentResolver(), notifyUri);
        String docId = DocumentsContract.getDocumentId(notifyUri);

        SLogs.d(TAG, "fetchDirentAsync()", "docId = " + docId);

        if (!path.endsWith("/")) {
            path = path + "/";
        }

        Single<List<DirentModel>> resultSingle = Objs.getDirentsSingleFromServer(account, repoModel.repo_id, repoModel.repo_name, path);
        Disposable disposable = resultSingle.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Consumer<List<DirentModel>>() {
                    @Override
                    public void accept(List<DirentModel> direntModels) throws Exception {
                        SLogs.d(TAG, "fetchDirentAsync()", "success: " + docId);

                        SeadroidApplication.getDocumentCache().put(docId);
                        notifyChanged(notifyUri);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        SLogs.d(TAG, "fetchDirentAsync()", "failed: " + docId);
                        SLogs.e(throwable);

                        notifyChanged(notifyUri);
                    }
                });
        compositeDisposable.add(disposable);
    }

    /**
     * Fetches starred files from Seafile asynchronously.
     * <p>
     * This will return nothing. It will only signal the client over the MatrixCursor. The client
     * will then recall DocumentProvider.queryChildDocuments() again.
     *
     * @param account account
     * @param result  Cursor object over which to signal the client.
     */
    private void fetchStarredAsync(Account account, Uri notifyUri, MatrixCursor result) {
        result.setNotificationUri(getContext().getContentResolver(), notifyUri);
        String docId = DocumentsContract.getDocumentId(notifyUri);
        SLogs.d(TAG, "fetchStarredAsync()", "docId = " + docId);

        Single<List<StarredModel>> listSingle = Objs.getStarredSingleFromServer(account);
        Disposable disposable = listSingle.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Consumer<List<StarredModel>>() {
                    @Override
                    public void accept(List<StarredModel> starredModels) throws Exception {
                        SLogs.d(TAG, "fetchStarredAsync()", "success: " + docId);
                        SeadroidApplication.getDocumentCache().put(docId);
                        notifyChanged(notifyUri);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        SLogs.d(TAG, "fetchDirentAsync()", "failed: " + docId);
                        SLogs.e(throwable);

                        notifyChanged(notifyUri);
                    }
                });
        compositeDisposable.add(disposable);
    }


    /**
     * Fetches a new list of repositories from Seafile asynchronously.
     * <p>
     * This will return nothing. It will only signal the client over the MatrixCursor. The client
     * will then recall DocumentProvider.queryChildDocuments() again.
     *
     * @param account account
     * @param result  Cursor object over which to signal the client.
     */
    private void fetchReposAsync(Account account, Uri notifyUri, MatrixCursor result) {
        result.setNotificationUri(getContext().getContentResolver(), notifyUri);

        String docId = DocumentsContract.getDocumentId(notifyUri);
        SLogs.d(TAG, "fetchReposAsync()", "docId = " + docId);

        Single<List<BaseModel>> resultSingle = Objs.getReposSingleFromServer(account);
        Disposable disposable = resultSingle.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Consumer<List<BaseModel>>() {
                    @Override
                    public void accept(List<BaseModel> baseModels) throws Exception {
                        SLogs.d(TAG, "fetchReposAsync()", "success: " + docId);
                        SeadroidApplication.getDocumentCache().put(docId);
                        notifyChanged(notifyUri);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        SLogs.d(TAG, "fetchReposAsync()", "failed: " + docId);
                        SLogs.e(throwable);
                        notifyChanged(notifyUri);
                    }
                });
        compositeDisposable.add(disposable);
    }

    private void notifyChanged(Uri uri) {
        // notify the SAF to to do a new queryChildDocuments
        SLogs.d(TAG, "notifyChanged()", "uri = " + uri);
        getContext().getContentResolver().notifyChange(uri, null);
    }

    private final CompositeDisposable compositeDisposable = new CompositeDisposable();

    @Override
    public void shutdown() {
        super.shutdown();

        releaseResources();
    }

    private void releaseResources() {
        if (!compositeDisposable.isDisposed()) {
            compositeDisposable.clear();
        }
    }

    /**
     * Reduce column list to what we support.
     *
     * @param requested requested columns
     * @param supported supported columns
     * @return common elements of both.
     */
    private String[] netProjection(String[] requested, String[] supported) {
        if (requested == null) {
            return (supported);
        }

        ArrayList<String> result = new ArrayList<String>();

        for (String request : requested) {
            for (String support : supported) {
                if (request.equals(support)) {
                    result.add(request);
                    break;
                }
            }
        }

        return (result.toArray(new String[0]));
    }

}
