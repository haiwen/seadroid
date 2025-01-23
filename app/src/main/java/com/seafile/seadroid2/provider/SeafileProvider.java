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

import static android.os.ParcelFileDescriptor.MODE_WRITE_ONLY;

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
import android.util.Log;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.request.RequestOptions;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.listener.ProgressListener;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import io.reactivex.Single;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;
import io.reactivex.schedulers.Schedulers;
import retrofit2.Call;
import retrofit2.Response;

/**
 * DocumentProvider for the Storage Access Framework.
 * <p>
 * It depends on API level 19 and supports API level 21.
 * <p>
 * This Provider gives access to other Apps to browse, read and write all files
 * contained in Seafile repositories.
 */
public class SeafileProvider extends DocumentsProvider {
    public static final String DEBUG_TAG = "SeafileProvider";

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

    /**
     * this flag is used to avoid infinite loops due to background refreshes
     */
    private boolean returnCachedData = false;

    private final Set<Account> reachableAccounts = new ConcurrentSkipListSet<Account>();

    private android.accounts.AccountManager androidAccountManager;

    private final Map<String, RepoModel> REPO_MAP = new HashMap<>();

    private RepoModel getRepoModelSync(String repoId) throws FileNotFoundException {
        if (!REPO_MAP.containsKey(repoId)) {
            List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoId);
            if (CollectionUtils.isEmpty(repoModels)) {
                throw new FileNotFoundException();
            }

            RepoModel repoModel = repoModels.get(0);

            REPO_MAP.put(repoId, repoModel);
            return repoModel;
        }
        return REPO_MAP.get(repoId);

    }

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

        String[] netProjection = netProjection(projection, SUPPORTED_ROOT_PROJECTION);
        MatrixCursor result = new MatrixCursor(netProjection);

        SLogs.d("queryRoots()");

        // add a Root for every signed in Seafile account we have.
        for (Account a : SupportAccountManager.getInstance().getAccountList()) {
            includeRoot(result, a);
        }

        // notification uri for the event, that the account list has changed
        result.setNotificationUri(getContext().getContentResolver(), NOTIFICATION_URI);

        return result;
    }

    /**
     * Create a MatrixCursor with the option to enable the extraLoading flag.
     *
     * @param netProjection column list
     * @param extraLoading  if true, the client will expect that more entries will arrive shortly.
     * @return the Cursor object
     */
    private static MatrixCursor createCursor(String[] netProjection, final boolean extraLoading, final boolean isReachable) {
        return new MatrixCursor(netProjection) {
            @Override
            public Bundle getExtras() {
                Bundle b = new Bundle();
                b.putBoolean(DocumentsContract.EXTRA_LOADING, extraLoading);
                if (!extraLoading && !isReachable) {
                    b.putString(DocumentsContract.EXTRA_ERROR, "Could not connect with server");
                }
                return b;
            }
        };
    }

    @Override
    public Cursor queryChildDocuments(String parentDocumentId, String[] projection, String sortOrder) throws FileNotFoundException {

        SLogs.d("queryChildDocuments: " + parentDocumentId);

        String[] netProjection = netProjection(projection, SUPPORTED_DOCUMENT_PROJECTION);

        Account account = DocumentIdParser.getAccountFromId(parentDocumentId);
        String repoId = DocumentIdParser.getRepoIdFromId(parentDocumentId);
        String path = DocumentIdParser.getPathFromId(parentDocumentId);

        if (repoId.isEmpty()) {
            // in this case the user is asking for a list of repositories

            MatrixCursor result;


            List<RepoModel> repos = AppDatabase.getInstance().repoDao().getListByAccountSync(account.getSignature());
            if (!CollectionUtils.isEmpty(repos)) {
                reachableAccounts.add(account);
                result = createCursor(netProjection, false, reachableAccounts.contains(account));
            } else {
                // fetch a new repo list in the background
                if (!returnCachedData) {
                    result = createCursor(netProjection, true, reachableAccounts.contains(account));
                    returnCachedData = true;
                    fetchReposAsync(account, result);
                } else {
                    result = createCursor(netProjection, false, reachableAccounts.contains(account));

                    returnCachedData = false;
                }
            }

            includeStarredFilesRepo(result, account);

            for (RepoModel repoModel : repos) {
                includeRepo(result, account, repoModel);
            }
            return result;

        } else if (DocumentIdParser.isStarredFiles(parentDocumentId)) {
            // the user is asking for the list of starred files

            MatrixCursor result;
            List<StarredModel> starredList = AppDatabase.getInstance().starredDirentDAO().getListByAccountSync(account.getSignature());
            if (!CollectionUtils.isEmpty(starredList)) {

            }
            if (!returnCachedData) {
                result = createCursor(netProjection, true, reachableAccounts.contains(account));
                returnCachedData = true;
                fetchStarredAsync(account, result);
            } else {
                result = createCursor(netProjection, false, reachableAccounts.contains(account));
                returnCachedData = false;
            }

            if (!CollectionUtils.isEmpty(starredList)) {
                for (StarredModel starredModel : starredList) {
                    includeStarredFileDirent(result, account, starredModel);
                }
            }
            return result;

        } else {
            // in this case, the repository is known. the user wants the entries of a specific
            // directory in the given repository.

            // the android API asks us to be quick, so just use the cache.

            RepoModel repoModel = getRepoModelSync(repoId);
            if (repoModel == null) {
                throw new FileNotFoundException();
            }

            // encrypted repos are not supported (we can't ask the user for the passphrase)
            if (repoModel.encrypted) {
                throw new FileNotFoundException();
            }

            MatrixCursor result;

            // fetch new dirents in the background
            if (!returnCachedData) {
                result = createCursor(netProjection, true, reachableAccounts.contains(account));
                returnCachedData = true;
                fetchDirentAsync(account, repoModel, path, result);
            } else {
                result = createCursor(netProjection, false, reachableAccounts.contains(account));
                returnCachedData = false;
            }

            if (!path.endsWith("/")) {
                path += "/";
            }
            List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByParentPathSync(repoId, path);
            // in the meantime return cached ones
            if (!CollectionUtils.isEmpty(direntModels)) {
                if (path.endsWith("/")) {
                    path = StringUtils.substringBeforeLast(path, "/");
                }

                for (DirentModel direntModel : direntModels) {
                    includeDirent(result, account, repoModel, path, direntModel);
                }
            }
            return result;
        }

    }

    @Override
    public Cursor queryDocument(String documentId, String[] projection) throws FileNotFoundException {

        SLogs.d("queryDocument: " + documentId);

        String[] netProjection = netProjection(projection, SUPPORTED_DOCUMENT_PROJECTION);
        MatrixCursor result = new MatrixCursor(netProjection);

        Account account = DocumentIdParser.getAccountFromId(documentId);
        String repoId = DocumentIdParser.getRepoIdFromId(documentId);
        String path = DocumentIdParser.getPathFromId(documentId);

        if (repoId.isEmpty()) {
            // the user has asked for the base document_id for a root

            includeDocIdRoot(result, account);
            return result;
        }

        // the android API asks us to be quick, so just use the cache.
        RepoModel repoModel = getRepoModelSync(repoId);
        if (repoModel == null) {
            throw new FileNotFoundException();
        }

        if (DocumentIdParser.isStarredFiles(documentId)) {
            includeStarredFilesRepo(result, account);
        } else if (path.equals(PATH_SEPARATOR)) {
            // this is the base of the repository. this is special, as we give back the information
            // about the repository itself, not some directory in it.
            includeRepo(result, account, repoModel);
        } else {
//             the general case. a query about a file/directory in a repository.

//             again we only use cached info in this function. that shouldn't be an issue, as
//             very likely there has been a SeafileProvider.queryChildDocuments() call just moments
//             earlier.

            // /同学/AccessKey_1406844958260436.csv
            String parentPath = Utils.getParentPath(path);
            String fileName = Utils.getFileNameFromPath(path);

            if (TextUtils.isEmpty(parentPath) || !parentPath.endsWith("/")) {
                parentPath += "/";
            }

            List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByParentPathSync(repoId, parentPath);
            List<StarredModel> starredModels = AppDatabase.getInstance().starredDirentDAO().getListByAccountSync(account.getSignature());

            if (!CollectionUtils.isEmpty(direntModels)) {
                // the file is in the dirent of the parent directory

                // look for the requested file in the dirents of the parent dir
                for (DirentModel direntModel : direntModels) {
                    if (direntModel.name.equals(fileName)) {
                        includeDirent(result, account, repoModel, parentPath, direntModel);
                        break;
                    }
                }
            } else if (!CollectionUtils.isEmpty(starredModels)) {
                //maybe the requested file is a starred file?

                // look for the requested file in the list of starred files
                for (StarredModel file : starredModels) {
                    if (file.path.equals(path)) {
                        includeStarredFileDirent(result, account, file);
                    }
                }
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
        Account account = DocumentIdParser.getAccountFromId(documentId);
        String path = DocumentIdParser.getPathFromId(documentId);
        String repoId = DocumentIdParser.getRepoIdFromId(documentId);

        RepoModel repoModel = getRepoModelSync(repoId);
        if (repoModel == null) {
            throw new FileNotFoundException();
        }

        if (TextUtils.isEmpty(path)) {
            throw new FileNotFoundException();
        }


        int accessMode = ParcelFileDescriptor.parseMode(mode);
        boolean writeOnly = (accessMode & MODE_WRITE_ONLY) != 0;
        if (writeOnly) {
            //So far write operations are not supported.
            throw new UnsupportedOperationException();
        }

        //TODO check sync_time and modified_at

        //check local
        File file = DataManager.getLocalRepoFile(account, repoModel.repo_id, repoModel.repo_name, path);
        if (file.exists()) {
            try {
                return makeParcelFileDescriptor(file, mode);
            } catch (IOException e) {
                Log.d(DEBUG_TAG, "could not open file", e);
                throw new FileNotFoundException();
            }
        }

        //local don't exists
        if (!NetworkUtils.isConnected()) {
            throw new FileNotFoundException();
        }

        // open the file. this might involve talking to the seafile server. this will hang until
        // it is done.
        final Future<ParcelFileDescriptor> future = ConcurrentAsyncTask.submit(new Callable<ParcelFileDescriptor>() {

            @Override
            public ParcelFileDescriptor call() throws Exception {
                File f = getFile(signal, account, repoModel, path);
                return makeParcelFileDescriptor(f, mode);
            }
        });

        if (signal != null) {
            signal.setOnCancelListener(new CancellationSignal.OnCancelListener() {
                @Override
                public void onCancel() {
                    Log.d(DEBUG_TAG, "openDocument cancelling download");
                    future.cancel(true);
                }
            });
        }

        try {
            return future.get();
        } catch (InterruptedException e) {
            Log.d(DEBUG_TAG, "openDocument cancelled download");
            throw new FileNotFoundException();
        } catch (CancellationException e) {
            Log.d(DEBUG_TAG, "openDocumentThumbnail cancelled download");
            throw new FileNotFoundException();
        } catch (ExecutionException e) {
            Log.d(DEBUG_TAG, "could not open file", e);
            throw new FileNotFoundException();
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

    /**
     * Load a file from the Seafile server.
     * <p>
     * This might take a while, therefore we have to listen to the CancellationSignal and abort
     * if it says so.
     *
     * @param signal  CancellationSignal
     * @param account account
     * @param repo    The repository where the file lies
     * @param path    File path
     * @return
     * @throws FileNotFoundException
     */
    private File getFile(final CancellationSignal signal, Account account, RepoModel repo, String path) throws FileNotFoundException {

        try {
            //get download url
            HttpIO httpIo = HttpIO.getInstanceByAccount(account);
            if (httpIo == null) {
                throw new FileNotFoundException();
            }
            Call<String> urlCall = httpIo.execute(FileService.class).getFileDownloadLinkSync(repo.repo_id, path, 1);

            Response<String> res = urlCall.execute();
            if (!res.isSuccessful()) {
                throw new FileNotFoundException();
            }

            String url = res.body();

            //download
            File targetFile = DataManager.getLocalRepoFile(account, repo.repo_id, repo.repo_name, path);
            SLogs.d("SeafileProvider: start download");
            SLogs.d("targetFile = " + targetFile);

            httpIo.downloadBinarySync(url, targetFile, new ProgressListener() {
                @Override
                public void onProgress(String fileName, long cur, long total) {
                    SLogs.d("fileName = " + fileName + ", cur = " + ", total = " + total);
                }

                @Override
                public boolean isCancelled() {
                    if (signal != null) {
                        return signal.isCanceled();
                    }
                    return false;
                }
            });

            if (targetFile.isDirectory()) {
                throw new FileNotFoundException();
            }

            return targetFile;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


    @Override
    public AssetFileDescriptor openDocumentThumbnail(String documentId, Point sizeHint, CancellationSignal signal) throws FileNotFoundException {

        if (TextUtils.isEmpty(documentId)) {
            SLogs.d("openDocumentThumbnail(): documentId is null.");
            throw new FileNotFoundException();
        }

        SLogs.d("openDocumentThumbnail(): " + documentId);

        Account account = DocumentIdParser.getAccountFromId(documentId);
        String path = DocumentIdParser.getPathFromId(documentId);
        String repoId = DocumentIdParser.getRepoIdFromId(documentId);

        if (repoId.isEmpty()) {
            throw new FileNotFoundException();
        }

        String mimeType = Utils.getFileMimeType(documentId);
        if (!mimeType.startsWith("image/")) {
            throw new FileNotFoundException();
        }

        RepoModel repoModel = getRepoModelSync(repoId);
        if (repoModel == null) {
            throw new FileNotFoundException();
        }


        final ParcelFileDescriptor[] pair;
        try {
            pair = ParcelFileDescriptor.createReliablePipe();
        } catch (IOException e) {
            throw new FileNotFoundException();
        }

        // do thumbnail download in another thread to avoid possible network access in UI thread
        Future<Bitmap> future = ConcurrentAsyncTask.submit(new Callable<Bitmap>() {
            @Override
            public Bitmap call() throws Exception {
                Bitmap bitmap = null;
                try (FileOutputStream fileStream = new FileOutputStream(pair[1].getFileDescriptor())) {

                    File localFile = DataManager.getLocalRepoFile(account, repoId, repoModel.repo_name, path);
                    String urlPath;
                    if (localFile.exists()) {
                        urlPath = "file://" + localFile.getAbsolutePath();
                        SLogs.d("urlPath = " + urlPath);
                    } else {
                        String pathEnc = URLEncoder.encode(path, "UTF-8");
                        urlPath = account.getServer() + String.format("api2/repos/%s/thumbnail/?p=%s&size=%s", repoId, pathEnc, sizeHint.x);
                        SLogs.d("urlPath = " + urlPath);
                    }

                    RequestOptions requestOptions = new RequestOptions()
                            .diskCacheStrategy(DiskCacheStrategy.ALL);

                    bitmap = GlideApp.with(getContext())
                            .asBitmap()
                            .apply(requestOptions)
                            .load(urlPath)
                            .centerCrop()
                            .submit(sizeHint.x, sizeHint.y)
                            .get();
                    if (bitmap != null) {
                        bitmap.compress(Bitmap.CompressFormat.PNG, 50, fileStream);
                    }

                } catch (UnsupportedEncodingException e) {
                    throw new RuntimeException(e);
                } catch (ExecutionException e) {
                    throw new RuntimeException(e);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                } finally {
                    IOUtils.closeQuietly(pair[1]);
                }

                return bitmap;
            }
        });

        if (signal != null) {
            signal.setOnCancelListener(new CancellationSignal.OnCancelListener() {
                @Override
                public void onCancel() {
                    Log.d(DEBUG_TAG, "openDocumentThumbnail() cancelling download");
                    future.cancel(true);
                    IOUtils.closeQuietly(pair[1]);
                }
            });
        }

        try {
            Bitmap bitmap = future.get();
            if (bitmap != null) {
                FileOutputStream fileStream = new FileOutputStream(pair[1].getFileDescriptor());
                bitmap.compress(Bitmap.CompressFormat.PNG, 100, fileStream);
            }

        } catch (ExecutionException e) {
            throw new RuntimeException(e);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        return new AssetFileDescriptor(pair[0], 0, AssetFileDescriptor.UNKNOWN_LENGTH);
    }

//    @Override
//    public String createDocument(String parentDocumentId, String mimeType, String displayName) throws FileNotFoundException {
//        Log.d(DEBUG_TAG, "createDocument: " + parentDocumentId + "; " + mimeType + "; " + displayName);
//
//        if (!Utils.isNetworkOn())
//            throw new FileNotFoundException();
//
//        String repoId = DocumentIdParser.getRepoIdFromId(parentDocumentId);
//        if (repoId.isEmpty()) {
//            throw new FileNotFoundException();
//        }
//
//        String parentPath = DocumentIdParser.getPathFromId(parentDocumentId);
//        DataManager dm = createDataManager(parentDocumentId);
//
//        try {
//
//            dm.getReposFromServer(); // refresh cache
//            SeafRepo repo = dm.getCachedRepoByID(repoId);
//
//            List<SeafDirent> list = dm.getDirentsFromServer(repoId, parentPath);
//            if (list == null) {
//                throw new SeafException(0, SeadroidApplication.getAppContext().getString(R.string.saf_write_diretory_exception));
//            }
//
//            // first check if target already exist. if yes, abort
//            for (SeafDirent e : list) {
//                if (e.getTitle().equals(displayName)) {
//                    throw new SeafException(0, SeadroidApplication.getAppContext().getString(R.string.saf_file_exist));
//                }
//            }
//
//            if (repo == null || !repo.hasWritePermission()) {
//                throw new SeafException(0, SeadroidApplication.getAppContext().getString(R.string.saf_write_diretory_exception));
//            } else if (mimeType == null) {
//                // bad mime type given by caller
//                throw new SeafException(0, SeadroidApplication.getAppContext().getString(R.string.saf_bad_mime_type));
//            } else if (mimeType.equals(Document.MIME_TYPE_DIR)) {
//                dm.createNewDir(repoId, parentPath, displayName);
//            } else {
//                dm.createNewFile(repoId, parentPath, displayName);
//            }
//
//            // update parent dirent cache
//            dm.getDirentsFromServer(repoId, parentPath);
//
//            return DocumentIdParser.buildId(dm.getAccount(), repoId, Utils.pathJoin(parentPath, displayName));
//
//        } catch (SeafException e) {
//            Log.d(DEBUG_TAG, "could not create file/dir", e);
//            throw new FileNotFoundException();
//        }
//    }


    /**
     * Add a cursor entry for the account root.
     * <p>
     * We don't know much about it.
     *
     * @param result  the cursor to write the row into.
     * @param account the account to add.
     */
    private void includeRoot(MatrixCursor result, Account account) {
        String docId = DocumentIdParser.buildId(account, null, null);
        String rootId = DocumentIdParser.buildRootId(account);

        SLogs.d("includeRoot - docId -> " + docId);
        SLogs.d("includeRoot - rootId -> " + rootId);

        final MatrixCursor.RowBuilder row = result.newRow();

        row.add(Root.COLUMN_ROOT_ID, rootId);
        row.add(Root.COLUMN_DOCUMENT_ID, docId);
        row.add(Root.COLUMN_ICON, R.drawable.ic_launcher);
        row.add(Root.COLUMN_FLAGS, Root.FLAG_SUPPORTS_IS_CHILD);//| Root.FLAG_SUPPORTS_CREATE
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
        String docId = DocumentIdParser.buildId(account, null, null);

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, account.getServerHost());
        row.add(Document.COLUMN_LAST_MODIFIED, null);
        row.add(Document.COLUMN_FLAGS, 0);
        row.add(Document.COLUMN_ICON, R.drawable.ic_launcher);
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
        String docId = DocumentIdParser.buildId(account, repoModel.repo_id, null);

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

        row.add(Document.COLUMN_SUMMARY, null);
//        if (RepoType.TYPE_MINE.equals(repoModel.type)) {
//            row.add(Document.COLUMN_SUMMARY, R.string.personal);
//        } else if (RepoType.TYPE_SHARED.equals(repoModel.type)) {
//            row.add(Document.COLUMN_SUMMARY, R.string.shared);
//        } else if (RepoType.TYPE_GROUP.equals(repoModel.type)) {
//            row.add(Document.COLUMN_SUMMARY, repoModel.group_name);
//        } else {
//            row.add(Document.COLUMN_SUMMARY, null);
//        }

        if (repoModel.encrypted || !reachableAccounts.contains(account)) {
            row.add(Document.COLUMN_MIME_TYPE, null); // undocumented: will grey out the entry
        } else {
            row.add(Document.COLUMN_MIME_TYPE, Document.MIME_TYPE_DIR);
        }
    }

    private void includeStarredFilesRepo(MatrixCursor result, Account account) {
        String docId = DocumentIdParser.buildStarredFilesId(account);

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);

        row.add(Document.COLUMN_DISPLAY_NAME, SeadroidApplication.getAppContext()
                .getResources().getString(R.string.tabs_starred));
        row.add(Document.COLUMN_ICON, R.drawable.star_normal);
        row.add(Document.COLUMN_FLAGS, 0);

        if (!reachableAccounts.contains(account)) {
            row.add(Document.COLUMN_MIME_TYPE, null); // undocumented: will grey out the entry
        } else {
            row.add(Document.COLUMN_MIME_TYPE, Document.MIME_TYPE_DIR);
        }

    }

    /**
     * add a dirent to the cursor.
     *
     * @param result     the cursor to write the row into.
     * @param parentPath the path of the parent directory
     * @param entry      the seafile dirent to add
     */
    private void includeDirent(MatrixCursor result, Account account, RepoModel repoModel, String parentPath, DirentModel entry) {
        String fullPath = Utils.pathJoin(parentPath, entry.name);

        String docId = DocumentIdParser.buildId(account, repoModel.repo_id, fullPath);

        String mimeType;
        if (entry.isDir())
            mimeType = DocumentsContract.Document.MIME_TYPE_DIR;
        else
            mimeType = Utils.getFileMimeType(docId);

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

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, entry.name);
        row.add(Document.COLUMN_SIZE, entry.size);
        row.add(Document.COLUMN_SUMMARY, null);
        row.add(Document.COLUMN_LAST_MODIFIED, entry.mtime * 1000);
        row.add(Document.COLUMN_FLAGS, flags);

        if (!reachableAccounts.contains(account)) {
            row.add(Document.COLUMN_MIME_TYPE, null); // undocumented: will grey out the entry
        } else {
            row.add(Document.COLUMN_MIME_TYPE, mimeType);
        }

    }

    /**
     * add a dirent to the cursor.
     *
     * @param result the cursor to write the row into.
     */
    private void includeStarredFileDirent(MatrixCursor result, Account account, StarredModel starredModel) {
        String docId = DocumentIdParser.buildId(account, starredModel.repo_id, starredModel.path);

        String mimeType;
        if (starredModel.is_dir) {
            mimeType = DocumentsContract.Document.MIME_TYPE_DIR;
        } else {
            mimeType = Utils.getFileMimeType(docId);
        }

        int flags = 0;
        // only offer a thumbnail if the file is an image
        if (mimeType.startsWith("image/")) {
            flags |= Document.FLAG_SUPPORTS_THUMBNAIL;
        }

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, starredModel.obj_name);
//        row.add(Document.COLUMN_SIZE, starredModel.getSize());
        row.add(Document.COLUMN_SUMMARY, null);
        row.add(Document.COLUMN_LAST_MODIFIED, starredModel.mtime_long);
        row.add(Document.COLUMN_FLAGS, flags);

        if (starredModel.repo_encrypted || !reachableAccounts.contains(account)) {
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
    private void fetchDirentAsync(Account account, RepoModel repoModel, String path, MatrixCursor result) {

        String id = DocumentIdParser.buildId(account, repoModel.repo_id, path);
        final Uri uri = DocumentsContract.buildChildDocumentsUri(AUTHORITY_OF_DOCUMENTS, id);

        result.setNotificationUri(getContext().getContentResolver(), uri);

        if (!path.endsWith("/")) {
            path = path + "/";
        }

        Single<List<DirentModel>> resultSingle = Objs.getDirentsSingleFromServer(account, repoModel.repo_id, repoModel.repo_name, path);
        Disposable disposable = resultSingle.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Consumer<List<DirentModel>>() {
                    @Override
                    public void accept(List<DirentModel> direntModels) throws Exception {
                        reachableAccounts.add(account);

                        notifyChanged(uri);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        reachableAccounts.remove(account);

                        notifyChanged(uri);
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
    private void fetchStarredAsync(Account account, MatrixCursor result) {

        String id = DocumentIdParser.buildStarredFilesId(account);
        final Uri uri = DocumentsContract.buildChildDocumentsUri(AUTHORITY_OF_DOCUMENTS, id);
        result.setNotificationUri(getContext().getContentResolver(), uri);

        Single<List<StarredModel>> listSingle = Objs.getStarredSingleFromServer(account);

        Disposable disposable = listSingle.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Consumer<List<StarredModel>>() {
                    @Override
                    public void accept(List<StarredModel> starredModels) throws Exception {
                        reachableAccounts.add(account);

                        notifyChanged(uri);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        reachableAccounts.remove(account);

                        notifyChanged(uri);
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
    private void fetchReposAsync(Account account, MatrixCursor result) {
        String id = DocumentIdParser.buildId(account, null, null);
        final Uri uri = DocumentsContract.buildChildDocumentsUri(AUTHORITY_OF_DOCUMENTS, id);
        result.setNotificationUri(getContext().getContentResolver(), uri);

        Single<List<BaseModel>> resultSingle = Objs.getReposSingleFromServer(account);
        Disposable disposable = resultSingle.subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Consumer<List<BaseModel>>() {
                    @Override
                    public void accept(List<BaseModel> baseModels) throws Exception {
                        reachableAccounts.add(account);

                        notifyChanged(uri);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        reachableAccounts.remove(account);

                        notifyChanged(uri);
                    }
                });
        compositeDisposable.add(disposable);
    }

    private void notifyChanged(Uri uri) {
        // The notification has to be sent only *after* queryChildDocuments has
        // finished. To be safe, wait a bit.
        try {
            Thread.sleep(100);
        } catch (InterruptedException e1) {
        }

        // notify the SAF to to do a new queryChildDocuments
        getContext().getContentResolver().notifyChange(uri, null);
    }

    private CompositeDisposable compositeDisposable = new CompositeDisposable();

//    @Override
//    public void shutdown() {
//        super.shutdown();
//
//        releaseResources();
//    }

    private void releaseResources() {
        if (compositeDisposable != null && !compositeDisposable.isDisposed()) {
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
