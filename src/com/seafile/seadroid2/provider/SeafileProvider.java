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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.annotation.TargetApi;
import android.content.res.AssetFileDescriptor;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.graphics.Bitmap;
import android.graphics.Point;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.CancellationSignal;
import android.os.Handler;
import android.os.ParcelFileDescriptor;
import android.provider.DocumentsContract;
import android.provider.DocumentsContract.Document;
import android.provider.DocumentsContract.Root;
import android.provider.DocumentsProvider;
import android.util.Log;

import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountDBHelper;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ProgressMonitor;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;

/**
 * DocumentProvider for the Storage Access Framework.
 *
 * It depends on API level 19.
 *
 * This Provider gives access to other Apps to browse, read and write all files
 * contained in Seafile repositories.
 *
 */
@TargetApi(Build.VERSION_CODES.KITKAT)
public class SeafileProvider extends DocumentsProvider {

    private static final String[] SUPPORTED_ROOT_PROJECTION = new String[] {
            Root.COLUMN_ROOT_ID,
            Root.COLUMN_FLAGS,
            Root.COLUMN_TITLE,
            Root.COLUMN_DOCUMENT_ID,
            Root.COLUMN_SUMMARY,
            Root.COLUMN_ICON
    };

    private static final String[] SUPPORTED_DOCUMENT_PROJECTION = new String[] {
                    Document.COLUMN_DOCUMENT_ID,
                    Document.COLUMN_MIME_TYPE,
                    Document.COLUMN_DISPLAY_NAME,
                    Document.COLUMN_LAST_MODIFIED,
                    Document.COLUMN_FLAGS,
                    Document.COLUMN_SIZE
            };
    
    /** we remember the last documentId queried so we don't run into a loop while doing Async lookups. */
    private String lastQueriedDocumentId = null;

    private DocumentIdParser docIdParser;

    @Override
    public boolean onCreate() {
        docIdParser = new DocumentIdParser(getContext());

        return true;
    }

    @Override
    public Cursor queryRoots(String[] projection)
            throws FileNotFoundException {

        String[] netProjection=
                netProjection(projection, SUPPORTED_ROOT_PROJECTION);
        MatrixCursor result=new MatrixCursor(netProjection);

        Log.d(getClass().getSimpleName(), "queryRoots()");

        // add a Root for every Seafile account we have.
        for(Account a: AccountDBHelper.getDatabaseHelper(getContext()).getAccountList()) {
            includeRoot(result, a);
        }

        // notification uri for the event, that the account list has changed
        result.setNotificationUri(getContext().getContentResolver(), AccountNotifier.NOTIFICATION_URI);

        return result;
    }

    @Override
    public Cursor queryChildDocuments(String parentDocumentId,
                                      String[] projection,
                                      String sortOrder)
            throws FileNotFoundException {

        Log.d(getClass().getSimpleName(), "queryChildDocuments: " + parentDocumentId);

        String[] netProjection = 
                netProjection(projection, SUPPORTED_DOCUMENT_PROJECTION);

        DataManager dm = createDataManager(parentDocumentId);

        String repoId = DocumentIdParser.getRepoIdFromId(parentDocumentId);

        if (repoId.isEmpty()) {
            // in this case the user is asking for a list of repositories

            MatrixCursor result;

            // fetch a new repo list in the background
            if (!parentDocumentId.equals(lastQueriedDocumentId)) {
                result = createCursor(netProjection, true);
                lastQueriedDocumentId = parentDocumentId;

                fetchReposAsync(dm, result);

            } else {
                result = createCursor(netProjection, false);
            }

            // in the meantime, return the cached repos
            List<SeafRepo> repoList = dm.getReposFromCache();
            if (repoList != null) {
                for (SeafRepo repo : repoList) {
                    includeRepo(result, dm.getAccount(), repo);
                }
            }
            return result;

        } else {
            // in this case, the repository is known. the user wants the entries of a specific
            // directory in the given repository.

            String path = DocumentIdParser.getPathFromId(parentDocumentId);

            MatrixCursor result;


            // fetch new dirents in the background
            if (!parentDocumentId.equals(lastQueriedDocumentId)) {

                lastQueriedDocumentId = parentDocumentId;

                result = createCursor(netProjection, true);

                fetchDirentAsync(dm, repoId, path, result);

            } else {
                result = createCursor(netProjection, false);
            }

            // in the meantime return cached ones
            List<SeafDirent> dirents = dm.getCachedDirents(repoId, path);
            if (dirents != null) {

                for (SeafDirent d : dirents) {
                    includeDirent(result, dm, repoId, path, d);
                }

            }
            return result;
        }

    }

    @Override
    public Cursor queryDocument(String documentId, String[] projection) throws FileNotFoundException {

        Log.d(getClass().getSimpleName(), "queryDocument: " + documentId);

        String[] netProjection = 
                netProjection(projection, SUPPORTED_DOCUMENT_PROJECTION);
        MatrixCursor result = new MatrixCursor(netProjection);

        DataManager dm = createDataManager(documentId);

        String repoId = DocumentIdParser.getRepoIdFromId(documentId);
        if (repoId.isEmpty()) {
            // the user has asked for the root, that contains all the repositories as children.
            // we don't have much to say about that "directory".

            includeRoot(result, dm.getAccount());
            return result;
        }

        // the android API asks us to be quick, so just use the cache.
        SeafRepo repo = dm.getCachedRepoByID(repoId);

        String path = DocumentIdParser.getPathFromId(documentId);

        if (path.equals(ProviderUtil.PATH_SEPERATOR)) {
            // this is the base of the repository. this is special, as we give back the information
            // about the repository itself, not some directory in it.
            includeRepo(result, dm.getAccount(), repo);
        } else {
            // the generic case. a query about a file/directory in a repository.

            // again we only use cached info in this function. that shouldn't be an issue, as
            // very likely there has been a SeafileProvider.queryChildDocuments() call just moments
            // earlier.

            // the file might not be cached. try to find the file in the dirent of its parent directory.
            String parentPath = ProviderUtil.getParentDirFromPath(path);
            List<SeafDirent> dirents = dm.getCachedDirents(repo.getID(), parentPath);
            for (SeafDirent entry: dirents) {
                if (entry.getTitle().equals(ProviderUtil.getFileNameFromPath(path))) {
                    includeDirent(result, dm, repo.getID(), parentPath, entry);
                }
            }
        }


        return(result);
    }

    @Override
    public boolean isChildDocument(String parentId, String documentId) {
        return documentId.startsWith(parentId);
    }

    @Override
    public ParcelFileDescriptor openDocument(String documentId,
                                             String mode,
                                             final CancellationSignal signal)
            throws FileNotFoundException {


        DataManager dm = createDataManager(documentId);

        String repoId = DocumentIdParser.getRepoIdFromId(documentId);
        if (repoId.isEmpty()) {
            throw new FileNotFoundException("Cannot open directory.");
        }
        SeafRepo repo = dm.getCachedRepoByID(repoId); // we can assume that the repo is cached because the client has already seen it

        String path = DocumentIdParser.getPathFromId(documentId);

        try {
            // open the file. this might involve talking to the seafile server. this will hang unti
            // it is done.
            final File f = getFile(signal, dm, repo, path);

            // return the file to the client.
            return makeParcelFileDescriptor(f, mode);

        } catch (IOException e) {
            throw new FileNotFoundException(SeadroidApplication.getAppContext()
                    .getResources()
                    .getString(R.string.saf_open_file_exception, documentId));
        }
    }

    @Override
    public AssetFileDescriptor openDocumentThumbnail(String documentId,
                                                     Point sizeHint,
                                                     CancellationSignal signal)
            throws FileNotFoundException {

        DataManager dm = createDataManager(documentId);

        String repoId = DocumentIdParser.getRepoIdFromId(documentId);
        if (repoId.isEmpty()) {
            throw new FileNotFoundException(SeadroidApplication.getAppContext()
                    .getResources()
                    .getString(R.string.saf_open_directory_exception));
        }

        String path = DocumentIdParser.getPathFromId(documentId);

        try {
            // open the file. this might involve talking to the seafile server. this will hang until
            // it is done.

            DisplayImageOptions options = new DisplayImageOptions.Builder()
                    .extraForDownloader(dm.getAccount())
                    .cacheInMemory(true)
                    .cacheOnDisk(true)
                    .considerExifParams(true)
                    .build();

            String url = dm.getThumbnailLink(repoId, path, sizeHint.x);
            if (url == null)
                return null;

            final Bitmap bmp = ImageLoader.getInstance().loadImageSync(url, options);
            final ParcelFileDescriptor[] pair = ParcelFileDescriptor.createPipe();

            if (bmp == null) {
                throw new FileNotFoundException(SeadroidApplication.getAppContext()
                        .getResources()
                        .getString(R.string.saf_open_file_exception, documentId));
            }

            // writing into the file descriptor might block, so do it in another thread
            new Thread() {
                public void run() {
                    try {
                        FileOutputStream fileStream = new FileOutputStream(pair[1].getFileDescriptor());

                        bmp.compress(Bitmap.CompressFormat.PNG, 100, fileStream);
                        fileStream.close();

                    } catch (IOException e) {
                        Log.d(getClass().getSimpleName(), "could not transfer thumbnail.");
                    }
                }

            }.start();

            return new AssetFileDescriptor(pair[0], 0, AssetFileDescriptor.UNKNOWN_LENGTH);

        } catch (IOException e) {
            throw new FileNotFoundException(SeadroidApplication.getAppContext()
                    .getResources()
                    .getString(R.string.saf_open_file_exception, documentId));
        }
    }

    /**
     * Create a MatrixCursor with the option to enable the extraLoading flag.
     *
     * @param netProjection column list
     * @param extraLoading if true, the client will expect that more entries will arrive shortly.
     * @return the Cursor object
     */
    private static MatrixCursor createCursor(String[] netProjection, final boolean extraLoading) {
        return new MatrixCursor(netProjection) {
            @Override
            public Bundle getExtras () {
                Bundle b = new Bundle();
                b.putBoolean(DocumentsContract.EXTRA_LOADING, extraLoading);
                return b;
            }
        };
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

        Handler handler = new Handler(getContext().getMainLooper());

        return ParcelFileDescriptor.open(file, accessMode, handler,
                new ParcelFileDescriptor.OnCloseListener() {
                    @Override
                    public void onClose(IOException e) {
                        // TODO: if the file has been modified, it can now be uploaded to the server.
                    }

                });
    }

    /**
     * Load a file from the Seafile server.
     *
     * This might take a while, therefore we have to listen to the CancellationSignal and abort
     * if it says so.
     *
     * @param signal CancellationSignal
     * @param dm DataManager object belonging to the seafile account, where the file lies
     * @param repo The repository where the file lies
     * @param path File path
     * @return
     * @throws com.seafile.seadroid2.SeafException
     * @throws FileNotFoundException
     */
    private static File getFile(final CancellationSignal signal,
                                DataManager dm, 
                                SeafRepo repo, 
                                String path)
            throws FileNotFoundException {
        try {
            // fetch the file from the Seafile server.
            File f = dm.getFile(repo.getName(), repo.getID(), path, new ProgressMonitor() {
                @Override
                public void onProgressNotify(long total) {
                }

                @Override
                public boolean isCancelled() {
                    // cancel the download if the client has lost interest.
                    if (signal != null)
                        return signal.isCanceled();
                    else
                        return false;
                }
            });

            if (f == null) {
                throw new FileNotFoundException(SeadroidApplication
                        .getAppContext()
                        .getResources()
                        .getString(R.string.saf_file_not_downloaded_exception,
                                path));
            }

            if (f.isDirectory()) {
                throw new FileNotFoundException(SeadroidApplication
                        .getAppContext().getResources()
                        .getString(R.string.saf_write_diretory_exception));
            }

            return f;

        } catch (SeafException e) {
            throw new FileNotFoundException(SeadroidApplication
                    .getAppContext()
                    .getResources()
                    .getString(R.string.saf_file_not_downloaded_exception,
                            path));
        }

    }

    /**
     * Add a cursor entry for the account root.
     *
     * We don't know much about it.
     *
     * @param result the cursor to write the row into.
     * @param account the account to add.
     */
    private void includeRoot(MatrixCursor result, Account account) {
        String docId = DocumentIdParser.buildId(account, null, null);

        final MatrixCursor.RowBuilder row = result.newRow();

        row.add(Root.COLUMN_ROOT_ID, docId);
        row.add(Root.COLUMN_DOCUMENT_ID, docId);
        row.add(Root.COLUMN_ICON, R.drawable.ic_launcher);
        row.add(Root.COLUMN_FLAGS, Root.FLAG_SUPPORTS_IS_CHILD | Root.FLAG_SUPPORTS_CREATE);
        row.add(Root.COLUMN_TITLE, account.getServerHost());
        row.add(Root.COLUMN_SUMMARY, account.getEmail());
    }

    /**
     * Add a seafile repo to the cursor.
     *
     * @param result the cursor to write the row into.
     * @param account the account that contains the repo.
     * @param repo the repo to add.
     */
    private void includeRepo(MatrixCursor result, Account account, SeafRepo repo) {
        String docId = DocumentIdParser.buildId(account, repo.getID(), null);

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, repo.getTitle());
        row.add(Document.COLUMN_SIZE, repo.size);
        row.add(Document.COLUMN_MIME_TYPE, DocumentsContract.Document.MIME_TYPE_DIR);
        row.add(Document.COLUMN_LAST_MODIFIED, repo.mtime * 1000);
        row.add(Document.COLUMN_FLAGS, 0);
    }

    /**
     * add a dirent to the cursor.
     *
     * @param result the cursor to write the row into.
     * @param dm the dataMamager that belongs to the repo.
     * @param repoId the repoId of the seafile repo
     * @param parentPath the path of the parent directory
     * @param entry the seafile dirent to add
     */
    private void includeDirent(MatrixCursor result, DataManager dm, String repoId, String parentPath, SeafDirent entry) {
        String fullPath = parentPath + ProviderUtil.PATH_SEPERATOR + entry.getTitle();
        String docId = DocumentIdParser.buildId(dm.getAccount(), repoId, fullPath);

        final String mimeType = ProviderUtil.getTypeForFile(docId, entry.isDir());

        int flags = 0;
        // only offer a thumbnail if the file is an image and it is cached.
        if (mimeType.startsWith("image/")) {
            // Allow the image to be represented by a thumbnail rather than an icon
            flags |= Document.FLAG_SUPPORTS_THUMBNAIL;
        }

        final MatrixCursor.RowBuilder row = result.newRow();
        row.add(Document.COLUMN_DOCUMENT_ID, docId);
        row.add(Document.COLUMN_DISPLAY_NAME, entry.getTitle());
        row.add(Document.COLUMN_SIZE, entry.size);
        row.add(Document.COLUMN_MIME_TYPE, mimeType);
        row.add(Document.COLUMN_LAST_MODIFIED, entry.mtime * 1000);
        row.add(Document.COLUMN_FLAGS, flags);
    }
    
    /**
     * Fetches a dirent (list of entries of a directory) from Seafile asynchronously.
     *
     * This will return nothing. It will only signal the client over the MatrixCursor. The client
     * will then recall DocumentProvider.queryChildDocuments() again.
     *
     * @param dm the dataManager to be used.
     * @param repoId the Id of the repository
     * @param path the path of the directory.
     * @param result Cursor object over which to signal the client.
     */
    private void fetchDirentAsync(final DataManager dm, final String repoId, final String path, MatrixCursor result) {
        final Uri uri = DocumentsContract.buildChildDocumentsUri(ProviderUtil.AUTHORITY, dm.getAccount().getServerHost() + repoId + path);
        result.setNotificationUri(getContext().getContentResolver(), uri);

        new Thread(new Runnable() {
            public void run() {
                try {
                    // fetch the dirents from the server
                    dm.getDirentsFromServer(repoId, path);

                } catch (SeafException e) {
                    Log.e(getClass().getSimpleName(), "Exception while querying server", e);
                }
                // notify the client in any case.
                // XXX: the API is unclear about this. we could also let him wait forever.
                getContext().getContentResolver().notifyChange(uri, null);
            }
        }).start();
    }

    /**
     * Fetches a new list of repositories from Seafile asynchronously.
     *
     * This will return nothing. It will only signal the client over the MatrixCursor. The client
     * will then recall DocumentProvider.queryChildDocuments() again.
     *
     * @param dm the dataManager to be used.
     * @param result Cursor object over which to signal the client.
     */
    private void fetchReposAsync(final DataManager dm, MatrixCursor result) {
        final Uri uri = DocumentsContract.buildChildDocumentsUri(ProviderUtil.AUTHORITY, dm.getAccount().getServerHost());
        result.setNotificationUri(getContext().getContentResolver(), uri);

        new Thread(new Runnable() {
            public void run() {
                try {
                    // fetch new repositories from the server
                    dm.getReposFromServer();

                } catch (SeafException e) {
                    Log.e(getClass().getSimpleName(), "Exception while querying server", e);
                }
                // notify the client in any case.
                // XXX: the API is unclear about this. we could also let him wait forever.
                getContext().getContentResolver().notifyChange(uri, null);
            }
        }).start();
    }

    /**
     * Create a new DataManager (which gives us access to the Seafile cache and server).
     *
     * @param documentId documentId, must contain at least a serverName.
     * @return dataManager object.
     * @throws FileNotFoundException if documentId is bogus or the account does not exist.
     */
    private DataManager createDataManager(String documentId) throws FileNotFoundException {

        Account account = docIdParser.getAccountFromId(documentId);

        return new DataManager(account);
    }


    /**
     * Reduce column list to what we support.
     *
     * @param requested requested columns
     * @param supported supported columns
     * @return common elements of both.
     */
    private static String[] netProjection(String[] requested, String[] supported) {
        if (requested==null) {
            return(supported);
        }

        ArrayList<String> result=new ArrayList<String>();

        for (String request : requested) {
            for (String support : supported) {
                if (request.equals(support)) {
                    result.add(request);
                    break;
                }
            }
        }

        return(result.toArray(new String[0]));
    }

}
