package com.seafile.seadroid2.data;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.Environment;
import android.util.Log;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;

public class DataManager {

    public static String getExternalRootDirectory() {
        if (Environment.getExternalStorageState().equals(Environment.MEDIA_MOUNTED)) {
            File extDir = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + "/Seafile/");
            if (extDir.mkdirs() || extDir.exists()) {
                return extDir.getAbsolutePath();
            } else {
                throw new RuntimeException("Couldn't create external directory");
            }
        } else {
            throw new RuntimeException("External Storage is currently not available");
        }
    }

    public static String getExternalTempDirectory() {
        String root = getExternalRootDirectory();
        File tmpDir = new File(root + "/" + "temp");
        if (tmpDir.exists())
            return tmpDir.getAbsolutePath();
        else {
            if (tmpDir.mkdirs() == false)
                throw new RuntimeException("Couldn't create external temp directory");
            else
                return tmpDir.getAbsolutePath();
        }
    }

    public static String getThumbDirectory() {
        String root = SeadroidApplication.getAppContext().getFilesDir().getAbsolutePath();
        File tmpDir = new File(root + "/" + "thumb");
        if (tmpDir.exists())
            return tmpDir.getAbsolutePath();
        else {
            if (tmpDir.mkdirs() == false)
                throw new RuntimeException("Couldn't create thumb directory");
            else
                return tmpDir.getAbsolutePath();
        }
    }

    public static String getExternalCacheDirectory() {
        String root = getExternalRootDirectory();
        File tmpDir = new File(root + "/" + "cache");
        if (tmpDir.exists())
            return tmpDir.getAbsolutePath();
        else {
            if (tmpDir.mkdirs() == false)
                throw new RuntimeException("Couldn't create external temp directory");
            else
                return tmpDir.getAbsolutePath();
        }
    }

    static public String constructFileName(String path, String oid) {
        String filename = path.substring(path.lastIndexOf("/") + 1);
        if (filename.contains(".")) {
            String purename = filename.substring(0, filename.lastIndexOf('.'));
            String suffix = filename.substring(filename.lastIndexOf('.') + 1);
            return purename + "-" + oid.substring(0, 8) + "." + suffix;
        } else {
            return filename + "-" + oid.substring(0, 8);
        }

    }

    static public File getFileForFileCache(String path, String oid) {
        String p = getExternalRootDirectory() + "/" + constructFileName(path, oid);
        return new File(p);
    }

    static public File getTempFile(String path, String oid) {
        String p = getExternalTempDirectory() + "/" + constructFileName(path, oid);
        return new File(p);
    }

    static public File getThumbFile(String oid) {
        String p = Utils.pathJoin(getThumbDirectory(), oid + ".png");
        return new File(p);
    }

    // Obtain a cache file for storing a directory with oid
    static public File getFileForDirentsCache(String oid) {
        return new File(getExternalCacheDirectory() + "/" + oid);
    }

    static public final int MAX_GEN_CACHE_THUMB = 1000000;  // Only generate thumb cache for files less than 1MB
    static public final int MAX_DIRECT_SHOW_THUMB = 100000;  // directly show thumb

    public void calculateThumbnail(String repoName, String repoID, String path, String oid) {
        try {
            final int THUMBNAIL_SIZE = 72;

            File file = getLocalRepoFile(repoName, repoID, path);
            if (!file.exists())
                return;
            if (file.length() > MAX_GEN_CACHE_THUMB)
                return;

            Bitmap imageBitmap = BitmapFactory.decodeStream(new FileInputStream(file));
            imageBitmap = Bitmap.createScaledBitmap(imageBitmap, THUMBNAIL_SIZE,
                    THUMBNAIL_SIZE, false);
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            imageBitmap.compress(Bitmap.CompressFormat.PNG, 100, baos);
            byte[] byteArray = baos.toByteArray();
            File thumb = getThumbFile(oid);
            FileOutputStream out = new FileOutputStream(thumb);
            out.write(byteArray);
            out.close();
        } catch (Exception ex) {

        }
    }

    /**
     * Caculate the thumbnail of an image directly when its size is less that
     * {@link #MAX_DIRECT_SHOW_THUMB}
     */
    public Bitmap getThumbnail(File file) {
        try {
            final int THUMBNAIL_SIZE = 72;

            if (!file.exists())
                return null;

            Bitmap imageBitmap = BitmapFactory.decodeStream(new FileInputStream(file));
            imageBitmap = Bitmap.createScaledBitmap(imageBitmap, THUMBNAIL_SIZE,
                    THUMBNAIL_SIZE, false);
            return imageBitmap;
        } catch (Exception ex) {
            return null;
        }
    }


    private static final String DEBUG_TAG = "DataManager";

    private SeafConnection sc;
    private Account account;
    private DatabaseHelper dbHelper;

    private class DirInfo {
        String dirID;
        long lastRefreshed;

        DirInfo(String dirID, long lastRefreshed) {
            this.dirID = dirID;
            this.lastRefreshed = lastRefreshed;
        }
    }

    static HashMap<String, DirInfo> dirLastUpdateMap = new HashMap<String, DirInfo>();
    List<SeafRepo> reposCache = null;
    // last time of repos update from server
    long lastRepoUpdate = 0;
    // force refresh interval
    private static final long REPOS_REFERSH_INTERVAL = 5 * 60 * 1000;
    private static final long DIR_REFERSH_INTERVAL = 1 * 60 * 1000;

    public DataManager(Account act) {
        account = act;
        sc = new SeafConnection(act);
        dbHelper = DatabaseHelper.getDatabaseHelper();
    }

    public Account getAccount() {
        return account;
    }

    private File getFileForReposCache() {
        String filename = "repos-" + (account.server + account.email).hashCode() + ".dat";
        return new File(getExternalCacheDirectory() + "/" +
                filename);
    }

    /**
     * The directory structure of Seafile on external storage is like this:
     *
     * /sdcard/Seafile
     *            |__ cache
     *            |__ temp
     *            |__ foo@gmail.com (cloud.seafile.com)
     *                      |__ Photos
     *                      |__ Musics
     *                      |__ ...
     *            |__ foo@mycompany.com (seafile.mycompany.com)
     *                      |__ Documents
     *                      |__ Manuals
     *                      |__ ...
     *            |__ ...
     *
     * In the above directory, the user has used two accounts.
     *
     * 1. One account has email "foo@gmail.com" and server
     * "cloud.seafile.com". Two repos, "Photos" and "Musics", has been
     * viewed.
     *
     * 2. Another account has email "foo@mycompany.com", and server
     * "seafile.mycompany.com". Two repos, "Documents" and "Manuals", has
     * been viewed.
     */
    private String getAccountDir() {

        String username = account.getEmail();
        String server = Utils.stripSlashes(account.getServerHost());
        // strip port, like :8000 in 192.168.1.116:8000
        if (server.indexOf(":") != -1)
            server = server.substring(0, server.indexOf(':'));
        String p = String.format("%s (%s)", username, server);
        p = p.replaceAll("[^\\w\\d\\.@\\(\\) ]", "_");
        String accountDir = Utils.pathJoin(getExternalRootDirectory(), p);
        return accountDir;
    }

    /**
     * Get the top dir of a repo. If there are multiple repos with same name,
     * say "ABC", their top dir would be "ABC", "ABC (1)", "ABC (2)", etc. The
     * mapping (repoName, repoID, dir) is stored in a database table.
     */
    private String getRepoDir(String repoName, String repoID) {
        File repoDir;

        // Check if there is a record in databse
        String path = dbHelper.getRepoDir(account, repoName, repoID);
        if (path != null) {
            // Has record in databse
            repoDir = new File(path);
            if (!repoDir.exists()) {
                if (repoDir.mkdirs() == false) {
                    throw new RuntimeException("Could not create library directory " + path);
                }
            }
            return path;
        }

        int i = 0;
        while (true) {
            String uniqueRepoName;
            if (i == 0) {
                uniqueRepoName = repoName;
            } else {
                uniqueRepoName = repoName + " (" + i + ")";
            }
            path = Utils.pathJoin(getAccountDir(), uniqueRepoName);
            repoDir = new File(path);
            if (!repoDir.exists() &&
                !dbHelper.repoDirExists(account, uniqueRepoName)) {
                // This repo dir does not exist yet, we can use it
                break;
            }
            i++;
        }

        if (repoDir.mkdirs() == false) {
            throw new RuntimeException("Could not create repo directory " + path);
        }

        // Save the new mapping in database
        dbHelper.saveRepoDirMapping(account, repoName, repoID, path);

        return repoDir.getPath();
    }

    /**
     * Each repo is places under [account-dir]/[repo-name]. When a
     * file is downloaded, it's placed in its repo, with it full path.
     * @param repoName
     * @param repoID
     * @param path
     */
    public File getLocalRepoFile(String repoName, String repoID, String path) {
        String localPath = Utils.pathJoin(getRepoDir(repoName, repoID), path);
        File parentDir = new File(Utils.getParentPath(localPath));
        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        return new File(localPath);
    }

    private List<SeafRepo> parseRepos(String json) {
        try {
            // may throw ClassCastException
            JSONArray array = Utils.parseJsonArray(json);
            if (array.length() == 0)
                return null;
            ArrayList<SeafRepo> repos = new ArrayList<SeafRepo>();
            for (int i = 0; i < array.length(); i++) {
                JSONObject obj = array.getJSONObject(i);
                SeafRepo repo = SeafRepo.fromJson(obj);
                if (repo != null)
                    repos.add(repo);
            }
            return repos;
        } catch (JSONException e) {
            Log.w(DEBUG_TAG, "repos: parse json error");
            return null;
        } catch (Exception e) {
            // other exception, for example ClassCastException
            return null;
        }
    }

    public List<SeafRepo> getCachedRepos() {
        return reposCache;
    }

    public SeafRepo getCachedRepo(int position) {
        return reposCache.get(position);
    }

    public SeafRepo getCachedRepoByID(String id) {
        List<SeafRepo> cachedRepos = getReposFromCache();
        if (cachedRepos == null) {
            return null;
        }

        for (SeafRepo repo: cachedRepos) {
            if (repo.getID().equals(id)) {
                return repo;
            }
        }

        return null;
    }

    public List<SeafRepo> getReposFromCache() {
        if (reposCache != null)
            return reposCache;

        File cache = getFileForReposCache();
        if (cache.exists()) {
            String json = Utils.readFile(cache);
            reposCache = parseRepos(json);
            return reposCache;
        }
        return null;
    }

    public List<SeafRepo> getRepos() throws SeafException {
        return getRepos(false);
    }

    public List<SeafRepo> getRepos(boolean forceRefresh) throws SeafException {
        boolean useCache = true;
        List<SeafRepo> repos;

        // First decide if use cache
        if (!Utils.isNetworkOn()) {
            useCache = true;
        } else {
            if (forceRefresh) {
                useCache = false;
            } else {
                long now = Calendar.getInstance().getTimeInMillis();
                if (now - lastRepoUpdate > REPOS_REFERSH_INTERVAL) {
                    useCache = false;
                }
            }
        }

        if (useCache) {
            repos = getReposFromCache();
            if (repos != null) {
                // Log.d(DEBUG_TAG, "get repos from cache");
                return repos;
            }
        }

        // Log.d(DEBUG_TAG, "get repos from server");
        String json = sc.getRepos();
        if (json == null)
            return null;

        reposCache = parseRepos(json);

        try {
            File cache = getFileForReposCache();
            Utils.writeFile(cache, json);
            lastRepoUpdate = Calendar.getInstance().getTimeInMillis();
        } catch (IOException e) {
            // ignore
        }

        return reposCache;
    }

    public interface ProgressMonitor {
        public void onProgressNotify(long total);
        boolean isCancelled();
    }

    public File getFile(String repoName, String repoID, String path,
                        ProgressMonitor monitor) throws SeafException {

        String cachedFileID = null;
        SeafCachedFile cf = getCachedFile(repoName, repoID, path);
        File localFile = getLocalRepoFile(repoName, repoID, path);
        // If local file is up to date, show it
        if (cf != null) {
            if (localFile.exists()) {
                cachedFileID = cf.fileID;
            }
        }

        TwoTuple<String, File> ret = sc.getFile(repoID, path, localFile.getPath(), cachedFileID, monitor);

        String fileID = ret.getFirst();
        if (fileID.equals(cachedFileID)) {
            // cache is valid
            return localFile;
        } else {
            File file = ret.getSecond();
            addCachedFile(repoName, repoID, path, fileID, file);
            return file;
        }
    }

    private List<SeafDirent> parseDirents(String json) {
        try {
            JSONArray array = Utils.parseJsonArray(json);
            if (array == null)
                return null;

            ArrayList<SeafDirent> dirents = new ArrayList<SeafDirent>();
            for (int i = 0; i < array.length(); i++) {
                JSONObject obj = array.getJSONObject(i);
                SeafDirent de = SeafDirent.fromJson(obj);
                if (de != null)
                    dirents.add(de);
            }
            return dirents;
        } catch (JSONException e) {
            return null;
        }
    }

    public List<SeafDirent> getDirents(String repoID, String path) throws SeafException {
        return getDirents(repoID, path, false);
    }

    private boolean shouldUseCachedDirents(String repoID, String path, boolean forceRefresh) {
        boolean useCache = true;
        DirInfo info = getDirCacheInfo(repoID, path);
        if (!Utils.isNetworkOn()) {
            useCache = true;
        } else {
            if (forceRefresh) {
                useCache = false;
            } else {
                long now = Calendar.getInstance().getTimeInMillis();
                if (info == null ||
                    now - info.lastRefreshed > DIR_REFERSH_INTERVAL) {
                    // Log.d(DEBUG_TAG,
                    //       String.format("path = %s, cache out of date", path));
                    useCache = false;
                }
            }
        }

        return useCache;
    }

    private String getCachedDirID(String repoID, String path) {
        String cachedDirID = null;
        DirInfo info = getDirCacheInfo(repoID, path);
        if (info != null) {
            cachedDirID = info.dirID;
        }

        File cache = null;
        if (cachedDirID != null) {
            cache = getFileForDirentsCache(cachedDirID);
            if (!cache.exists()) {
                cachedDirID = null;
                // Log.d(DEBUG_TAG,
                //     String.format("cache %s does not exist for path %s",
                //         cachedDirID, path));
            }
        }

        return cachedDirID;
    }

    public List<SeafDirent> getDirents(String repoID,
            String path, boolean forceRefresh) throws SeafException {

        boolean useCache = shouldUseCachedDirents(repoID, path, forceRefresh);

        // Get the ID of the cached dir. This is necessary even if useCache is
        // false, because if the cached dir ID is the same as the latest
        // version on the server, we don't need to get the content of the dir.
        String cachedDirID = getCachedDirID(repoID, path);

        if (useCache && cachedDirID != null) {
            // Log.d(DEBUG_TAG, "get dirents from cache, p = " + path);
            String json = Utils.readFile(getFileForDirentsCache(cachedDirID));
            return parseDirents(json);
        }

        // Log.d(DEBUG_TAG, "get dirents from server, p = " + path);
        TwoTuple<String, String> ret = sc.getDirents(repoID, path, cachedDirID);
        String newDirID = ret.getFirst();

        File cache = getFileForDirentsCache(newDirID);
        if (newDirID.equals(cachedDirID)) {
            // local cache is valid
            String json = Utils.readFile(cache);
            saveDirLastRefreshed(repoID, path, newDirID);
            return parseDirents(json);
        } else {
            // no cache
            String content = ret.getSecond();
            List<SeafDirent> dirents = parseDirents(content);

            try {
                Utils.writeFile(cache, content);
                saveDirLastRefreshed(repoID, path, newDirID);
            } catch (IOException e) {
                // ignore
            }
            return dirents;
        }
    }

    public SeafCachedFile getCachedFile(String repoName, String repoID, String path) {
        SeafCachedFile cf = dbHelper.getFileCacheItem(repoID, path, this);
        return cf;
    }

    public List<SeafCachedFile> getCachedFiles() {
        return dbHelper.getFileCacheItems(this);
    }

    public void addCachedFile(String repoName, String repoID, String path, String fileID, File file) {
        SeafCachedFile item = new SeafCachedFile();
        item.repoName = repoName;
        item.repoID = repoID;
        item.path = path;
        item.fileID = fileID;
        item.accountSignature = account.getSignature();
        dbHelper.saveFileCacheItem(item, this);
    }

    public void removeCachedFile(SeafCachedFile cf) {
        cf.file.delete();
        dbHelper.deleteFileCacheItem(cf);
    }

    public void setPassword(String repoID, String passwd) throws SeafException {
        sc.setPassword(repoID, passwd);
    }

    public void uploadFile(String repoID, String dir, String filePath,
            ProgressMonitor monitor) throws SeafException {
        sc.uploadFile(repoID, dir, filePath, monitor);
    }

    public String updateFile(String repoID, String dir, String filePath,
            ProgressMonitor monitor) throws SeafException {
        return sc.updateFile(repoID, dir, filePath, monitor);
    }

    /** Remove cached dirents from dir to the root.
     */
    public void invalidateCache(String repoID, String dir) {
        if (repoID == null || dir == null)
            return;

        String d = dir;
        while (true) {
            removeDirCacheInfo(repoID, d);
            if (d.equals("/"))
                break;
            d = Utils.getParentPath(d);
        }
    }

    private DirInfo getDirCacheInfo(String repoID, String path) {
        synchronized(dirLastUpdateMap) {
            return dirLastUpdateMap.get(repoID + path);
        }
    }

    private void saveDirLastRefreshed(String repoID, String path, String dirID) {
        long now = Utils.now();
        // Log.d(DEBUG_TAG, "path = " + path + ", time = " + now);
        DirInfo info = new DirInfo(dirID, now);
        synchronized(dirLastUpdateMap) {
            dirLastUpdateMap.put(repoID + path, info);
        }
    }

    private void removeDirCacheInfo(String repoID, String path) {
        synchronized(dirLastUpdateMap) {
            dirLastUpdateMap.remove(repoID + path);
        }
    }

    public void createNewDir(String repoID, String parentDir, String dirName) throws SeafException {
        TwoTuple<String, String> ret = sc.createNewDir(repoID, parentDir, dirName);
        if (ret == null) {
            return;
        }

        invalidateCache(repoID, parentDir);

        String newDirID = ret.getFirst();
        String response = ret.getSecond();
        // The response is the dirents of the parentDir after creating
        // the new dir. We save it to avoid request it again
        File cache = getFileForDirentsCache(newDirID);
        try {
            Utils.writeFile(cache, response);
            saveDirLastRefreshed(repoID, parentDir, newDirID);
        } catch (IOException e) {
        }
    }

    public void createNewFile(String repoID, String parentDir, String fileName) throws SeafException {
        TwoTuple<String, String> ret = sc.createNewFile(repoID, parentDir, fileName);
        if (ret == null) {
            return;
        }

        invalidateCache(repoID, parentDir);

        String newDirID = ret.getFirst();
        String response = ret.getSecond();
        // The response is the dirents of the parentDir after creating
        // the new dir. We save it to avoid request it again
        File cache = getFileForDirentsCache(newDirID);
        try {
            Utils.writeFile(cache, response);
            saveDirLastRefreshed(repoID, parentDir, newDirID);
        } catch (IOException e) {
        }
    }

    public File getLocalCachedFile(String repoName, String repoID, String filePath, String fileID) {
        File localFile = getLocalRepoFile(repoName, repoID, filePath);
        if (!localFile.exists()) {
            return null;
        }

        SeafCachedFile cf = getCachedFile(repoName, repoID, filePath);
        if (cf != null && fileID.equals(cf.fileID)) {
            return localFile;
        } else {
            return null;
        }
    }
}
