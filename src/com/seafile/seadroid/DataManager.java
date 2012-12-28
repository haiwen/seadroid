package com.seafile.seadroid;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Environment;
import android.util.Log;

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
        String purename = filename.substring(0, filename.lastIndexOf('.'));
        String suffix = filename.substring(filename.lastIndexOf('.') + 1);
        return purename + "-" + oid.substring(0, 8) + "." + suffix;  
    }
    
    static public File getFileForFileCache(String path, String oid) {
        String p = getExternalRootDirectory() + "/" + constructFileName(path, oid); 
        return new File(p);
    }
    
    static public File getTempFile(String path, String oid) {
        String p = getExternalTempDirectory() + "/" + constructFileName(path, oid); 
        return new File(p);
    }
    
    // Obtain a cache file for storing a directory with oid
    static public File getFileForDirentsCache(String oid) {
        return new File(getExternalCacheDirectory() + "/" + oid);
    }
    
    static public void saveDirentsToCache() {
        
    }
    
    private static final String DEBUG_TAG = "DataManager";

    private SeafConnection sc;
    private Account account;
    private Context context;
    
    
    HashMap<String, String> pathObjectIDMap = new HashMap<String, String>();
    List<SeafRepo> reposCache = null;
    
    public DataManager(Context cnt, Account act) {
        context = cnt;
        account = act;
        sc = new SeafConnection(act);
    }

    public Account getAccount() {
        return sc.getAccount();
    }
    
    private File getFileForReposCache() {
        String filename = "repos-" + (account.server + account.email).hashCode() + ".dat";
        return new File(getExternalCacheDirectory() + "/" + 
                filename);
    }
    
    private boolean networkOn() {
        ConnectivityManager connMgr = (ConnectivityManager) 
                context.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();

        if (networkInfo != null && networkInfo.isConnected()) {
            return true;
        } else
            return false;
    }
    
    
    private List<SeafRepo> parseRepos(String json) {
        try {
            JSONArray array = Utils.parseJsonArray(json);
            ArrayList<SeafRepo> repos = new ArrayList<SeafRepo>();
            for (int i = 0; i < array.length(); i++) {
                JSONObject obj = array.getJSONObject(i);
                SeafRepo repo = SeafRepo.fromJson(obj);
                if (repo != null)
                    repos.add(repo);
            }
            return repos;
        } catch (JSONException e) {
            Log.d(DEBUG_TAG, "repos: parse json error");
            return null;
        }
    }

    public List<SeafRepo> getCachedRepos() {
        return reposCache;
    }
    
    public SeafRepo getCachedRepo(int position) {
        return reposCache.get(position);
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
    
    public List<SeafRepo> getRepos() {
        if (!networkOn()) {
            if (reposCache != null)
                return reposCache;
            
            File cache = getFileForReposCache();
            if (cache.exists()) {
                String json = Utils.readFile(cache);
                reposCache = parseRepos(json);
                return reposCache;
            }
        }
        
        String json = sc.getRepos();
        if (json == null)
            return null;
        reposCache = parseRepos(json);
        
        try {
            File cache = getFileForReposCache();
            Utils.writeFile(cache, json);
        } catch (IOException e) {
            // ignore
        }
        
        return reposCache;
    }
    
    public interface ProgressMonitor {
        public void onProgressNotify(long total);
        boolean isCancelled();
    }
    
    public File getFile(String repoID, String path, String oid, ProgressMonitor monitor) {
        String p = getExternalRootDirectory() + "/" + constructFileName(path, oid); 
        File f = new File(p);
        if (f.exists())
            return f;
        return sc.getFile(repoID, path, oid, monitor);
    }

    private List<SeafDirent> parseDirents(String json) {
        try {
            JSONArray array = Utils.parseJsonArray(json);

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
    
    public List<SeafDirent> getDirents(String repoID, 
            String path, String objectID) {
        //Log.d(DEBUG_TAG, "getDirents " + repoID + ":" + path + ", " + objectID);
        
        if (objectID != null) {
            // put the mapping to cache for later usage.
            pathObjectIDMap.put(repoID + path, objectID);
        } else {
            objectID = pathObjectIDMap.get(repoID + path);
        }
        
        if (objectID != null) {
            File cache = getFileForDirentsCache(objectID);
            if (cache.exists()) {
                String json = Utils.readFile(cache);
                return parseDirents(json);
            }
        }
        
        String json = sc.getDirents(repoID, path);
        if (json == null)
            return null;
        List<SeafDirent> dirents = parseDirents(json);
        
        if (objectID != null) {
            try {
                File cache = getFileForDirentsCache(objectID);
                Utils.writeFile(cache, json);
            } catch (IOException e) {
                // ignore
            }
        }
        
        return dirents;
    }
    
}
