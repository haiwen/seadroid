package com.seafile.seadroid2.framework.datastore;

import android.text.TextUtils;

import com.blankj.utilcode.util.GsonUtils;
import com.google.common.collect.Maps;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Type;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class DataManager {

    //    private static final long SET_PASSWORD_INTERVAL = 59 * 60 * 1000; // 59 min
    // private static final long SET_PASSWORD_INTERVAL = 5 * 1000; // 5s
    public static final long SET_PASSWORD_INTERVAL = 1000 * 60 * 60 * 24;//1 days

    // pull to refresh
    public static final String PULL_TO_REFRESH_LAST_TIME_FOR_REPOS_FRAGMENT = "repo fragment last update";
    public static final String PULL_TO_REFRESH_LAST_TIME_FOR_STARRED_FRAGMENT = "starred fragment last update ";

    private static SimpleDateFormat ptrDataFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private static Map<String, Long> direntsRefreshTimeMap = Maps.newHashMap();
    public static final long REFRESH_EXPIRATION_MSECS = 10 * 60 * 1000; // 10 mins
    public static long repoRefreshTimeStamp = 0;

    public static final int BUFFER_SIZE = 2 * 1024 * 1024;
    public static final int PAGE_SIZE = 25;

    //    private SeafConnection sc;
//    private Account account;
//    private static DatabaseHelper dbHelper;
    private static final StorageManager storageManager = StorageManager.getInstance();

    public DataManager() {
//        dbHelper = DatabaseHelper.getDatabaseHelper();
    }

    /**
     * Creates and returns a temporary file. It is guarantied that the file is unique and freshly
     * created. The caller has to delete that file himself.
     *
     * @return a newly created file.
     * @throws IOException if the file could not be created.
     */
    public static File createTempFile() throws IOException {
        return File.createTempFile("file-", ".tmp", storageManager.getTempDir());
    }

    /**
     * Creates and returns a temporary directory. It is guarantied that the directory is unique and
     * empty. The caller has to delete that directory himself.
     *
     * @return a newly created directory.
     * @throws IOException if the directory could not be created.
     */
    public static File createTempDir() throws IOException {
        String dirName = "dir-" + UUID.randomUUID();
        File dir = new File(storageManager.getTempDir(), dirName);
        if (dir.mkdir()) {
            return dir;
        } else {
            throw new IOException("Could not create temp directory");
        }
    }


    /**
     * The account directory structure of Seafile is like this:
     * <p>
     * StorageManager.getMediaDir()
     * |__ foo@gmail.com (cloud.seafile.com)
     * |__ Photos
     * |__ Musics
     * |__ ...
     * |__ foo@mycompany.com (seafile.mycompany.com)
     * |__ Documents
     * |__ Manuals
     * |__ ...
     * |__ ...
     * <p>
     * In the above directory, the user has used two accounts.
     * <p>
     * 1. One account has email "foo@gmail.com" and server
     * "cloud.seafile.com". Two repos, "Photos" and "Musics", has been
     * viewed.
     * <p>
     * 2. Another account has email "foo@mycompany.com", and server
     * "seafile.mycompany.com". Two repos, "Documents" and "Manuals", has
     * been viewed.
     */
    public static String getAccountDir(Account account) {
        if (account == null) {
            return null;
        }

        String username = account.email;
        String server = Utils.stripSlashes(account.getServerHost());

        // strip port, like :8000 in 192.168.1.116:8000
        if (server.contains(":")) {
            server = server.substring(0, server.indexOf(':'));
        }

        String p = String.format("%s (%s)", username, server);
        p = p.replaceAll("[^\\w\\d\\.@\\(\\) ]", "_");

        String path = StorageManager.getInstance().getMediaDir().getAbsolutePath();

        return Utils.pathJoin(path, p);
    }


    /**
     * The data format in DataStore is as follows:
     * <p>
     * repo-id::::new-repo-name
     * </p>
     */
    private static String getSpecialRepoDirMapping(Account account, String repo_id) {

        List<String> list = getRepoNameMaps(account);
        for (String set : list) {
            String[] sp = StringUtils.split(set, DataStoreKeys.SEPARATOR);
            if (repo_id.equals(sp[0])) {
                return sp[1];
            }
        }

        return null;
    }

    public static List<String> getRepoNameMaps(Account account) {

        String names = Settings.getUserSharedPreferences().getString(DataStoreKeys.DS_REPO_DIR_MAPPING, null);
//        String names = DataStoreManager.getInstanceByUser(account.getSignature()).readString(DataStoreKeys.DS_REPO_DIR_MAPPING);
        Type listType = new TypeToken<List<String>>() {
        }.getType();

        List<String> list = GsonUtils.fromJson(names, listType);
        if (null == list) {
            list = new ArrayList<>();
        }
        return list;
    }

    private static boolean checkSpecialRepoDirMapping(Account account, String repo_name) {
        List<String> list = getRepoNameMaps(account);
        for (String set : list) {
            String[] sp = StringUtils.split(set, DataStoreKeys.SEPARATOR);
            if (repo_name.equals(sp[1])) {
                return true;
            }
        }

        return false;
    }

    public static File getRepoDirMappingDataStore(Account account, String repo_id, String repo_name) {

        String accountDir = DataManager.getAccountDir(account);

        String repoDirName = getSpecialRepoDirMapping(account, repo_id);

        File repoDir;
        if (!TextUtils.isEmpty(repoDirName)) {
            repoDir = new File(accountDir, repoDirName);
            if (!repoDir.exists()) {
                if (!repoDir.mkdirs()) {
                    throw new RuntimeException("Could not create library directory " + repoDir);
                }
            }
        } else {
            String uniqueRepoName;
            int i = 0;
            while (true) {
                if (i == 0) {
                    uniqueRepoName = repo_name;
                } else {
                    uniqueRepoName = repo_name + " (" + i + ")";
                }

                boolean isDuplicate = checkSpecialRepoDirMapping(account, uniqueRepoName);
                repoDir = new File(accountDir, uniqueRepoName);
                if (!repoDir.exists() && !isDuplicate) {
                    break;
                }

                i++;
            }

            if (!repoDir.mkdirs()) {
                throw new RuntimeException("Could not create repo directory " + uniqueRepoName
                        + "Phone storage space is insufficient or too many " + uniqueRepoName + " directory in phone");
            }


            List<String> list = getRepoNameMaps(account);
            list.add(repo_id + DataStoreKeys.SEPARATOR + uniqueRepoName);
            String v = GsonUtils.toJson(list);

            Settings.getUserSharedPreferences().edit().putString(DataStoreKeys.DS_REPO_DIR_MAPPING, v).commit();
//            DataStoreManager.getInstanceByUser(account.getSignature()).writeString(DataStoreKeys.DS_REPO_DIR_MAPPING, v);
        }

        return repoDir;
    }


    public static File getLocalRepoFile(Account account, FileTransferEntity transferEntity) {
        File file = getRepoDirMappingDataStore(account, transferEntity.repo_id, transferEntity.repo_name);

        String localPath = Utils.pathJoin(file.getAbsolutePath(), transferEntity.full_path);

        //build valid file path and name
        localPath = com.seafile.seadroid2.framework.util.FileUtils.buildValidFilePathName(localPath);

        File parentDir = new File(Utils.getParentPath(localPath));
        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        return new File(localPath);
    }

    /**
     * Each repo is placed under [account-dir]/[repo-name]. When a
     * file is downloaded, it's placed in its repo, with its full path.
     */
    public static File getLocalRepoFile(Account account, String repoId, String repoName, String path) throws RuntimeException {
        File file = getRepoDirMappingDataStore(account, repoId, repoName);

        String localPath = Utils.pathJoin(file.getAbsolutePath(), path);

        //build valid file path and name
        localPath = com.seafile.seadroid2.framework.util.FileUtils.buildValidFilePathName(localPath);

        File parentDir = new File(Utils.getParentPath(localPath));
        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        return new File(localPath);
    }

    /**
     * Each repo is placed under [account-dir]/[repo-name].
     * When a file is downloaded, it's placed in its repo, with its full path.
     */
    public static File getLocalRepoPath(Account account, String repoId, String repoName) throws RuntimeException {
        File file = getRepoDirMappingDataStore(account, repoId, repoName);


        //build valid file path and name
        String localPath = com.seafile.seadroid2.framework.util.FileUtils.buildValidFilePathName(file.getAbsolutePath());

        File parentDir = new File(Utils.getParentPath(localPath));
        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        return new File(localPath);
    }


    /**
     * calculate if refresh time is expired, the expiration is 10 mins
     */
    public boolean isReposRefreshTimeout() {
        if (Utils.now() < repoRefreshTimeStamp + REFRESH_EXPIRATION_MSECS) {
            return false;
        }

        return true;
    }

    public boolean isDirentsRefreshTimeout(String repoID, String path) {
        if (!direntsRefreshTimeMap.containsKey(Utils.pathJoin(repoID, path))) {
            return true;
        }
        long lastRefreshTime = direntsRefreshTimeMap.get(Utils.pathJoin(repoID, path));

        if (Utils.now() < lastRefreshTime + REFRESH_EXPIRATION_MSECS) {
            return false;
        }
        return true;
    }

    public boolean isStarredFilesRefreshTimeout() {
        if (!direntsRefreshTimeMap.containsKey(PULL_TO_REFRESH_LAST_TIME_FOR_STARRED_FRAGMENT)) {
            return true;
        }
        long lastRefreshTime = direntsRefreshTimeMap.get(PULL_TO_REFRESH_LAST_TIME_FOR_STARRED_FRAGMENT);

        if (Utils.now() < lastRefreshTime + REFRESH_EXPIRATION_MSECS) {
            return false;
        }
        return true;
    }

    public void setDirsRefreshTimeStamp(String repoID, String path) {
        direntsRefreshTimeMap.put(Utils.pathJoin(repoID, path), Utils.now());
    }

    public void setReposRefreshTimeStamp() {
        repoRefreshTimeStamp = Utils.now();
    }

    public void saveLastPullToRefreshTime(long lastUpdateTime, String whichFragment) {
        direntsRefreshTimeMap.put(whichFragment, lastUpdateTime);
    }

    public String getLastPullToRefreshTime(String whichFragment) {

        if (!direntsRefreshTimeMap.containsKey(whichFragment)) {
            return null;
        }

        Long objLastUpdate = direntsRefreshTimeMap.get(whichFragment);
        if (objLastUpdate == null) return null;

        long lastUpdate = direntsRefreshTimeMap.get(whichFragment);

        long diffTime = new Date().getTime() - lastUpdate;
        int seconds = (int) (diffTime / 1000);
        if (diffTime < 0) {
            return null;
        }
        if (seconds <= 0) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        sb.append(SeadroidApplication.getAppContext().getString(R.string.pull_to_refresh_last_update));

        if (seconds < 60) {
            sb.append(SeadroidApplication.getAppContext().getString(R.string.pull_to_refresh_last_update_seconds_ago, seconds));
        } else {
            int minutes = (seconds / 60);
            if (minutes > 60) {
                int hours = minutes / 60;
                if (hours > 24) {
                    Date date = new Date(lastUpdate);
                    sb.append(ptrDataFormat.format(date));
                } else {
                    sb.append(SeadroidApplication.getAppContext().getString(R.string.pull_to_refresh_last_update_hours_ago, hours));
                }

            } else {
                sb.append(SeadroidApplication.getAppContext().getString(R.string.pull_to_refresh_last_update_minutes_ago, minutes));
            }
        }
        return sb.toString();
    }
}