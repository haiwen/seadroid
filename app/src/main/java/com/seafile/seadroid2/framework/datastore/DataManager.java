package com.seafile.seadroid2.framework.datastore;

import android.text.TextUtils;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.google.common.io.Files;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class DataManager {

    // private static final long SET_PASSWORD_INTERVAL = 59 * 60 * 1000; // 59 min
    // private static final long SET_PASSWORD_INTERVAL = 5 * 1000; // 5s
    public static final long SET_PASSWORD_INTERVAL = 1000 * 60 * 60 * 24;//1 days
    private static final StorageManager storageManager = StorageManager.getInstance();

    public DataManager() {
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

    public static List<String> getRepoMapping() {

        String names = Settings.getCurrentAccountSharedPreferences().getString(DataStoreKeys.DS_REPO_DIR_MAPPING, null);
        Type listType = new TypeToken<List<String>>() {
        }.getType();

        List<String> list = GsonUtils.fromJson(names, listType);
        if (null == list) {
            list = new ArrayList<>();
        }
        return list;
    }

    /**
     * The data format in DataStore is as follows:
     * <p>
     * repo-id::::new-repo-name
     * </p>
     */
    private static String getSpecialRepoMappingName(String repo_id) {
        List<String> list = getRepoMapping();
        for (String set : list) {
            String[] sp = StringUtils.split(set, DataStoreKeys.SEPARATOR);
            if (repo_id.equals(sp[0])) {
                return sp[1];
            }
        }

        return null;
    }


    private static boolean checkSpecialRepoMappingDir(String repo_name) {
        List<String> list = getRepoMapping();
        for (String set : list) {
            String[] sp = StringUtils.split(set, DataStoreKeys.SEPARATOR);
            if (repo_name.equals(sp[1])) {
                return true;
            }
        }

        return false;
    }

    public static File renameRepoName(Account account, String repo_id, String new_repo_name) {
        String accountDir = DataManager.getAccountDir(account);
        File repoDir;
        String uniqueRepoName;
        int i = 0;
        while (true) {
            if (i == 0) {
                uniqueRepoName = new_repo_name;
            } else {
                uniqueRepoName = new_repo_name + " (" + i + ")";
            }

            boolean isDuplicate = checkSpecialRepoMappingDir(uniqueRepoName);
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

        List<String> list = getRepoMapping();
        List<String> list2 = new ArrayList<>();
        for (String set : list) {
            String[] sp = StringUtils.split(set, DataStoreKeys.SEPARATOR);
            if (repo_id.equals(sp[0])) {
                sp[1] = uniqueRepoName;
                list2.add(sp[0] + DataStoreKeys.SEPARATOR + sp[1]);
            } else {
                list2.add(set);
            }
        }

        String v = GsonUtils.toJson(list2);
        Settings.getCurrentAccountSharedPreferences().edit().putString(DataStoreKeys.DS_REPO_DIR_MAPPING, v).commit();

        return repoDir;
    }

    public static File getOrCreateRepoMappingDir(Account account, String repo_id, String repo_name) {

        String accountDir = DataManager.getAccountDir(account);
        String repoDirName = getSpecialRepoMappingName(repo_id);

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

                boolean isDuplicate = checkSpecialRepoMappingDir(uniqueRepoName);
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


            List<String> list = getRepoMapping();
            list.add(repo_id + DataStoreKeys.SEPARATOR + uniqueRepoName);
            String v = GsonUtils.toJson(list);

            Settings.getCurrentAccountSharedPreferences().edit().putString(DataStoreKeys.DS_REPO_DIR_MAPPING, v).commit();
        }

        return repoDir;
    }


    /**
     * Each repo is placed under [account-dir]/[repo-name]. When a
     * file is downloaded, it's placed in its repo, with its full path.
     */
    public static File getLocalRepoFile(Account account, String repoId, String repoName, String path) throws RuntimeException {
        File file = getOrCreateRepoMappingDir(account, repoId, repoName);

        String localPath = Utils.pathJoin(file.getAbsolutePath(), path);

        //build valid file path and name
        localPath = com.seafile.seadroid2.framework.util.FileUtils.buildValidFilePathName(localPath);

        File parentDir = new File(Utils.getParentPath(localPath));
        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        return new File(localPath);
    }
}