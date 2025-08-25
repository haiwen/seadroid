package com.seafile.seadroid2.framework.datastore;

import android.text.TextUtils;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.util.Utils;

import java.io.File;
import java.io.IOException;

public class DataManager {
    public static final long SET_PASSWORD_INTERVAL = 1000 * 60 * 60 * 24;//1 days

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
        return File.createTempFile("file-", ".tmp", StorageManager.getInstance().getTempDir());
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
    public static String getAccountMediaDir(Account account) {
        if (account == null) {
            return null;
        }

        String p = getAccountDir(account);

        String path = StorageManager.getInstance().getMediaDir().getAbsolutePath();

        return Utils.pathJoin(path, p);
    }

    private static String getAccountDir(Account account) {
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

        return p;
    }


    /**
     * @return local repo directory
     * @date 2025/5/20
     * @description local repo dir structure:
     * [account-dir]/[repo-id(0-4)]_[repo-name]
     * <p>
     * for example:<p>
     * repo_id: 1234567890987654321<p>
     * repo_name: My Library
     * <p>
     * The local repo dir will be:
     * /storage/emulated/0/Android/data/com.seafile.seadroid2(.debug)/foo@bar.com (cloud.seafile.com)/My Library_1234
     */
    public static File getLocalRepoDir(Account account, String repo_id, String repo_name) {
        if (TextUtils.isEmpty(repo_id)) {
            throw new IllegalArgumentException("repo_id is empty");
        }
        if (TextUtils.isEmpty(repo_name)) {
            throw new IllegalArgumentException("repo_name is empty");
        }

        String accountDir = DataManager.getAccountMediaDir(account);
        String repoDirName = String.format("%s_%s", repo_name, repo_id.substring(0, 4));
        return new File(accountDir, repoDirName);
    }

    /**
     * Each repo is placed under [account-dir]/[repo-name]. When a
     * file is downloaded, it's placed in its repo, with its full path.
     */
    public static File getLocalFileCachePath(Account account, String repoId, String repoName, String path) throws RuntimeException {
        String filePath = getLocalRepoDir(account, repoId, repoName).getAbsolutePath();

        String localPath = Utils.pathJoin(filePath, path);

        //build valid file path and name
        localPath = com.seafile.seadroid2.framework.util.FileUtils.buildValidFilePathName(localPath);

        File parentDir = new File(Utils.getParentPath(localPath));
        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        return new File(localPath);
    }
}