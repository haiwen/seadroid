/*
 * Copyright (C) 2014 Dariush Forouher
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

import android.content.Context;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.util.Utils;

import java.io.FileNotFoundException;

/**
 * Helper class to create and parse DocumentIds for the DocumentProvider
 *
 * Format: FullServerServerSignature::RepoId::Path
 * Example:
 * email@adress.com@https://server.com/seafile/::::550e8400-e29b-11d4-a716-446655440000::::/dir/file.jpg
 *
 * the separation using "::::" is arbitrary. Is has to be something, that is neither in an URL
 * nor in a repoId UUID.
 *
 */
public class DocumentIdParser {

    /** used to separate serverName, RepoId and Path. */
    private static final String DOC_SEPERATOR = "::::";
    private static final String STARRED_FILE_REPO_ID = "starred-file-magic-repo";
    private static final String ROOT_REPO_ID = "root-magic-repo";

    Context context;
    AccountManager manager;

    public DocumentIdParser(Context context) {
        this.context = context;
        this.manager = new AccountManager(context);
    }

    /**
     * Extract the Seafile account from the documentId
     *
     * @param documentId our documentId, as created by createDocumentId()
     * @return the corresponding Account
     * @throws java.io.FileNotFoundException if the documentId is bogus or the account doesn't exist
     */
    public Account getAccountFromId(String documentId) throws FileNotFoundException {
        String[] list = documentId.split(DOC_SEPERATOR, 2);
        if (list.length > 0) {
            String server = list[0];
            for (Account a: manager.getAccountList()) {
                if (a.getSignature().equals(server)) {
                    return a;
                }
            }
        }
        throw new FileNotFoundException();
    }

    /**
     * extract the repoId from the given documentId
     *
     * @param documentId our documentId, as created by createDocumentId()
     * @return the repoId, might be empty string (if documentId isn't containing one)
     */
    public static String getRepoIdFromId(String documentId) {
        String[] list = documentId.split(DOC_SEPERATOR, 3);
        if (list.length>1) {
            String repoId = list[1];
            return repoId;
        }
        return "";
    }


    /**
     * extract the file path from the given documentId.
     *
     * that might be a directory or a file
     *
     * @param documentId our documentId, as created by createDocumentId()
     * @return a file path
     */
    public static String getPathFromId(String documentId) {
        String[] list = documentId.split(DOC_SEPERATOR, 3);
        if (list.length > 2) {
            String path = list[2];
            if (path.length() > 0)
                return path;
        }
        return Utils.PATH_SEPERATOR;
    }

    /**
     * create a documentId based on an account, a repoId and a file path.
     *
     * @param a the account object. must not be null.
     * @param repoId the repoId. May be null.
     * @param path The file path. May be null
     * @returns a documentId
     */
    public static String buildId(Account a, String repoId, String path) {
        if (repoId != null && path != null)
            return a.getSignature() + DOC_SEPERATOR + repoId + DOC_SEPERATOR + path;
        else if (repoId != null)
            return a.getSignature() + DOC_SEPERATOR + repoId;
        else
            return a.getSignature();
    }

    /**
     * create a documentId based on an account, a repoId and a file path.
     *
     * @param a the account object. must not be null.
     * @returns a documentId
     */
    public static String buildRootId(Account a) {
        return a.getSignature() + DOC_SEPERATOR + ROOT_REPO_ID;
    }

    public static String buildStarredFilesId(Account a) {
        return a.getSignature() + DOC_SEPERATOR + STARRED_FILE_REPO_ID;
    }

    public static boolean isRoot(String documentId) {
        return getRepoIdFromId(documentId).equals(ROOT_REPO_ID);
    }

    public static boolean isStarredFiles(String documentId) {
        return getRepoIdFromId(documentId).equals(STARRED_FILE_REPO_ID);
    }
}
