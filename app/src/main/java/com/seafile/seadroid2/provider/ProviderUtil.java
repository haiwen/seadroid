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

import android.provider.DocumentsContract;
import android.webkit.MimeTypeMap;

/**
 * Some parsing functions the DocumentProvider
 */
public class ProviderUtil {

    public static final String PATH_SEPERATOR = "/";
    public static final String AUTHORITY = "com.seafile.seadroid2";
    private static final String ARBITRARY_BINARY_DATA = "application/octet-stream";
    /**
     * get MIME type of file
     *
     * @param path file path.
     * @param isDir whether it is a directory
     * @return
     */
    public static String getTypeForFile(String path, boolean isDir) {
        if (isDir) {
            return DocumentsContract.Document.MIME_TYPE_DIR;
        } else {
            return getTypeForName(path);
        }
    }

    /**
     * Extract MIME Type of file ending.
     *
     * @param name file name
     * @return MIME type
     */
    public static String getTypeForName(String name) {
        int lastDot = name.lastIndexOf('.');
        if (lastDot >= 0) {
            String extension = name.substring(lastDot + 1).toLowerCase();
            String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(extension);
            if (mime != null) {
                return mime;
            }
        }
        return ARBITRARY_BINARY_DATA;
    }
    /**
     * takes a file path and returns the parent directory, excluding the last /.
     *
     * @param path some filepath.
     * @returns the parent directory.
     */
    public static String getParentDirFromPath(String path) {
        int lastSlash = path.lastIndexOf('/');
        return path.substring(0, lastSlash);
    }

    /**
     * Takes a file path and returns only the last filename component.
     * Works with files as well as with directories.
     *
     * @param path some filepath
     * @returns the filename.
     */
    public static String getFileNameFromPath(String path) {
        int lastSlash = path.lastIndexOf('/');
        return path.substring(lastSlash + 1);
    }


}
