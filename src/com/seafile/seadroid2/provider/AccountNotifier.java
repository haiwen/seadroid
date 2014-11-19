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
import android.net.Uri;

import com.seafile.seadroid2.SeadroidApplication;

/**
 * Helper class over which the AccountDBHelper can notify the DocumentProvider that the account
 * list has changed.
 *
 */
public class AccountNotifier {

    /** This is a workaround
     *
     * the right thing to do would be this:
     * public static final Uri NOTIFICATION_URI = DocumentsContract.buildRootsUri(ProviderUtil.AUTHORITY);
     *
     * However that would bump the API level to 19. Therefore we build the identical RootsUri by hand.
     */
    public static final Uri NOTIFICATION_URI = new Uri.Builder().scheme("content").
            authority(ProviderUtil.AUTHORITY).appendEncodedPath("root").build();

    public static void notifyProvider() {
        Context c = SeadroidApplication.getAppContext();
        c.getContentResolver().notifyChange(NOTIFICATION_URI, null);
    }

}
