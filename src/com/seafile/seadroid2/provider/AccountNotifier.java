/*
 * Copyright (C) 2014 Dariush Forouher
 *
<<<<<<< HEAD
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
=======
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
>>>>>>> review
 */

package com.seafile.seadroid2.provider;

import android.content.Context;
import android.net.Uri;

import com.seafile.seadroid2.SeadroidApplication;

/**
<<<<<<< HEAD
 * Helper class over which the AccountDbHelper can notify the DocumentProvider that the account
=======
 * Helper class over which the AccountDBHelper can notify the DocumentProvider that the account
>>>>>>> review
 * list has changed.
 *
 */
public class AccountNotifier {

    /** This is a workaround
     *
     * the right thing to do would be this:
     * public static final Uri notificationUri = DocumentsContract.buildRootsUri("com.seafile.seadroid2");
     *
     * However that would bump the API level to 19. Therefore we build the identical RootsUri by hand.
     */
    public static final Uri notificationUri = new Uri.Builder().scheme("content").
            authority("com.seafile.seadroid2").appendEncodedPath("root").build();

    public static void notifyProvider() {
        Context c = SeadroidApplication.getAppContext();
        c.getContentResolver().notifyChange(notificationUri, null);
    }

}
