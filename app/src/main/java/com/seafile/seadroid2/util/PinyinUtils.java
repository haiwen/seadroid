/**
 * Copyright (C) 2011 Pinyin for Android Project
 * <p/>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p/>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p/>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.seafile.seadroid2.util;

import android.content.Context;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * This class is pinyin4android main interface .
 * there are two methods you can call them to convert the chinese to pinyin.
 * PinyinUtil.toPinyin(Context context,char c);
 * PinyinUtil.toPinyin(Context context,String hanzi);
 * <p/>
 * User: Ryan
 * Date: 11-5-29
 * Time: 21:13
 */
public abstract class PinyinUtils {
    /**
     * to convert chinese to pinyin
     *
     * @param context Android Context
     * @param c       the chinese character
     * @return pinyin
     */
    public static String toPinyin(Context context, char c) {
        if (c >= 'A' && c <= 'Z') {
            return String.valueOf((char) (c + 32));
        }
        if (c >= 'a' && c <= 'z') {
            return String.valueOf(c);
        }
        if (c == 0x3007) return "ling";
        if (c < 4E00 || c > 0x9FA5) {
            return null;
        }
        RandomAccessFile is = null;
        try {
            is = new RandomAccessFile(PinyinSource.getFile(context), "r");
            long sp = (c - 0x4E00) * 6;
            is.seek(sp);
            byte[] buf = new byte[6];
            is.read(buf);
            return new String(buf).trim();
        } catch (FileNotFoundException e) {
            //
        } catch (IOException e) {
            //
        } finally {
            try {
                if (null != is) is.close();
            } catch (IOException e) {
                //
            }
        }
        return null;
    }

    /**
     * to convert chinese to pinyin
     *
     * @param context Android Context
     * @param hanzi   the chinese string
     * @return pinyin
     */
    public static String toPinyin(Context context, String hanzi) {
        StringBuffer sb = new StringBuffer("");
        RandomAccessFile is = null;
        try {
            is = new RandomAccessFile(PinyinSource.getFile(context), "r");
            for (int i = 0; i < hanzi.length(); i++) {
                char ch = hanzi.charAt(i);
                if (ch >= 'A' && ch <= 'Z') {
                    sb.append((char) (ch + 32));
                    continue;
                }
                if (ch >= 'a' && ch <= 'z') {
                    sb.append(ch);
                    continue;
                }
                if (ch == 0x3007) {
                    sb.append("ling").append(' ');
                } else if (ch >= 0x4E00 || ch <= 0x9FA5) {
                    long sp = (ch - 0x4E00) * 6;
                    is.seek(sp);
                    byte[] buf = new byte[6];
                    is.read(buf);
                    sb.append(new String(buf).trim()).append(' ');
                }
            }
        } catch (IOException e) {
            //
        } finally {
            try {
                if (null != is) is.close();
            } catch (IOException e) {
                //
            }
        }
        return sb.toString().trim();
    }
}
