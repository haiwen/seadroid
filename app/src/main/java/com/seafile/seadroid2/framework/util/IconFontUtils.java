package com.seafile.seadroid2.framework.util;

import android.graphics.Typeface;

import com.blankj.utilcode.util.ResourceUtils;
import com.google.gson.Gson;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.model.iconfont.IconFontMapModel;
import com.seafile.seadroid2.framework.model.iconfont.IconFontModel;

import java.util.HashMap;

public class IconFontUtils {
    private static volatile Typeface mIconFontTypeFace = null;

    private static final HashMap<String, String> mIconFontMap = new HashMap<>();

    public static void initIconFont() {
        mIconFontTypeFace = Typeface.createFromAsset(SeadroidApplication.getAppContext().getAssets(), "iconfont.ttf");

        initIconFontMap();
    }

    public static Typeface getIconFontTypeFace() {
        if (mIconFontTypeFace == null) {
            synchronized (IconFontUtils.class) {
                if (mIconFontTypeFace == null) {
                    mIconFontTypeFace = Typeface.createFromAsset(SeadroidApplication.getAppContext().getAssets(), "iconfont.ttf");
                }
            }
        }
        return mIconFontTypeFace;
    }


    public static HashMap<String, String> getIconFontMap() {
        if (mIconFontMap.isEmpty()) {
            throw new IllegalArgumentException("no init iconfonts");
        }
        return mIconFontMap;
    }

    //
    public static String getIconFontDefaultMap() {
        if (mIconFontMap.isEmpty()) {
            throw new IllegalArgumentException("no init iconfonts");
        }
        return mIconFontMap.get("haiwen-book-bookmark-fill");
    }

    private static void initIconFontMap() {
        String msg = ResourceUtils.readAssets2String("iconfont.json");

        Gson gson = new Gson();
        IconFontModel model = gson.fromJson(msg, IconFontModel.class);

        mIconFontMap.clear();
        for (IconFontMapModel glyph : model.glyphs) {
            mIconFontMap.put(model.css_prefix_text + glyph.name, glyph.unicode);
        }
    }
}
