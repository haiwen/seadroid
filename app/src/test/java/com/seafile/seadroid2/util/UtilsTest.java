package com.seafile.seadroid2.util;

import android.text.TextUtils;

import com.seafile.seadroid2.enums.OfficeViewMode;
import com.seafile.seadroid2.framework.model.ServerInfo;
import com.seafile.seadroid2.framework.util.Utils;

import org.junit.Test;
import org.junit.Assert;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@RunWith(RobolectricTestRunner.class)
public class UtilsTest {
    @Test
    public void testJoinPaths() {
        String result = Utils.pathJoin("a", "b");
        Assert.assertEquals("a/b", result);

        result = Utils.pathJoin("a", "b/c");
        Assert.assertEquals("a/b/c", result);

        result = Utils.pathJoin("/a", "b/c");
        Assert.assertEquals("/a/b/c", result);

        result = Utils.pathJoin("/a", "/b/c");
        Assert.assertEquals("/a/b/c", result);

        result = Utils.pathJoin("/a/", "/b/c");
        Assert.assertEquals("/a/b/c", result);

        result = Utils.pathJoin("/a/", "/b/c/");
        Assert.assertEquals("/a/b/c/", result);

        result = Utils.pathJoin("/a", "/b/c", "d/e");
        Assert.assertEquals("/a/b/c/d/e", result);

        result = Utils.pathJoin("/a/", "/b/c/", "/d/e");
        Assert.assertEquals("/a/b/c/d/e", result);
    }

    @Test
    public void testReadableFileSize() {
        String result = Utils.readableFileSize(0);
        Assert.assertEquals("0 KB", result);

        result = Utils.readableFileSize(957498560);
        Assert.assertEquals("957.5 MB", result);

        result = Utils.readableFileSize(2579477837L);
        Assert.assertEquals("2.6 GB", result);

        result = Utils.readableFileSize(285008);
        Assert.assertEquals("285 KB", result);
    }


    @Test
    public void checkFormat() {
        String fileName = "logo.png@100px|auto";
        if (TextUtils.isEmpty(fileName)) {
            return;
        }

        int dotIndex = fileName.lastIndexOf(".");
        if (dotIndex == -1) {
            return;
        }

        String regex = "[^a-zA-Z0-9]"; // Whether it contains non-alphabetic-number characters
        String f = fileName.substring(dotIndex + 1);

        //if fileName = logo.png@100px|auto
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(f);
        if (!matcher.find()) {
            System.out.println(f);
            return;
        }

        int startIndex = matcher.start();
        if (startIndex == 0) {//not support if fileName = logo.@100px|auto
            System.out.println(fileName);
            return;
        }

        String fff = f.substring(0, startIndex);
        System.out.println(fff);
        return;
    }

    @Test
    public void getOfficeFileClickAction_ReturnsExternalWhenServerIsNull() {
        OfficeViewMode result = Utils.getOfficeFileClickAction("file.docx", null, OfficeViewMode.INTERNAL);
        Assert.assertEquals(OfficeViewMode.EXTERNAL, result);
    }

    @Test
    public void getOfficeFileClickAction_ReturnsExternalWhenOnlyOfficeNotEnabled() {
        ServerInfo serverInfo = new ServerInfo("https://example.com", "11.0.0", "seafile-basic,seafile-pro", null);
        OfficeViewMode result = Utils.getOfficeFileClickAction("file.docx", serverInfo, OfficeViewMode.INTERNAL);
        Assert.assertEquals(OfficeViewMode.EXTERNAL, result);
    }

    @Test
    public void getOfficeFileClickAction_ReturnsExternalWhenOnlyOfficeEnabledButNonOfficeFile() {
        ServerInfo serverInfo = new ServerInfo("https://example.com", "11.0.0", "seafile-basic,onlyoffice,file-search", null);
        OfficeViewMode result = Utils.getOfficeFileClickAction("image.png", serverInfo, OfficeViewMode.INTERNAL);
        Assert.assertEquals(OfficeViewMode.EXTERNAL, result);
    }

    @Test
    public void getOfficeFileClickAction_ReturnsInternalWhenOnlyOfficeEnabledAndOfficeFileAndPrefInternal() {
        ServerInfo serverInfo = new ServerInfo("https://example.com", "11.0.0", "seafile-basic,onlyoffice,file-search", null);
        OfficeViewMode result = Utils.getOfficeFileClickAction("file.docx", serverInfo, OfficeViewMode.INTERNAL);
        Assert.assertEquals(OfficeViewMode.INTERNAL, result);
    }

    @Test
    public void getOfficeFileClickAction_ReturnsAskWhenOnlyOfficeEnabledAndOfficeFileAndPrefAsk() {
        ServerInfo serverInfo = new ServerInfo("https://example.com", "11.0.0", "seafile-basic,onlyoffice,file-search", null);
        OfficeViewMode result = Utils.getOfficeFileClickAction("file.docx", serverInfo, OfficeViewMode.ASK);
        Assert.assertEquals(OfficeViewMode.ASK, result);
    }

    @Test
    public void getOfficeFileClickAction_ReturnsExternalWhenOnlyOfficeEnabledAndOfficeFileAndPrefExternal() {
        ServerInfo serverInfo = new ServerInfo("https://example.com", "11.0.0", "seafile-basic,onlyoffice,file-search", null);
        OfficeViewMode result = Utils.getOfficeFileClickAction("file.docx", serverInfo, OfficeViewMode.EXTERNAL);
        Assert.assertEquals(OfficeViewMode.EXTERNAL, result);
    }
}