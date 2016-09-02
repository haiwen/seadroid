package com.seafile.seadroid2.util;

import org.junit.Test;
import org.junit.Assert;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.text.DecimalFormat;

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

}
