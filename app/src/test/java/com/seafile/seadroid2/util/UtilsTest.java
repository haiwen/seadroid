package com.seafile.seadroid2.util;

import org.junit.Test;
import org.junit.Assert;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

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
}
