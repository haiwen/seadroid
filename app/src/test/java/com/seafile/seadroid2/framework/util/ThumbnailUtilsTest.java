package com.seafile.seadroid2.framework.util;

import static org.junit.Assert.*;

import android.app.Application;
import android.webkit.MimeTypeMap;
import org.junit.Before;
import org.robolectric.Shadows;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 28, manifest = Config.NONE, application = Application.class)
public class ThumbnailUtilsTest {
    @Before public void populateAndroidMimeTypes() {
        for (String extension : new String[]{"mp4", "mov", "m4v", "webm"}) {
            Shadows.shadowOf(MimeTypeMap.getSingleton()).addExtensionMimeTypMapping(extension, "video/" + extension);
        }
    }
    @Test public void cameraVideoExtensionsAreCaseInsensitive() {
        for (String name : new String[]{"clip.mp4", "clip.MP4", "clip.MOV", "clip.M4V", "clip.WebM", "clip.FLV"}) {
            assertTrue(name, Utils.isVideoFile(name));
            assertTrue(name, Utils.availableThumbnail(name));
        }
        assertFalse(Utils.isVideoFile("notes.txt"));
        assertFalse(Utils.isVideoFile("no-extension"));
    }

    @Test public void usesServerProvidedPreviewWithoutDoubleEncoding() {
        DirentModel video = new DirentModel();
        video.name = "clip.MOV";
        video.encoded_thumbnail_src = "thumbnail/repo/512/My%20videos/clip.MOV";
        assertEquals("https://example.com/seafile/thumbnail/repo/512/My%20videos/clip.MOV",
                ThumbnailUtils.convertThumbnailUrl("https://example.com/seafile", video));
    }

    @Test public void resolvesRootRelativeAndAbsolutePreviews() {
        assertEquals("https://example.com/thumbnail/clip.jpg",
                ThumbnailUtils.resolveThumbnailUrl("https://example.com/seafile/", "/thumbnail/clip.jpg"));
        assertEquals("https://example.com/thumbnail/clip.jpg",
                ThumbnailUtils.resolveThumbnailUrl("https://example.com", "https://example.com/thumbnail/clip.jpg"));
    }

    @Test public void rejectsPreviewUrlsThatWouldLeakCredentials() {
        assertNull(ThumbnailUtils.resolveThumbnailUrl("https://example.com", "https://other.test/thumbnail"));
        assertNull(ThumbnailUtils.resolveThumbnailUrl("https://example.com", "//other.test/thumbnail"));
        assertNull(ThumbnailUtils.resolveThumbnailUrl("https://example.com", "http://example.com/thumbnail"));
        assertNull(ThumbnailUtils.resolveThumbnailUrl("https://example.com", "https://example.com:444/thumbnail"));
    }

    @Test public void newEndpointPreservesFoldersAndEncodesReservedCharacters() {
        assertEquals("https://example.com/thumbnail/repo/256/My%20videos/a%2Bb%23%25.MOV",
                ThumbnailUtils.getNewThumbnailUrl("https://example.com/", "repo", "/My videos/a+b#%.MOV"));
    }

    @Test public void thumbnailServerIsSupportedStartingWithVersion13() {
        assertFalse(ThumbnailUtils.supportsNewThumbnailApi(null));
        assertFalse(ThumbnailUtils.supportsNewThumbnailApi("12.0.13"));
        assertTrue(ThumbnailUtils.supportsNewThumbnailApi("13.0.0"));
        assertTrue(ThumbnailUtils.supportsNewThumbnailApi("13.0.12"));
        assertTrue(ThumbnailUtils.supportsNewThumbnailApi("14.0.0"));
    }
}
