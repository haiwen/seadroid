package com.seafile.seadroid2.framework.service;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import static org.junit.Assert.*;

public class BackupFileStabilityTest {
    @Rule public TemporaryFolder temp = new TemporaryFolder();

    @Test public void unchangedFileIsReadyAfterSamplingInterval() throws IOException {
        File file = temp.newFile();
        assertTrue(BackupFileStability.awaitStable(file, millis -> assertEquals(1000, millis)));
    }

    @Test public void sameSizeRewriteIsDeferred() throws IOException {
        File file = temp.newFile();
        assertTrue(file.setLastModified(10000));
        assertFalse(BackupFileStability.awaitStable(file,
                millis -> assertTrue(file.setLastModified(20000))));
    }

    @Test public void growingSourceIsDeferredThenAcceptedOnNextAttempt() throws IOException {
        File file = temp.newFile();
        assertFalse(BackupFileStability.awaitStable(file, millis -> {
            try (RandomAccessFile writer = new RandomAccessFile(file, "rw")) {
                writer.setLength(10);
            } catch (IOException error) { throw new AssertionError(error); }
        }));
        assertTrue(BackupFileStability.awaitStable(file, millis -> { }));
    }

    @Test public void deletedSourceIsNotStable() throws IOException {
        File file = temp.newFile();
        assertFalse(BackupFileStability.awaitStable(file, millis -> assertTrue(file.delete())));
    }
}
