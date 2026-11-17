package com.seafile.seadroid2.framework.notification;

import org.junit.Test;
import java.util.HashSet;
import java.util.Set;
import static org.junit.Assert.*;

public class TransferNotificationsTest {
    @Test
    public void sixStableIdentities() {
        Set<Integer> ids = new HashSet<>();
        for (NotificationKey key : NotificationKey.values()) {
            assertTrue(ids.add(key.id));
            assertSame(key, NotificationKey.from(key.source));
        }
        assertEquals(6, ids.size());
        assertEquals(3302, NotificationKey.SHARE.id);
        assertFalse(ids.contains(3001));
        assertFalse(ids.contains(4000));
        assertFalse(ids.contains(3002));
    }

    @Test
    public void notificationLifecycleDirectCalls() {
        TransferNotifications notices = TransferNotifications.get();
        NotificationKey key = NotificationKey.ALBUM;

        notices.begin(key);
        notices.scanning(key);
        notices.prepared(key);
        notices.startFile(key, "photo.jpg");
        notices.progress(key, 1, 50);
        notices.fileFinished(key, "photo.jpg", true, false);
        notices.finish(key, TransferNotifications.ResultState.SUCCESS);
    }
}
