package com.seafile.seadroid2.util;

import com.seafile.seadroid2.framework.db.dao.FileCacheStatusDAO;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.util.FileCacheHelper;
import com.seafile.seadroid2.ui.file.FileService;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.robolectric.RobolectricTestRunner;

import java.util.ArrayList;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.observers.TestObserver;

import static org.mockito.Mockito.when;

@RunWith(RobolectricTestRunner.class)
public class FileCacheHelperTest {

    @Mock
    private FileService mockFileService;

    @Mock
    private FileCacheStatusDAO mockDao;

    @Before
    public void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void checkRemoteAndOpen_NotCached() {
        String repoId = "repo1";
        String path = "/path/1";

        // DB returns empty list
        when(mockDao.getByFullPath(repoId, path)).thenReturn(Single.just(new ArrayList<>()));
        when(mockFileService.getFileDetail(repoId, path)).thenReturn(Single.just(new DirentFileModel()));

        TestObserver<String> testObserver = FileCacheHelper.checkRemoteAndOpen(mockFileService, mockDao, repoId, path).test();

        // Should return an empty string since the file is not cached.
        testObserver.assertValue("");
        testObserver.assertComplete();
    }

    @Test
    public void checkRemoteAndOpen_StaleCache() {
        String repoId = "repo1";
        String path = "/path/1";
        String localFileId = "local_id";
        String remoteFileId = "remote_id";

        // DB returns cache with local_id
        FileCacheStatusEntity entity = new FileCacheStatusEntity();
        entity.file_id = localFileId;
        List<FileCacheStatusEntity> entities = new ArrayList<>();
        entities.add(entity);
        when(mockDao.getByFullPath(repoId, path)).thenReturn(Single.just(entities));

        // Server returns different id
        DirentFileModel remoteModel = new DirentFileModel();
        remoteModel.id = remoteFileId;
        when(mockFileService.getFileDetail(repoId, path)).thenReturn(Single.just(remoteModel));

        TestObserver<String> testObserver = FileCacheHelper.checkRemoteAndOpen(mockFileService, mockDao, repoId, path).test();

        // Should return an empty string since the cached copy is stale.
        testObserver.assertValue("");
        testObserver.assertComplete();
    }

    @Test
    public void checkRemoteAndOpen_CachedAndUpToDate() {
        String repoId = "repo1";
        String path = "/path/1";
        String sameFileId = "same_id";

        // DB returns cache with same_id
        FileCacheStatusEntity entity = new FileCacheStatusEntity();
        entity.file_id = sameFileId;
        List<FileCacheStatusEntity> entities = new ArrayList<>();
        entities.add(entity);
        when(mockDao.getByFullPath(repoId, path)).thenReturn(Single.just(entities));

        // Server returns same_id
        DirentFileModel remoteModel = new DirentFileModel();
        remoteModel.id = sameFileId;
        when(mockFileService.getFileDetail(repoId, path)).thenReturn(Single.just(remoteModel));

        TestObserver<String> testObserver = FileCacheHelper.checkRemoteAndOpen(mockFileService, mockDao, repoId, path).test();

        // Should return the file id since we can use the cached copy.
        testObserver.assertValue(sameFileId);
        testObserver.assertComplete();
    }
}
