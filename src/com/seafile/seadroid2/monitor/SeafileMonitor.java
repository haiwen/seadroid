package com.seafile.seadroid2.monitor;

import java.util.Map;

import org.apache.commons.io.monitor.FileAlterationMonitor;

import com.google.common.collect.Maps;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.monitor.SeafileObserver.CachedFileChangedListener;

public class SeafileMonitor {

	private static final String DEBUG_TAG = "SeafileMonitor";
    // TODO: concurrency of observerMap
	private Map<Account, SeafileObserver> observerMap = Maps.newHashMap();
	private FileAlterationMonitor alterationMonitor;
	private CachedFileChangedListener listener;

	public SeafileMonitor(CachedFileChangedListener listener) {
		this.listener = listener;
		alterationMonitor = new FileAlterationMonitor();
	}

	public void monitorFilesForAccount(Account account) {
		if (observerMap.containsKey(account)) {
			return;
		}
		SeafileObserver fileObserver = new SeafileObserver(account, listener);
		addObserver(fileObserver);
		observerMap.put(account, fileObserver);
	}

	public void stopMonitorFilesForAccount(Account account) {
		SeafileObserver fileObserver = observerMap.get(account);
		removeObserver(fileObserver);
		observerMap.remove(account);
	}

	private void addObserver(SeafileObserver fileObserver) {
		alterationMonitor.addObserver(fileObserver.getAlterationObserver());
	}

	private void removeObserver(SeafileObserver fileObserver) {
		alterationMonitor.removeObserver(fileObserver.getAlterationObserver());
	}

    public void onFilesDownloaded(Account account, String repoID, String repoName,
                                  String filePathInRepo, String localPath) {
        SeafileObserver observer = observerMap.get(account);
        observer.watchDownloadedFile(repoID, repoName, filePathInRepo, localPath);
    }

	public void start() throws Exception {
		alterationMonitor.start();
	}

	public void stop() throws Exception {
		alterationMonitor.stop();
	}
}
