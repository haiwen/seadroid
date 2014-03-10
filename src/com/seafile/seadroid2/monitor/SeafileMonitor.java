package com.seafile.seadroid2.monitor;

import java.util.Map;

import org.apache.commons.io.monitor.FileAlterationMonitor;

import com.google.common.collect.Maps;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.monitor.SeafileObserver.CachedFileChangedListener;

public class SeafileMonitor {

	private static final String DEBUG_TAG = "SeafileMonitor";
    // TODO: concurrency of observers map
	private Map<Account, SeafileObserver> observers = Maps.newHashMap();
	private FileAlterationMonitor alterationMonitor = new FileAlterationMonitor();;
	private CachedFileChangedListener listener;

	public SeafileMonitor(CachedFileChangedListener listener) {
		this.listener = listener;
	}

	public void monitorFilesForAccount(Account account) {
		if (observers.containsKey(account)) {
			return;
		}
		SeafileObserver fileObserver = new SeafileObserver(account, listener);
		addObserver(fileObserver);
		observers.put(account, fileObserver);
	}

	public void stopMonitorFilesForAccount(Account account) {
		SeafileObserver fileObserver = observers.get(account);
		removeObserver(fileObserver);
		observers.remove(account);
	}

	private void addObserver(SeafileObserver fileObserver) {
		alterationMonitor.addObserver(fileObserver.getAlterationObserver());
	}

	private void removeObserver(SeafileObserver fileObserver) {
		alterationMonitor.removeObserver(fileObserver.getAlterationObserver());
	}

    public void onFilesDownloaded(Account account, String repoID, String repoName,
                                  String filePathInRepo, String localPath) {
        SeafileObserver observer = observers.get(account);
        observer.watchDownloadedFile(repoID, repoName, filePathInRepo, localPath);
    }

	public void start() throws Exception {
		alterationMonitor.start();
	}

	public void stop() throws Exception {
		alterationMonitor.stop();
	}
}
