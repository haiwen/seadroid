package com.seafile.seadroid2.gallery;

import java.util.ArrayList;

import android.os.Bundle;

public class MonitoredActivity extends NoSearchActivity {

    private final ArrayList<LifeCycleListener> mListeners =
            new ArrayList<LifeCycleListener>();

    public static interface LifeCycleListener {
        public void onActivityCreated(MonitoredActivity activity);
        public void onActivityDestroyed(MonitoredActivity activity);
        public void onActivityStarted(MonitoredActivity activity);
        public void onActivityStopped(MonitoredActivity activity);
    }

    public static class LifeCycleAdapter implements LifeCycleListener {
        @Override
		public void onActivityCreated(MonitoredActivity activity) {
        }

        @Override
		public void onActivityDestroyed(MonitoredActivity activity) {
        }

        @Override
		public void onActivityStarted(MonitoredActivity activity) {
        }

        @Override
		public void onActivityStopped(MonitoredActivity activity) {
        }
    }

    public void addLifeCycleListener(LifeCycleListener listener) {
        if (mListeners.contains(listener)) return;
        mListeners.add(listener);
    }

    public void removeLifeCycleListener(LifeCycleListener listener) {
        mListeners.remove(listener);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        for (LifeCycleListener listener : mListeners) {
            listener.onActivityCreated(this);
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        for (LifeCycleListener listener : mListeners) {
            listener.onActivityDestroyed(this);
        }
    }

    @Override
    protected void onStart() {
        super.onStart();
        for (LifeCycleListener listener : mListeners) {
            listener.onActivityStarted(this);
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        for (LifeCycleListener listener : mListeners) {
            listener.onActivityStopped(this);
        }
    }
}
