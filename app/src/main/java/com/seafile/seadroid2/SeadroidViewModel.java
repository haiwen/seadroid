package com.seafile.seadroid2;

import android.app.Application;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;

import androidx.lifecycle.AndroidViewModel;

import com.seafile.seadroid2.transfer.TransferService;

public class SeadroidViewModel extends AndroidViewModel {
    @Override
    protected void onCleared() {
        super.onCleared();

    }

    private TransferService txService;
    private ServiceConnection serviceConnection;

    public SeadroidViewModel(Application application) {
        super(application);

        serviceConnection = new ServiceConnection() {
            @Override
            public void onServiceConnected(ComponentName name, IBinder service) {
                TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
                txService = binder.getService();
            }

            @Override
            public void onServiceDisconnected(ComponentName name) {
                txService = null;
            }
        };
    }

    public void bindService() {
        Intent intent = new Intent(getApplication(), TransferService.class);
        getApplication().startService(intent);
        getApplication().bindService(intent, serviceConnection, Context.BIND_AUTO_CREATE);
    }

    public void unbindService() {
        getApplication().unbindService(serviceConnection);
    }
}
