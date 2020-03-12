package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.cameraupload.CameraUploadManager;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.transfer.TransferService;

import java.util.ArrayList;
import java.util.List;

class SwitchStorageTask extends TaskDialog.Task {

    private static final String DEBUG_TAG = "SwitchStorageTask";

    private TransferService txService;
    private StorageManager.Location location = null;

    SwitchStorageTask() {
        Intent bIntent = new Intent(SeadroidApplication.getAppContext(), TransferService.class);
        SeadroidApplication.getAppContext().bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    public void setNewLocation(StorageManager.Location loc) {
        location = loc;
    }

    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

    @Override
    protected void runTask() {
        if (location == null)
            return;

        if (txService != null) {
            Log.d(DEBUG_TAG, "Cancel all TransferService tasks");
            txService.cancelAllUploadTasks();
            txService.cancellAllDownloadTasks();
        } else {
            return;
        }

        Context context = SeadroidApplication.getAppContext();

        CameraUploadManager camera = new CameraUploadManager(context);
        Account camAccount = camera.getCameraAccount();
        if (camera.isCameraUploadEnabled()) {
            Log.d(DEBUG_TAG, "Temporarily disable camera upload");
            camera.disableCameraUpload();
        }

        Log.i(DEBUG_TAG, "Switching storage to " + location.description);
        StorageManager.getInstance().setStorageDir(location.id);

        if (camAccount != null) {
            Log.d(DEBUG_TAG, "Reenable camera upload");
            camera.setCameraAccount(camAccount);
        }
    }
}

public class SwitchStorageTaskDialog extends TaskDialog {
    RadioGroup group;
    List<RadioButton> buttonList = new ArrayList<>();
    int currentLocationId = -1;

    SwitchStorageTask task;

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {

        // create task early to allow the service to connect
        task = new SwitchStorageTask();

        View view = inflater.inflate(R.layout.dialog_switch_storage, null);
        group = (RadioGroup) view.findViewById(R.id.storage_options);

        ArrayList<StorageManager.Location> options = StorageManager.getInstance().getStorageLocations();

        for (StorageManager.Location location: options) {
            RadioButton b = new RadioButton(getContext());
            b.setText(location.description);
            b.setTag(location);
            b.setEnabled(location.available);
            group.addView(b);
            buttonList.add(b);

            if (location.currentSelection)
                currentLocationId = b.getId();

        }
        group.check(currentLocationId);

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(getResources().getString(R.string.settings_cache_location_title));
    }

    @Override
    protected Task prepareTask() {
        // we can't stop the storage switch once it's started.
        disableCancel();

        int selectedId = group.getCheckedRadioButtonId();
        for (RadioButton b: buttonList) {
            if (b.getId() == selectedId) {
                StorageManager.Location location = (StorageManager.Location) b.getTag();
                task.setNewLocation(location);
                break;
            }
        }
        return task;
    }
}
