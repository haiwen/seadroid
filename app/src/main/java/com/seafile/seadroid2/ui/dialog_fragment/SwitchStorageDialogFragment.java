package com.seafile.seadroid2.ui.dialog_fragment;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import androidx.annotation.Nullable;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.ui.base.fragment.CustomDialogFragment;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.util.SLogs;

import java.util.ArrayList;
import java.util.List;

public class SwitchStorageDialogFragment extends CustomDialogFragment {

    private List<RadioButton> buttonList = new ArrayList<>();
    private int currentLocationId = -1;
    private RadioGroup group;

    public static SwitchStorageDialogFragment newInstance() {

        Bundle args = new Bundle();

        SwitchStorageDialogFragment fragment = new SwitchStorageDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Intent bIntent = new Intent(requireContext(), TransferService.class);
        requireContext().bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_switch_storage;
    }

    @Override
    protected void onPositiveClick() {
        int selectedId = group.getCheckedRadioButtonId();
        for (RadioButton b : buttonList) {
            if (b.getId() == selectedId) {
                location = (StorageManager.Location) b.getTag();
                break;
            }
        }

        run();
    }


    private TransferService txService;
    private StorageManager.Location location = null;
    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            SLogs.d("txService connected");

            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            SLogs.d("txService disconnected");

            txService = null;
        }
    };

    private void run() {
        if (location == null)
            return;

        if (txService != null) {
            SLogs.d("Cancel all TransferService tasks");
            txService.cancelAllUploadTasks();
            txService.cancelAllDownloadTasks();
        } else {
            return;
        }

        CameraUploadManager camera = new CameraUploadManager();
        Account camAccount = camera.getCameraAccount();
        if (camera.isCameraUploadEnabled()) {
            SLogs.d("Temporarily disable camera upload");
            camera.disableCameraUpload();
        }

        SLogs.d("Switching storage to " + location.description);
        StorageManager.getInstance().setStorageDir(location.id);

        if (camAccount != null) {
            SLogs.d("reEnable camera upload");
            camera.setCameraAccount(camAccount);
        }
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.settings_cache_location_title;
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        group = getDialogView().findViewById(R.id.storage_options);
        ArrayList<StorageManager.Location> options = StorageManager.getInstance().getStorageLocations();

        for (StorageManager.Location location : options) {
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

    }
}
