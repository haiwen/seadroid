package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import androidx.lifecycle.Observer;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.SwitchStorageViewModel;

import java.util.ArrayList;
import java.util.List;

public class SwitchStorageDialogFragment extends RequestCustomDialogFragmentWithVM<SwitchStorageViewModel> {

    private final List<RadioButton> buttonList = new ArrayList<>();
    private int currentLocationId = -1;
    private RadioGroup group;

    public static SwitchStorageDialogFragment newInstance() {

        Bundle args = new Bundle();

        SwitchStorageDialogFragment fragment = new SwitchStorageDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_switch_storage;
    }

    @Override
    protected void onPositiveClick() {
        StorageManager.Location newLocation = null;
        int selectedId = group.getCheckedRadioButtonId();
        for (RadioButton b : buttonList) {
            if (b.getId() == selectedId) {
                newLocation = (StorageManager.Location) b.getTag();
                break;
            }
        }

        if (newLocation == null || !newLocation.available) {
            Toasts.show(R.string.not_available);
            return;
        }

        getViewModel().switchStorage(requireContext(), newLocation);
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
            RadioGroup.LayoutParams vlp = new RadioGroup.LayoutParams(-1, -2);
            vlp.bottomMargin = Constants.DP.DP_8;
            group.addView(b, vlp);
            buttonList.add(b);

            if (location.selected)
                currentLocationId = b.getId();

        }
        group.check(currentLocationId);

    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getActionLiveData().observe(this, new Observer<String>() {
            @Override
            public void onChanged(String s) {
                refreshData();

                dismiss();
            }
        });

        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoading(aBoolean);
            }
        });
    }
}
