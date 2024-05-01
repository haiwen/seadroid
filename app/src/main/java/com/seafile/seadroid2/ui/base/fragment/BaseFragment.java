package com.seafile.seadroid2.ui.base.fragment;

import androidx.fragment.app.Fragment;

import com.seafile.seadroid2.framework.util.SLogs;

public class BaseFragment extends Fragment {
    public void d(String e) {
        SLogs.d(this.getClass().getSimpleName() + " => " + e);
    }
}
