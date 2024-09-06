package com.seafile.seadroid2.preferences;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.preference.PreferenceFragmentCompat;

public abstract class RenameSharePreferenceFragmentCompat extends PreferenceFragmentCompat {
    public abstract String getSharePreferenceSuffix();

    @Override
    public void onCreatePreferences(@Nullable Bundle savedInstanceState, @Nullable String rootKey) {

        String spName = PreferenceManagerCompat.getDefaultSharedPreferencesName(getContext(), getSharePreferenceSuffix());
        getPreferenceManager().setSharedPreferencesName(spName);
    }

}
