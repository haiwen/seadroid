package com.seafile.seadroid2.cache;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ui.ToastUtils;

import java.io.File;

/**
 *
 */
public class CacheDirectoryFragment extends Fragment {

    private SelectCacheDirectoryActivity mActivity;
    private FragmentManager fm;
    private CacheDirSelectionFragment mSelectionFragment;
    private Button mDoneBtn;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        mActivity = (SelectCacheDirectoryActivity) getActivity();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.cache_local_directory_fragment, null);

        fm = getChildFragmentManager();
        fm.beginTransaction()
                .add(R.id.cache_local_directory_list_container, getSelectionFragment())
                .commit();

        mDoneBtn = (Button) rootView.findViewById(R.id.cache_local_directory_btn);
        mDoneBtn.setOnClickListener(onClickListener);
        return rootView;
    }

    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            if (!DataManager.isExternalStorageWritable()) {
                ToastUtils.show(mActivity, R.string.settings_storage_not_available);
                return;
            } else {
                File customDir = new File(mSelectionFragment.getmCurrentDir() + "/Seafile");
                if (!customDir.exists()) {
                    if (!customDir.mkdirs()) {
                        ToastUtils.show(mActivity, R.string.settings_failed_write_data);
                        return;
                    }
                }
            }
            mActivity.saveCacheDirectory(mSelectionFragment.getmCurrentDir());
            mActivity.finish();
        }
    };

    /**
     * Instantiates a new fragment if mSelectionFragment is null.
     * Returns the current fragment, otherwise.
     */
    public CacheDirSelectionFragment getSelectionFragment() {
        if (mSelectionFragment == null) {
            mSelectionFragment = new CacheDirSelectionFragment();
        }

        return mSelectionFragment;
    }

}
