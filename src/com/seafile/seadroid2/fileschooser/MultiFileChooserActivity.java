package com.seafile.seadroid2.fileschooser;

import java.io.File;
import java.net.URISyntaxException;
import java.util.List;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.Utils;

import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.app.FragmentManager.BackStackEntry;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.widget.Toast;

public class MultiFileChooserActivity extends FragmentActivity implements
OnBackStackChangedListener {

    public static final String EXTERNAL_BASE_PATH = Environment
            .getExternalStorageDirectory().getAbsolutePath();
    public static final String PATH = "path";
    public static final String MULTI_FILES_PATHS = "com.seafile.seadroid2.fileschooser.paths";

    private String mPath;
    private FragmentManager mFragmentManager;
    private FileFooterFragment mFooterFragment;
    private List<File> mSelectedFiles;

    private BroadcastReceiver mStorageListener = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Toast.makeText(context, R.string.storage_removed, Toast.LENGTH_LONG).show();
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.multiple_files_chooser);

        mFragmentManager = getSupportFragmentManager();
        mFragmentManager.addOnBackStackChangedListener(this);

        if (savedInstanceState == null) {
            mPath = EXTERNAL_BASE_PATH;
            addFragment(mPath);
        } else {
            mPath = savedInstanceState.getString(PATH);
        }

        mFooterFragment = new FileFooterFragment();
        mFragmentManager.beginTransaction()
        .add(R.id.footer_fragment, mFooterFragment).commit();

        setTitle(mPath);
        mSelectedFiles = Lists.newArrayList();
    }

    @Override
    protected void onPause() {
        super.onPause();
        unregisterStorageListener();
    }

    @Override
    protected void onResume() {
        super.onResume();
        registerStorageListener();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);

        outState.putString(PATH, mPath);
    }

    private void addFragment(String path) {
        FileListFragment explorerFragment = FileListFragment.newInstance(mPath);
        mFragmentManager.beginTransaction()
                .add(R.id.explorer_fragment, explorerFragment).commit();
    }

    @Override
    public void onBackStackChanged() {
        mPath = EXTERNAL_BASE_PATH;

        int count = mFragmentManager.getBackStackEntryCount();
        if (count > 0) {
            BackStackEntry fragment = mFragmentManager
                    .getBackStackEntryAt(count - 1);
            mPath = fragment.getName();
        }

        setTitle(mPath);

    }

    public void onCancelButtonClicked() {
        setResult(RESULT_CANCELED);
        finish();
    }

    public void onConfirmButtonClicked() {
        File file;
        Uri uri;
        String path;
        String[] paths = new String[mSelectedFiles.size()];

        for (int i = 0; i < mSelectedFiles.size(); ++i) {
            file = mSelectedFiles.get(i);
            uri = Uri.fromFile(file);
            try {
                path = Utils.getPath(this, uri);
                paths[i] = path;
            } catch (URISyntaxException e) {
                e.printStackTrace();
                return;
            }

        }

        Intent intent = new Intent();
        intent.putExtra(MULTI_FILES_PATHS, paths);
        setResult(RESULT_OK, intent);
        finish();
    }

    private void updateSelectionStatus() {
        int nSelected = mSelectedFiles.size();
        String status;
        if (nSelected == 0) {
            status = getResources().getString(R.string.select_upload_files);
        } else {
            status = getResources().getQuantityString(R.plurals.n_upload_files_selected, nSelected, nSelected);
        }
        mFooterFragment.getStatusView().setText(status);
    }

    private void updateUploadButtonStatus() {
        int nSelected = mSelectedFiles.size();
        if (nSelected == 0) {
            mFooterFragment.getConfirmButton().setEnabled(false);
        } else {
            mFooterFragment.getConfirmButton().setEnabled(true);
        }
    }

    private void updateSelectedFileList(SelectableFile file) {
        File defaultFile = file.getFile();
        if (file.isSelected()) {
            mSelectedFiles.add(defaultFile);
        } else {
            if (mSelectedFiles.contains(defaultFile)) {
                int index = mSelectedFiles.indexOf(defaultFile);
                mSelectedFiles.remove(index);
            }
        }

    }

    public List<File> getSelectedFiles() {
        return mSelectedFiles;
    }

    /**
     * "Replace" the existing Fragment with a new one using given path.
     * We're really adding a Fragment to the back stack.
     *
     * @param path The absolute path of the file (directory) to display.
     */
    private void replaceFragment(String path) {
        FileListFragment explorerFragment = FileListFragment.newInstance(path);
        mFragmentManager.beginTransaction()
                .replace(R.id.explorer_fragment, explorerFragment)
                .setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
                .addToBackStack(path).commit();

    }

    /**
     * Finish this Activity with a result code and URI of the selected file.
     *
     * @param file The file selected.
     */
    private void finishWithResult(SelectableFile file) {
        if (file != null) {
            Uri uri = Uri.fromFile(file.getFile());
            setResult(RESULT_OK, new Intent().setData(uri));
            finish();
        } else {
            setResult(RESULT_CANCELED);
            finish();
        }
    }

    /**
     * Called when the user selects a File
     *
     * @param file The file that was selected
     */
    protected void onFileChecked(SelectableFile file) {
        if (file != null) {
            mPath = file.getAbsolutePath();

            if (file.isDirectory()) {
                replaceFragment(mPath);
            } else {
                updateSelectedFileList(file);
                updateSelectionStatus();
                updateUploadButtonStatus();
                //finishWithResult(file);
            }
        } else {
            Toast.makeText(MultiFileChooserActivity.this, R.string.error_selecting_file, Toast.LENGTH_SHORT).show();
        }
    }

    /**
     * Register the external storage BroadcastReceiver.
     */
    private void registerStorageListener() {
        IntentFilter filter = new IntentFilter();
        filter.addAction(Intent.ACTION_MEDIA_REMOVED);
        registerReceiver(mStorageListener, filter);
    }

    /**
     * Unregister the external storage BroadcastReceiver.
     */
    private void unregisterStorageListener() {
        unregisterReceiver(mStorageListener);
    }

}
