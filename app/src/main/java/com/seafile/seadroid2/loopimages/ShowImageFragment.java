package com.seafile.seadroid2.loopimages;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.dialog.DeleteFileDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.io.File;

/**
 * A simple {@link Fragment} subclass.
 * create an instance of this fragment.
 */
public class ShowImageFragment extends Fragment {
    public static final String DEBUG_TAG = "GalleryActivity";

    private ImageView mImageView;
    private TextView mImageInfoTextView;
    private ImageView mDeleteBtn;
    private ImageView mStarBtn;
    private ImageView mShareBtn;
    private RelativeLayout mToolbar;
    private DataManager dataMgr;
    private DirInfo dirInfo;
    private String fileName;
    public static int taskID;
    private LoopImagesWidgetConfigureActivity mActivity;
    private Account mAccount;

    public static final int  MAX_BITMAP_EDGE = 1024;

    /** flag to mark if the tool bar was shown */
    private boolean showToolBar = true;
    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            switch (v.getId()) {
                case R.id.loopimages_delete_photo:
                    deleteFile(dirInfo, fileName);
                    break;
                case R.id.loopimages_star_photo:
                    starFile(dirInfo.getRepoId(), dirInfo.getDirPath(), fileName);
                    break;
                case R.id.loopimages_share_photo:
                    shareFile(dirInfo.getRepoId(), false, Utils.pathJoin(dirInfo.getDirPath(), fileName));
                    break;
            }
        }
    };

//    @Override
//    public void onSaveInstanceState(@NonNull Bundle outState) {
//        super.onSaveInstanceState(outState);
//        mActivity.finish();
//    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        mActivity = (LoopImagesWidgetConfigureActivity) getActivity();
        Context context = mActivity.getApplicationContext();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.loop_imaget_show_image_fragment, container, false);

        mDeleteBtn = (ImageView) rootView.findViewById(R.id.loopimages_delete_photo);
        mStarBtn = (ImageView) rootView.findViewById(R.id.loopimages_star_photo);
        mShareBtn = (ImageView) rootView.findViewById(R.id.loopimages_share_photo);
        mToolbar = (RelativeLayout) rootView.findViewById(R.id.loopimages_tool_bar);
        mDeleteBtn.setOnClickListener(onClickListener);
        mStarBtn.setOnClickListener(onClickListener);
        mShareBtn.setOnClickListener(onClickListener);

        mImageView = (ImageView) rootView.findViewById(R.id.loopimages_show_image_image_view);
        mImageView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                hideOrShowToolBar();
            }
        });

        mImageInfoTextView = (TextView) rootView.findViewById(R.id.loopimages_image_info);

        Intent intent = mActivity.getIntent();
        dirInfo = LoopImagesWidgetConfigureActivity.getDirInfoFromString(context, intent.getStringExtra(LoopImagesWidget.DIR_INFO));
        fileName = intent.getStringExtra(LoopImagesWidget.IMAGE_NAME);
        mAccount = dirInfo.getAccount();
        dataMgr = new DataManager(mAccount);

        displayPhotos(dirInfo, fileName);

        return rootView;
    }


    private void displayPhotos(DirInfo dirInfo, String fileName) {
        if(dirInfo == null || fileName == null || fileName.length() <= 0){
            return;
        }
        // calculate thumbnail urls by cached dirents
        File file = dataMgr.getLocalRepoFile(dirInfo.getRepoName(), dirInfo.getRepoId(), Utils.pathJoin(dirInfo.getDirPath(), fileName));
        if(file == null){
            return;
        }
        Bitmap bitmap = BitmapFactory.decodeFile(file.getAbsolutePath());
        if(bitmap == null){
            return;
        }
        if(bitmap.getHeight() > MAX_BITMAP_EDGE || bitmap.getWidth() > MAX_BITMAP_EDGE){
            float scale = MAX_BITMAP_EDGE/(float)Math.max(bitmap.getHeight(), bitmap.getWidth());
            Matrix matrix = new Matrix();
            matrix.postScale(scale, scale);
            bitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);
        }
        if(bitmap == null){
            return;
        }
        mImageView.setImageBitmap(bitmap);
        mImageInfoTextView.setText(dirInfo.getRepoName()+":"+Utils.pathJoin(dirInfo.getDirPath(), fileName));
    }


    /**
     * This method will get called when tapping at the center of a photo,
     * tool bar will auto hide when open the gallery,
     * and will show or hide alternatively when tapping.
     */
    public void hideOrShowToolBar() {
        if (showToolBar) {
            mToolbar.setVisibility(View.VISIBLE);
            mImageInfoTextView.setVisibility(View.VISIBLE);
        } else {
            mToolbar.setVisibility(View.GONE);
            mImageInfoTextView.setVisibility(View.GONE);
        }
        showToolBar = !showToolBar;

    }
    public void showShortToast(final Activity activity, final String message) {
        if (activity == null)
            return;
        Toast.makeText(activity, message, Toast.LENGTH_SHORT).show();
    }

    public void showShortToast(final Activity activity, final int resId) {
        if (activity == null)
            return;

        showShortToast(activity, activity.getString(resId));
    }

    public void showLongToast(final Activity activity, final String message) {
        if (activity == null)
            return;
        Toast.makeText(activity, message, Toast.LENGTH_LONG).show();
    }

    public void showLongToast(final Activity activity, final int resId) {
        if (activity == null)
            return;

        showLongToast(activity, activity.getString(resId));
    }


    private void deleteFile(DirInfo dirInfo, String fileName) {
        String path = Utils.pathJoin(dirInfo.getDirPath(), fileName);
        File file = dataMgr.getLocalRepoFile(dirInfo.getRepoName(), dirInfo.getRepoId(), path);
        final DeleteFileDialog dialog = new DeleteFileDialog();
        dialog.init(dirInfo.getRepoId(), path, false, mAccount);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(mActivity, R.string.delete_successful);
                if(file != null || file.exists()){
                    file.delete();
                }
                mActivity.finish();
            }
        });
        dialog.show(mActivity.getSupportFragmentManager(), "DialogFragment");
    }

    private void starFile(String repoId, String dir, String fileName) {
        if (!Utils.isNetworkOn()) {
            showShortToast(mActivity, R.string.network_down);
            return;
        }

        String p = Utils.pathJoin(dir, fileName);
        ConcurrentAsyncTask.execute(new StarFileTask(repoId, p));
    }

    private void shareFile(String repoID, boolean isEncrypt, String path) {
        if (isEncrypt) {
            WidgetUtils.inputSharePassword(mActivity, repoID, path, false, mAccount);
        } else {
            WidgetUtils.chooseShareApp(mActivity, repoID, path, false, mAccount, null, null);
        }
    }

    class StarFileTask extends AsyncTask<Void, Void, Void> {
        private String repoId;
        private String path;
        private SeafException err;

        public StarFileTask(String repoId, String path) {
            this.repoId = repoId;
            this.path = path;
        }

        @Override
        protected Void doInBackground(Void... params) {

            if (dataMgr == null)
                return null;

            try {
                dataMgr.star(repoId, path);
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (err != null) {
                showShortToast(mActivity, R.string.star_file_failed);
                return;
            }

            showShortToast(mActivity, R.string.star_file_succeed);
        }
    }
}