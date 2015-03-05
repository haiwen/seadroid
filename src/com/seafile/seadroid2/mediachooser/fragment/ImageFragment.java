package com.seafile.seadroid2.mediachooser.fragment;

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.support.v4.app.Fragment;
import android.view.*;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.GridView;
import android.widget.Toast;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.mediachooser.MediaChooserConstants;
import com.seafile.seadroid2.mediachooser.MediaModel;
import com.seafile.seadroid2.mediachooser.activity.HomeFragmentActivity;
import com.seafile.seadroid2.mediachooser.adapter.GridViewAdapter;
import com.seafile.seadroid2.ui.ToastUtils;

import java.io.File;
import java.util.ArrayList;


public class ImageFragment extends Fragment {
    private ArrayList<String> mSelectedItems = new ArrayList<String>();
    private ArrayList<MediaModel> mGalleryModelList;
    private GridView mImageGridView;
    private View mView;
    private OnImageSelectedListener mCallback;
    private GridViewAdapter mImageAdapter;
    private Cursor mImageCursor;
    private HomeFragmentActivity mActivity;


    // Container Activity must implement this interface
    public interface OnImageSelectedListener {
        public void onImageSelected(int count);
    }

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        mActivity = (HomeFragmentActivity) getActivity();
        // This makes sure that the container activity has implemented
        // the callback interface. If not, it throws an exception
        try {
            mCallback = (OnImageSelectedListener) activity;
        } catch (ClassCastException e) {
            throw new ClassCastException(activity.toString() + " must implement OnImageSelectedListener");
        }
    }

    public ImageFragment() {
        setRetainInstance(true);
    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        if (mView == null) {
            mView = inflater.inflate(R.layout.media_chooser_view_grid_layout, container, false);

            mImageGridView = (GridView) mView.findViewById(R.id.gridViewFromMediaChooser);


            if (getArguments() != null) {
                initPhoneImages(getArguments().getString("name"));
            } else {
                initPhoneImages();
            }

        } else {
            ((ViewGroup) mView.getParent()).removeView(mView);
            if (mImageAdapter == null || mImageAdapter.getCount() == 0) {
                ToastUtils.show(mActivity, R.string.media_chooser_no_media_file_available);
            }
        }

        return mView;
    }


    private void initPhoneImages(String bucketName) {
        try {
            final String orderBy = MediaStore.Images.Media.DATE_TAKEN;
            String searchParams = null;
            String bucket = bucketName;
            searchParams = "bucket_display_name = \"" + bucket + "\"";

            final String[] columns = {MediaStore.Images.Media.DATA, MediaStore.Images.Media._ID};
            mImageCursor = mActivity.getContentResolver().query(MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                    columns,
                    searchParams,
                    null,
                    orderBy + " DESC");

            setAdapter(mImageCursor);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void initPhoneImages() {
        try {
            final String orderBy = MediaStore.Images.Media.DATE_TAKEN;
            final String[] columns = {MediaStore.Images.Media.DATA, MediaStore.Images.Media._ID};
            mImageCursor = mActivity.getContentResolver().query(MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                    columns,
                    null,
                    null,
                    orderBy + " DESC");

            setAdapter(mImageCursor);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    private void setAdapter(Cursor imagecursor) {

        if (imagecursor.getCount() > 0) {

            mGalleryModelList = new ArrayList<MediaModel>();

            for (int i = 0; i < imagecursor.getCount(); i++) {
                imagecursor.moveToPosition(i);
                int dataColumnIndex = imagecursor.getColumnIndex(MediaStore.Images.Media.DATA);
                MediaModel galleryModel = new MediaModel(imagecursor.getString(dataColumnIndex).toString(), false);
                mGalleryModelList.add(galleryModel);
            }


            mImageAdapter = new GridViewAdapter(mActivity, 0, mGalleryModelList, false);
            mImageGridView.setAdapter(mImageAdapter);
        } else {
            ToastUtils.show(mActivity, R.string.media_chooser_no_media_file_available);
        }

        mImageGridView.setOnItemLongClickListener(new OnItemLongClickListener() {

            @Override
            public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {

                GridViewAdapter adapter = (GridViewAdapter) parent.getAdapter();
                MediaModel galleryModel = adapter.getItem(position);
                File file = new File(galleryModel.url);
                Intent intent = new Intent(Intent.ACTION_VIEW);
                intent.setDataAndType(Uri.fromFile(file), "image/*");
                startActivity(intent);
                return true;
            }
        });

        mImageGridView.setOnItemClickListener(new OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> parent,
                                    View view, int position, long id) {
                // update the mStatus of each category in the adapter
                GridViewAdapter adapter = (GridViewAdapter) parent.getAdapter();
                MediaModel galleryModel = adapter.getItem(position);


                if (!galleryModel.status) {
                    long size = MediaChooserConstants.ChekcMediaFileSize(new File(galleryModel.url.toString()), false);
                    if (size != 0) {
                        ToastUtils.show(mActivity, mActivity.getResources().getString(R.string.media_chooser_file_size_exeeded) + "  " + MediaChooserConstants.SELECTED_IMAGE_SIZE_IN_MB + " " + mActivity.getResources().getString(R.string.media_chooser_mb));
                        return;
                    }

                    if ((MediaChooserConstants.MAX_MEDIA_LIMIT == MediaChooserConstants.SELECTED_MEDIA_COUNT)) {
                        if (MediaChooserConstants.SELECTED_MEDIA_COUNT < 2) {
                            ToastUtils.show(mActivity, mActivity.getResources().getString(R.string.media_chooser_max_limit_file) + "  " + MediaChooserConstants.SELECTED_MEDIA_COUNT + " " + mActivity.getResources().getString(R.string.media_chooser_file));
                            return;
                        } else {
                            ToastUtils.show(mActivity, mActivity.getResources().getString(R.string.media_chooser_max_limit_file) + "  " + MediaChooserConstants.SELECTED_MEDIA_COUNT + " " + mActivity.getResources().getString(R.string.media_chooser_files));
                            return;
                        }

                    }
                }

                // inverse the status
                galleryModel.status = !galleryModel.status;

                adapter.notifyDataSetChanged();

                if (galleryModel.status) {
                    mSelectedItems.add(galleryModel.url.toString());
                    MediaChooserConstants.SELECTED_MEDIA_COUNT++;

                    startActionModeForImageFragment();
                } else {
                    mSelectedItems.remove(galleryModel.url.toString().trim());
                    MediaChooserConstants.SELECTED_MEDIA_COUNT--;

                    if(mActionMode != null)
                        mActionMode.setTitle(mSelectedItems.size() + " selected");
                }

                if (mCallback != null) {
                    mCallback.onImageSelected(mSelectedItems.size());
                    Intent intent = new Intent();
                    intent.putStringArrayListExtra(MediaChooserConstants.MEDIA_SELECTED_LIST, mSelectedItems);
                    mActivity.setResult(Activity.RESULT_OK, intent);
                }

            }
        });
    }

    private void startActionModeForImageFragment() {

        if (mSelectedItems.size() > 0 && mActionMode == null)
            // there are some selected items, start the actionMode
            mActionMode = mActivity.startActionMode(new ActionModeCallback());
        else if (mSelectedItems.size() <= 0 && mActionMode != null)
            // there no selected items, finish the actionMode
            mActionMode.finish();

        if(mActionMode != null)
            mActionMode.setTitle(mSelectedItems.size() + " selected");

    }

    public void selectAll(boolean select) {
        mSelectedItems.clear();
        MediaChooserConstants.SELECTED_MEDIA_COUNT = 0;

        if (mGalleryModelList.isEmpty())
            return;

        for (MediaModel mediaModel : mGalleryModelList) {
            if (select) {
                MediaChooserConstants.SELECTED_MEDIA_COUNT++;
                mediaModel.status = true;
                mSelectedItems.add(mediaModel.url.toString());
            } else
                mediaModel.status = false;
        }
        mImageAdapter.notifyDataSetChanged();

        if (mCallback != null) {
            mCallback.onImageSelected(mSelectedItems.size());
            Intent intent = new Intent();
            intent.putStringArrayListExtra(MediaChooserConstants.MEDIA_SELECTED_LIST, mSelectedItems);
            mActivity.setResult(Activity.RESULT_OK, intent);
        }

        if(mActionMode != null)
            mActionMode.setTitle(mSelectedItems.size() + " selected");
    }

    public ArrayList<String> getSelectedImageList() {
        return mSelectedItems;
    }

    public void addItem(String item) {
        if (mImageAdapter != null) {
            MediaModel model = new MediaModel(item, false);
            mGalleryModelList.add(0, model);
            mImageAdapter.notifyDataSetChanged();
        } else {
            initPhoneImages();
        }
    }

    public void finishActionModeIfOn() {
        if (mActionMode != null)
            mActionMode.finish();
    }

    private ActionMode mActionMode;

    private class ActionModeCallback implements ActionMode.Callback {

        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            // inflate contextual menu
            mode.getMenuInflater().inflate(R.menu.media_chooser_context_menu, menu);
            return true;
        }

        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            MenuItem menu_selectAll = menu.findItem(R.id.select_all);
            return true;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            switch (item.getItemId()) {
                case R.id.select_all:
                    if (mGalleryModelList.size() != mSelectedItems.size())
                        selectAll(true);
                    else
                        selectAll(false);
                    break;
            }
            // close action mode
            // mode.finish();
            return true;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            mActionMode = null;
        }

    }
}
