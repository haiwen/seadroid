/**
 * Copy & Modified from
 *     https://github.com/vikaskanani/Android-Custom-Gallery-And-Instant-Upload
 */

package com.seafile.seadroid;

import java.io.File;
import java.util.ArrayList;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.provider.MediaStore;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.Toast;


public class MultipleImageSelectionActivity extends Activity {

    public ImageAdapter imageAdapter;
    private final static int VIEW_IMAGE = 3;
    private static final String DEBUG_TAG = "MultipleImageSelectionActivity";

    public GridView imageGrid;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.multiple_image_selection);

        imageAdapter = new ImageAdapter();
        imageGrid = (GridView) findViewById(R.id.PhoneImageGrid);
        imageGrid.setAdapter(imageAdapter);

        final Button selectBtn = (Button) findViewById(R.id.selectBtn);
        selectBtn.setOnClickListener(new OnClickListener() {

            public void onClick(View v) {
                final int len = imageAdapter.images.size();
                ArrayList<String> selected = new ArrayList<String>();
                for (int i = 0; i < len; i++) {
                    if (imageAdapter.images.get(i).selection) {
                        selected.add(imageAdapter.images.get(i).path);
                    }
                }
                if (selected.isEmpty()) {
                    Toast.makeText(getApplicationContext(),
                            "Please select at least one image",
                            Toast.LENGTH_LONG).show();
                } else {
                    Intent result = new Intent();
                    result.putExtra("photos", selected);
                    setResult(Activity.RESULT_OK, result);
                    finish();
                }
            }
        });

        ConcurrentAsyncTask.execute(new ThumbnailTask(imageAdapter));
    }

    public class ImageAdapter extends BaseAdapter {
        private LayoutInflater mInflater;
        public ArrayList<ImageItem> images = new ArrayList<ImageItem>();

        public ImageAdapter() {
            mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        }

        public int getCount() {
            return images.size();
        }

        public ImageItem getItem(int position) {
            return images.get(position);
        }

        public long getItemId(int position) {
            return position;
        }

        public View getView(int position, View convertView, ViewGroup parent) {
            ViewHolder holder;
            if (convertView == null) {
                holder = new ViewHolder();
                convertView = mInflater.inflate(R.layout.gallery_item, null);
                holder.imageview = (ImageView) convertView
                        .findViewById(R.id.thumbImage);
                holder.checkbox = (CheckBox) convertView
                        .findViewById(R.id.itemCheckBox);

                convertView.setTag(holder);
            } else {
                holder = (ViewHolder) convertView.getTag();
            }
            ImageItem item = images.get(position);
            holder.checkbox.setId(position);
            holder.imageview.setId(position);
            holder.checkbox.setOnClickListener(new OnClickListener() {

                public void onClick(View v) {
                    CheckBox cb = (CheckBox) v;
                    int id = cb.getId();
                    if (images.get(id).selection) {
                        cb.setChecked(false);
                        images.get(id).selection = false;
                    } else {
                        cb.setChecked(true);
                        images.get(id).selection = true;
                    }
                }
            });

            holder.imageview.setOnClickListener(new OnClickListener() {

                public void onClick(View v) {
                    int id = v.getId();
                    ImageItem item = images.get(id);
                    Intent intent = new Intent();
                    intent.setAction(Intent.ACTION_VIEW);
                    final String[] columns = { MediaStore.Images.Media.DATA };
                    Cursor imagecursor =  getContentResolver().query(
                            MediaStore.Images.Media.EXTERNAL_CONTENT_URI, columns,
                            MediaStore.Images.Media._ID + " = " + item.id, null, MediaStore.Images.Media._ID);
                    if (imagecursor != null && imagecursor.getCount() > 0){
                        imagecursor.moveToPosition(0);
                        String path = imagecursor.getString(imagecursor.getColumnIndexOrThrow(MediaStore.Images.Media.DATA));
                        File file = new File(path);
                        imagecursor.close();
                        intent.setDataAndType(
                                Uri.fromFile(file),
                                "image/*");
                        startActivityForResult(intent, VIEW_IMAGE);
                    }
                }
            });

            holder.imageview.setImageBitmap(item.thumb);
            holder.checkbox.setChecked(item.selection);

            return convertView;
        }
    }

    class ViewHolder {
        ImageView imageview;
        CheckBox checkbox;
    }

    class ImageItem {
        boolean selection;
        int id;
        String path;
        Bitmap thumb;
    }

    private class ThumbnailTask extends AsyncTask<Void, ImageItem, Void> {
        private ImageAdapter adapter;

        public ThumbnailTask(ImageAdapter adapter) {
            this.adapter = adapter;
        }

        @Override
        protected void onProgressUpdate(ImageItem... values) {
            ImageItem item = values[0];
            adapter.images.add(item);
            adapter.notifyDataSetChanged();
            Log.d(DEBUG_TAG, "main: get thumbnail of " + item.path);
        }

        @Override
        protected Void doInBackground(Void... params) {
            final String[] columns = {
                MediaStore.Images.Media._ID,
                MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Images.Media.DATA,
            };

            final String orderBy = MediaStore.Images.Media._ID;

            Cursor imagecursor = getContentResolver().query(
                    MediaStore.Images.Media.EXTERNAL_CONTENT_URI, columns,
                    null, null, orderBy);

            if (imagecursor.moveToFirst()) {
                int id_index = imagecursor.getColumnIndex(MediaStore.Images.Media._ID);
                int data_index = imagecursor.getColumnIndex(MediaStore.Images.Media.DATA);

                do {
                    String path = imagecursor.getString(data_index);
                    // only show photos taken by cameras
                    if (!path.contains("DCIM")) {
                        continue;
                    }

                    ImageItem item = new ImageItem();
                    item.id = imagecursor.getInt(id_index);
                    item.path = path;

                    Bitmap img = MediaStore.Images.Thumbnails.getThumbnail(
                        getApplicationContext().getContentResolver(), item.id,
                        MediaStore.Images.Thumbnails.MICRO_KIND, null);
                    item.thumb = img;

                    publishProgress(item);
                    Log.d(DEBUG_TAG, "async: finished thumbnail of " + item.path);

                } while (imagecursor.moveToNext());
            }

            imagecursor.close();

            return null;
        }
    }
}
