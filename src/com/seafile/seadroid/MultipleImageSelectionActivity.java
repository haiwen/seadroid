/**
 * Copy & Modified from
 *     https://github.com/vikaskanani/Android-Custom-Gallery-And-Instant-Upload
 */

package com.seafile.seadroid;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

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
import android.widget.AbsListView;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.Toast;


public class MultipleImageSelectionActivity extends Activity {

    // public ImageAdapter imageAdapter;
    // private final static int VIEW_IMAGE = 3;
    // private static final String DEBUG_TAG = "MultipleImageSelectionActivity";

    // public GridView imageGrid;

    // @Override
    // public void onCreate(Bundle savedInstanceState) {
    //     super.onCreate(savedInstanceState);
    //     setContentView(R.layout.multiple_image_selection);

    //     imageAdapter = new ImageAdapter();
    //     imageGrid = (GridView) findViewById(R.id.PhoneImageGrid);
    //     imageGrid.setAdapter(imageAdapter);
    //     imageGrid.setOnScrollListener(imageAdapter);

    //     final Button selectBtn = (Button) findViewById(R.id.selectBtn);
    //     selectBtn.setOnClickListener(new OnClickListener() {

    //         public void onClick(View v) {
    //             final int len = imageAdapter.images.size();
    //             ArrayList<String> selected = new ArrayList<String>();
    //             for (int i = 0; i < len; i++) {
    //                 if (imageAdapter.images.get(i).selection) {
    //                     selected.add(imageAdapter.images.get(i).path);
    //                 }
    //             }
    //             if (selected.isEmpty()) {
    //                 Toast.makeText(getApplicationContext(),
    //                         "Please select at least one image",
    //                         Toast.LENGTH_LONG).show();
    //             } else {
    //                 Intent result = new Intent();
    //                 result.putExtra("photos", selected);
    //                 setResult(Activity.RESULT_OK, result);
    //                 finish();
    //             }
    //         }
    //     });
    // }

    // public class ImageAdapter extends BaseAdapter
    //                         implements GridView.OnScrollListener {
    //     private LayoutInflater mInflater;
    //     private ArrayList<ImageItem> images;
    //     private int firstVisibleItem, visibleItemCount;

    //     public ImageAdapter() {
    //         mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    //         images = getImages();
    //         firstVisibleItem = 0;
    //         visibleItemCount = 0;
    //     }

    //     public int getCount() {
    //         return images.size();
    //     }

    //     public ImageItem getItem(int position) {
    //         return images.get(position);
    //     }

    //     public long getItemId(int position) {
    //         return position;
    //     }

    //     public View getView(int position, View convertView, ViewGroup parent) {
    //         ViewHolder holder;
    //         if (convertView == null) {
    //             holder = new ViewHolder();
    //             convertView = mInflater.inflate(R.layout.gallery_item, null);
    //             holder.imageview = (ImageView) convertView
    //                     .findViewById(R.id.thumbImage);
    //             holder.checkbox = (CheckBox) convertView
    //                     .findViewById(R.id.itemCheckBox);

    //             convertView.setTag(holder);
    //         } else {
    //             holder = (ViewHolder) convertView.getTag();
    //         }

    //         ImageItem item = images.get(position);
    //         item.holder = holder;

    //         Log.d(DEBUG_TAG, "getView called on " + item);
    //         holder.checkbox.setId(position);
    //         holder.imageview.setId(position);
    //         holder.checkbox.setOnClickListener(new OnClickListener() {

    //             public void onClick(View v) {
    //                 CheckBox cb = (CheckBox) v;
    //                 int id = cb.getId();
    //                 if (images.get(id).selection) {
    //                     cb.setChecked(false);
    //                     images.get(id).selection = false;
    //                 } else {
    //                     cb.setChecked(true);
    //                     images.get(id).selection = true;
    //                 }
    //             }
    //         });

    //         holder.imageview.setOnClickListener(new OnClickListener() {

    //             public void onClick(View v) {
    //                 int id = v.getId();
    //                 ImageItem item = images.get(id);
    //                 Intent intent = new Intent();
    //                 intent.setAction(Intent.ACTION_VIEW);
    //                 final String[] columns = { MediaStore.Images.Media.DATA };
    //                 Cursor imagecursor =  getContentResolver().query(
    //                         MediaStore.Images.Media.EXTERNAL_CONTENT_URI, columns,
    //                         MediaStore.Images.Media._ID + " = " + item.id, null, MediaStore.Images.Media._ID);
    //                 if (imagecursor != null && imagecursor.getCount() > 0) {
    //                     imagecursor.moveToPosition(0);
    //                     String path = imagecursor.getString(imagecursor.getColumnIndexOrThrow(MediaStore.Images.Media.DATA));
    //                     File file = new File(path);
    //                     imagecursor.close();
    //                     intent.setDataAndType(
    //                             Uri.fromFile(file),
    //                             "image/*");
    //                     startActivityForResult(intent, VIEW_IMAGE);
    //                 }
    //             }
    //         });

    //         holder.checkbox.setChecked(item.selection);
    //         if (item.thumb != null) {
    //             holder.imageview.setImageBitmap(item.thumb);
    //         } else {
    //             if (item.thumbTask == null) {

    //                 ThumbnailTask task = new ThumbnailTask(this, item);
    //                 ConcurrentAsyncTask.execute(task);
    //             }
    //         }

    //         return convertView;
    //     }

    //     public void onThumbnailTaskFinished(ImageItem item) {
    //         // Update the image view, if the holder is still holding this
    //         // item's view
    //         int currentPosition = item.holder.checkbox.getId();
    //         Log.d(DEBUG_TAG, "currentPosition = " + currentPosition
    //               + ", item.position = " + item.position);

    //         if (currentPosition == item.position) {
    //             item.holder.imageview.setImageBitmap(item.thumb);
    //         }
    //     }

    //     /**
    //      * Dynamically free thubmails and cancel async tasks when image grids
    //      * are no longer visible as the user scrolls the screen.
    //      *
    //      */
    //     @Override
    //     public void onScroll(AbsListView view, int firstVisibleItem,
    //                          int visibleItemCount, int totalItemCount) {
            
    //         this.firstVisibleItem = firstVisibleItem;
    //         this.visibleItemCount = visibleItemCount;
            
    //         int i, n = images.size();
            
    //         Log.d(DEBUG_TAG, String.format("onScroll: first = %d, len = %d, total = %d",
    //                                        firstVisibleItem, visibleItemCount, totalItemCount));

    //         for (i = 0; i < n; i++) {
    //             if (i < firstVisibleItem || i > firstVisibleItem + visibleItemCount - 1) {
    //                 ImageItem item = images.get(i);
    //                 if (item.thumbTask != null) {
    //                     item.thumbTask.cancel(true);
    //                     item.thumbTask = null;
    //                     Log.d(DEBUG_TAG, "canceled task for " + item);
    //                 }

    //                 if (item.thumb != null) {
    //                     item.thumb.recycle();
    //                     item.thumb = null;
    //                     Log.d(DEBUG_TAG, "removed thumb of " + item);
    //                 }
    //             }
    //         }
    //     }

    //     @Override
    //     public void onScrollStateChanged(AbsListView view, int scrollState) {
    //     }

    //     /**
    //      * Retrive a list of images path
    //      */
    //     private ArrayList<ImageItem> getImages() {
    //         ArrayList<ImageItem> items = new ArrayList<ImageItem>();
    //         final String[] columns = {
    //             MediaStore.Images.Media._ID,
    //             MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
    //             MediaStore.Images.Media.DATA,
    //         };

    //         final String orderBy = MediaStore.Images.Media._ID;

    //         Cursor imagecursor = getContentResolver().query(
    //                 MediaStore.Images.Media.EXTERNAL_CONTENT_URI, columns,
    //                 null, null, orderBy);

    //         if (imagecursor.moveToFirst()) {
    //             int id_index = imagecursor.getColumnIndex(MediaStore.Images.Media._ID);
    //             int data_index = imagecursor.getColumnIndex(MediaStore.Images.Media.DATA);

    //             do {
    //                 String path = imagecursor.getString(data_index);
    //                 // only show photos taken by cameras
    //                 if (!path.contains("DCIM")) {
    //                     continue;
    //                 }

    //                 ImageItem item = new ImageItem();
    //                 item.id = imagecursor.getInt(id_index);
    //                 item.path = path;

    //                 item.position = items.size();
    //                 items.add(item);

    //             } while (imagecursor.moveToNext());
    //         }

    //         imagecursor.close();

    //         return items;
    //     }

    // }

    // class ViewHolder {
    //     ImageView imageview;
    //     CheckBox checkbox;
    // }

    // class ImageItem {
    //     boolean selection;
    //     int id;
    //     String path;
    //     Bitmap thumb;
    //     ThumbnailTask thumbTask;
    //     ViewHolder holder;
    //     int position;

    //     @Override
    //     public String toString() {
    //         return String.format("image(id = %d, path = %s)", id, path);
    //     }
    // }

    // private class ThumbnailTask extends AsyncTask<Void, Void, Bitmap> {
    //     private ImageItem item;
    //     private ImageAdapter adapter;

    //     public ThumbnailTask(ImageAdapter adapter, ImageItem item) {
    //         this.adapter = adapter;
    //         this.item = item;
    //         item.thumbTask = this;
    //     }

    //     @Override
    //     protected Bitmap doInBackground(Void... params) {

    //         Bitmap img = MediaStore.Images.Thumbnails.getThumbnail(
    //             getApplicationContext().getContentResolver(), item.id,
    //             MediaStore.Images.Thumbnails.MICRO_KIND, null);

    //         Log.d(DEBUG_TAG, "async: finished thumbnail of " + item);

    //         return img;
    //     }

    //     @Override
    //     protected void onPostExecute(Bitmap thumb) {
    //         item.thumb = thumb;
    //         item.thumbTask = null;
    //         adapter.onThumbnailTaskFinished(item);
    //     }
    // }
}
