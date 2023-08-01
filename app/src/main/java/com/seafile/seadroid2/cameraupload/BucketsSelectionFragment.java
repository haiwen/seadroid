package com.seafile.seadroid2.cameraupload;

import android.content.Context;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.provider.MediaStore;
import androidx.fragment.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.util.GlideApp;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Choose gallery buckets to upload
 */
public class BucketsSelectionFragment extends Fragment {

    private List<GalleryBucketUtils.Bucket> buckets;
    private boolean[] selectedBuckets;
    private ImageAdapter imageAdapter;
    private Bitmap[] thumbnails;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        View rootView = getActivity().getLayoutInflater().inflate(R.layout.cuc_bucket_selection_layout, null);
        SettingsManager settingsManager = SettingsManager.instance();
        List<String> currentBucketList = settingsManager.getCameraUploadBucketList();
        buckets = GalleryBucketUtils.getMediaBuckets(getActivity().getApplicationContext());
        selectedBuckets = new boolean[buckets.size()];
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.Q) {
            thumbnails = new Bitmap[buckets.size()];
            for (int i = 0; i < buckets.size(); i++) {
                GalleryBucketUtils.Bucket b = buckets.get(i);
                if (b.image_id > 0) {
                    thumbnails[i] = MediaStore.Images.Thumbnails.getThumbnail(
                            getActivity().getApplicationContext().getContentResolver(), b.image_id,
                            MediaStore.Images.Thumbnails.MINI_KIND, null);
                }
                if (currentBucketList.size() > 0)
                    selectedBuckets[i] = currentBucketList.contains(b.id);
                else
                    selectedBuckets[i] = b.isCameraBucket;
            }
        } else {
            for (int i = 0; i < this.buckets.size(); i++) {
                GalleryBucketUtils.Bucket b = this.buckets.get(i);
                if (b.isImages != null && b.isImages.equals(GalleryBucketUtils.IMAGES)) {
                    Uri image_uri = Uri.withAppendedPath(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, b.imageId);
                    String image_path = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), image_uri, "images");
                    b.imagePath = image_path;
                } else {
                    Uri video_uri = Uri.withAppendedPath(MediaStore.Video.Media.EXTERNAL_CONTENT_URI, b.videoId);
                    String videoPath = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), video_uri, "video");
                    b.videoPath = videoPath;
                }

                // if the user has previously selected buckets, mark these.
                // otherwise, select the ones that will be auto-guessed.
                if (currentBucketList.size() > 0)
                    selectedBuckets[i] = currentBucketList.contains(b.id);
                else
                    selectedBuckets[i] = b.isCameraBucket;
            }
        }

        GridView imagegrid = (GridView) rootView.findViewById(R.id.cuc_bucket_selection_grid);
        imageAdapter = new ImageAdapter();
        imagegrid.setAdapter(imageAdapter);

        return rootView;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        getActivity().finish();
    }

    @Override
    public void onPause() {
        super.onPause();
        getActivity().finish();
    }

    public List<String> getSelectedBuckets() {
        List<String> ret = new ArrayList<>();
        for (int i = 0; i < buckets.size(); i++) {
            if (selectedBuckets[i]) {
                ret.add(buckets.get(i).id);
            }
        }

        return ret;
    }

    public class ImageAdapter extends BaseAdapter {
        private LayoutInflater mInflater;

        public ImageAdapter() {
            mInflater = (LayoutInflater) getActivity().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        }

        public int getCount() {
            return buckets.size();
        }

        public Object getItem(int position) {
            return position;
        }

        public long getItemId(int position) {
            return position;
        }

        public View getView(int position, View convertView, ViewGroup parent) {
            final ViewHolder holder;
            if (convertView == null) {
                holder = new ViewHolder();
                convertView = mInflater.inflate(R.layout.bucket_item, null);
                holder.imageview = (ImageView) convertView.findViewById(R.id.bucket_item_thumbImage);
                holder.text = (TextView) convertView.findViewById(R.id.bucket_item_name);
                holder.marking = (ImageView) convertView.findViewById(R.id.bucket_item_marking);

                convertView.setTag(holder);
            }
            else {
                holder = (ViewHolder) convertView.getTag();
            }
            holder.imageview.setId(position);
            holder.text.setText(buckets.get(position).name);
            holder.imageview.setOnClickListener(new View.OnClickListener() {

                public void onClick(View v) {
                    int id = v.getId();
                    selectedBuckets[id] = !selectedBuckets[id];
                    if (selectedBuckets[id])
                        holder.marking.setBackgroundResource(R.drawable.checkbox_checked);
                    else
                        holder.marking.setBackgroundResource(R.drawable.checkbox_unchecked);
                }
            });
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.Q) {
                holder.imageview.setImageBitmap(thumbnails[position]);
            } else {
                if (buckets.get(position).isImages != null && buckets.get(position).isImages.equals(GalleryBucketUtils.IMAGES)) {
                    GlideApp.with(getActivity()).load(buckets.get(position).imagePath).into(holder.imageview);
                } else {
                    GlideApp.with(getActivity()).load(Uri.fromFile(new File(buckets.get(position).videoPath))).into(holder.imageview);
                }
            }
            if (selectedBuckets[position])
                holder.marking.setBackgroundResource(R.drawable.checkbox_checked);
            else
                holder.marking.setBackgroundResource(R.drawable.checkbox_unchecked);
            holder.id = position;
            return convertView;
        }
    }

    static class ViewHolder {
        ImageView imageview;
        ImageView marking;
        TextView text;
        int id;
    }
}

