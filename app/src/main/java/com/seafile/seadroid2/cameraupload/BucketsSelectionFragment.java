package com.seafile.seadroid2.cameraupload;

import android.graphics.Bitmap;
import android.os.Bundle;
import android.provider.MediaStore;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import android.content.Context;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.cameraupload.GalleryBucketUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Choose gallery buckets to upload
 */
public class BucketsSelectionFragment extends Fragment {

    private List<GalleryBucketUtils.Bucket> buckets;
    private Bitmap[] thumbnails;
    private boolean[] selectedBuckets;
    private ImageAdapter imageAdapter;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        View rootView = getActivity().getLayoutInflater().inflate(R.layout.cuc_bucket_selection_layout, null);
        buckets = GalleryBucketUtils.getMediaBuckets(getActivity().getApplicationContext());

        SettingsManager settingsManager = SettingsManager.instance();
        List<String> currentBucketList = settingsManager.getCameraUploadBucketList();

        thumbnails = new Bitmap[buckets.size()];
        selectedBuckets = new boolean[buckets.size()];
        for (int i = 0; i < buckets.size(); i++) {
            GalleryBucketUtils.Bucket b = buckets.get(i);
            if (b.image_id > 0) {
                thumbnails[i] = MediaStore.Images.Thumbnails.getThumbnail(
                        getActivity().getApplicationContext().getContentResolver(), b.image_id,
                        MediaStore.Images.Thumbnails.MINI_KIND, null);
            }

            // if the user has previously selected buckets, mark these.
            // otherwise, select the ones that will be auto-guessed.
            if (currentBucketList.size() > 0)
                selectedBuckets[i] = currentBucketList.contains(b.id);
            else
                selectedBuckets[i] = b.isCameraBucket;
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
                        holder.marking.setVisibility(View.VISIBLE);
                    else
                        holder.marking.setVisibility(View.INVISIBLE);
                }
            });
            holder.imageview.setImageBitmap(thumbnails[position]);
            if (selectedBuckets[position])
                holder.marking.setVisibility(View.VISIBLE);
            else
                holder.marking.setVisibility(View.INVISIBLE);
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

