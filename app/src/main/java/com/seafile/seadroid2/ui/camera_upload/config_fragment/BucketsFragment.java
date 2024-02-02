package com.seafile.seadroid2.ui.camera_upload.config_fragment;

import android.content.Context;
import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.TranslateAnimation;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.RadioGroup;
import android.widget.RadioGroup.OnCheckedChangeListener;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadConfigActivity;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;
import com.seafile.seadroid2.util.sp.SettingsManager;
import com.seafile.seadroid2.util.GlideApp;

import java.util.ArrayList;
import java.util.List;

/**
 * Buckets fragment
 */
public class BucketsFragment extends Fragment {

    private CameraUploadConfigActivity mActivity;
    private RadioGroup mRadioGroup;

    private TranslateAnimation mSlideInAnimation;
    private TranslateAnimation mSlideOutAnimation;
    private GridView mGridView;
    private List<GalleryBucketUtils.Bucket> buckets;
    private boolean[] selectedBuckets;
    private ImageAdapter imageAdapter;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        mActivity = (CameraUploadConfigActivity) getActivity();
        View rootView = mActivity.getLayoutInflater().inflate(R.layout.cuc_local_directory_fragment, null);
        mSlideInAnimation = new TranslateAnimation(Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 2.0f,
                Animation.RELATIVE_TO_SELF, 0.0f);

        mSlideInAnimation.setDuration(200);
        mSlideInAnimation.setInterpolator(new AccelerateDecelerateInterpolator());
        mSlideInAnimation.setAnimationListener(slideInListener);

        mSlideOutAnimation = new TranslateAnimation(Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 0.0f,
                Animation.RELATIVE_TO_SELF, 2.0f);
        mSlideOutAnimation.setDuration(200);
        mSlideOutAnimation.setInterpolator(new AccelerateDecelerateInterpolator());
        mSlideOutAnimation.setAnimationListener(slideOutListener);
        mGridView = rootView.findViewById(R.id.cuc_bucket_selection_grid);
        mRadioGroup = rootView.findViewById(R.id.cuc_local_directory_radio_group);

        SettingsManager settingsManager = SettingsManager.getInstance();
        if (settingsManager.getCameraUploadBucketList().isEmpty()) {
            // auto scan
            mGridView.setVisibility(View.INVISIBLE);
            mGridView.setEnabled(false);
            mRadioGroup.check(R.id.cuc_local_directory_auto_scan_rb);
        } else {
            // pick custom folders to scan
            mGridView.setVisibility(View.VISIBLE);
            mGridView.setEnabled(true);
            mRadioGroup.check(R.id.cuc_local_directory_pick_folders_rb);
        }

        mRadioGroup.setOnCheckedChangeListener(onCheckedChangeListener);
        List<String> currentBucketList = settingsManager.getCameraUploadBucketList();
        buckets = GalleryBucketUtils.getMediaBuckets(getActivity().getApplicationContext());

        selectedBuckets = new boolean[buckets.size()];
        for (int i = 0; i < this.buckets.size(); i++) {
            GalleryBucketUtils.Bucket b = this.buckets.get(i);
            if (!currentBucketList.isEmpty())
                selectedBuckets[i] = currentBucketList.contains(b.id);
            else
                selectedBuckets[i] = b.isCameraBucket;
        }

        imageAdapter = new ImageAdapter();
        mGridView.setAdapter(imageAdapter);

        return rootView;
    }

    /**
     * RadioButton selection listener.
     */
    private final OnCheckedChangeListener onCheckedChangeListener = new OnCheckedChangeListener() {

        @Override
        public void onCheckedChanged(RadioGroup radioGroup, int radioButtonId) {
            switch (radioButtonId) {
                case R.id.cuc_local_directory_auto_scan_rb:
                    mGridView.startAnimation(mSlideOutAnimation);
                    mGridView.setEnabled(false);
                    break;
                case R.id.cuc_local_directory_pick_folders_rb:
                    mGridView.startAnimation(mSlideInAnimation);
                    mGridView.setEnabled(true);
                    break;
            }
        }
    };

    public boolean isAutoScanSelected() {
        return mRadioGroup.getCheckedRadioButtonId() == R.id.cuc_local_directory_auto_scan_rb;
    }

    /**
     * Slide out animation listener.
     */
    private final AnimationListener slideOutListener = new AnimationListener() {

        @Override
        public void onAnimationEnd(Animation arg0) {
            mGridView.setVisibility(View.INVISIBLE);
        }

        @Override
        public void onAnimationRepeat(Animation arg0) {
        }

        @Override
        public void onAnimationStart(Animation arg0) {
            mGridView.setVisibility(View.VISIBLE);
        }

    };

    /**
     * Slide in animation listener.
     */
    private final AnimationListener slideInListener = new AnimationListener() {

        @Override
        public void onAnimationEnd(Animation arg0) {
            mGridView.setVisibility(View.VISIBLE);
        }

        @Override
        public void onAnimationRepeat(Animation arg0) {
        }

        @Override
        public void onAnimationStart(Animation arg0) {
            mGridView.setVisibility(View.VISIBLE);
        }

    };


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
                holder.imageview = convertView.findViewById(R.id.bucket_item_thumbImage);
                holder.text = convertView.findViewById(R.id.bucket_item_name);
                holder.marking = convertView.findViewById(R.id.bucket_item_marking);

                convertView.setTag(holder);
            } else {
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

            GlideApp.with(getContext()).load(buckets.get(position).uri).into(holder.imageview);

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

    public List<String> getSelectedBuckets() {
        List<String> ret = new ArrayList<>();
        for (int i = 0; i < buckets.size(); i++) {
            if (selectedBuckets[i]) {
                ret.add(buckets.get(i).id);
            }
        }

        return ret;
    }
}
