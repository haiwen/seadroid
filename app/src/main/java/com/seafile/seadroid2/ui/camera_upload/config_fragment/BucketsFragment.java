package com.seafile.seadroid2.ui.camera_upload.config_fragment;

import android.os.Bundle;
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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.seafile.seadroid2.GlideApp;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Buckets fragment
 */
public class BucketsFragment extends Fragment {
    private RadioGroup mRadioGroup;

    private TranslateAnimation mSlideInAnimation;
    private TranslateAnimation mSlideOutAnimation;
    private GridView mGridView;
    private List<GalleryBucketUtils.Bucket> buckets;
    private boolean[] selectedBuckets;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        return inflater.inflate(R.layout.cuc_local_directory_fragment, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initAnimation();

        mGridView = view.findViewById(R.id.cuc_bucket_selection_grid);
        mRadioGroup = view.findViewById(R.id.cuc_local_directory_radio_group);

        if (AlbumBackupSharePreferenceHelper.readBucketIds().isEmpty()) {
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

        buckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
        if (buckets == null) {
            SLogs.e("BucketsFragment", "buckets is null");
            Toasts.show(R.string.permission_not_granted);
            return;
        }

        selectedBuckets = new boolean[buckets.size()];

        List<String> currentBucketList = AlbumBackupSharePreferenceHelper.readBucketIds();
        for (int i = 0; i < this.buckets.size(); i++) {
            GalleryBucketUtils.Bucket b = this.buckets.get(i);
            if (!currentBucketList.isEmpty())
                selectedBuckets[i] = currentBucketList.contains(b.bucketId);
            else
                selectedBuckets[i] = b.isCameraBucket;
        }

        ImageAdapter imageAdapter = new ImageAdapter();
        mGridView.setAdapter(imageAdapter);
    }

    private void initAnimation() {
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
    }

    /**
     * RadioButton selection listener.
     */
    private final OnCheckedChangeListener onCheckedChangeListener = new OnCheckedChangeListener() {

        @Override
        public void onCheckedChanged(RadioGroup radioGroup, int radioButtonId) {
            if (radioButtonId == R.id.cuc_local_directory_auto_scan_rb) {
                mGridView.startAnimation(mSlideOutAnimation);
                mGridView.setEnabled(false);
            } else if (radioButtonId == R.id.cuc_local_directory_pick_folders_rb) {
                mGridView.startAnimation(mSlideInAnimation);
                mGridView.setEnabled(true);
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
        private final LayoutInflater mInflater;

        public ImageAdapter() {
            mInflater = LayoutInflater.from(requireContext());
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
            holder.text.setText(buckets.get(position).bucketName);
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

            GlideApp.with(requireContext()).load(buckets.get(position).uri).into(holder.imageview);

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
        if (buckets == null) {
            Toasts.show(R.string.permission_not_granted);
            return null;
        }

        List<String> ret = new ArrayList<>();
        for (int i = 0; i < buckets.size(); i++) {
            if (selectedBuckets[i]) {
                ret.add(buckets.get(i).bucketId);
            }
        }

        return ret;
    }
}
