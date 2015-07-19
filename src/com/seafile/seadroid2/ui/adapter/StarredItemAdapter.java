package com.seafile.seadroid2.ui.adapter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import android.graphics.Bitmap;
import com.google.common.collect.Lists;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.display.FadeInBitmapDisplayer;
import com.nostra13.universalimageloader.core.listener.ImageLoadingListener;
import com.nostra13.universalimageloader.core.listener.SimpleImageLoadingListener;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafItem;
import com.seafile.seadroid2.data.SeafStarredFile;
import com.seafile.seadroid2.ui.AnimateFirstDisplayListener;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.activity.BrowserActivity;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import com.seafile.seadroid2.util.Utils;

public class StarredItemAdapter extends BaseAdapter {

    private ArrayList<SeafItem> items;
    private BrowserActivity mActivity;

    public StarredItemAdapter(BrowserActivity activity) {
        this.mActivity = activity;
        items = Lists.newArrayList();
    }

    @Override
    public int getCount() {
        return items.size();
    }

    public void clear() {
        items.clear();
    }

    public void add(SeafItem entry) {
        items.add(entry);
    }

    public void notifyChanged() {
        notifyDataSetChanged();
    }

    @Override
    public SeafItem getItem(int position) {
        return items.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        SeafItem item = items.get(position);
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            ImageView action = (ImageView) view.findViewById(R.id.list_item_action);
            viewHolder = new Viewholder(title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        int iconID = item.getIcon();
        viewHolder.icon.setImageResource(iconID);
        viewHolder.title.setText(item.getTitle());
        viewHolder.subtitle.setText(item.getSubtitle());

        if (Utils.isViewableImage(item.getTitle())) {
            DisplayImageOptions options = new DisplayImageOptions.Builder()
                    .extraForDownloader(mActivity.getDataManager().getAccount())
                    .delayBeforeLoading(500)
                    .resetViewBeforeLoading(true)
                    .showImageOnLoading(R.drawable.file_image)
                    .showImageForEmptyUri(R.drawable.file_image)
                    .showImageOnFail(R.drawable.file_image)
                    .cacheInMemory(true)
                    .cacheOnDisk(true)
                    .considerExifParams(true)
                    .build();

            ImageLoadingListener animateFirstListener = new AnimateFirstDisplayListener();
            String url = mActivity.getDataManager().getThumbnailLink(((SeafStarredFile) item).getRepoID(), ((SeafStarredFile) item).getPath(), WidgetUtils.getThumbnailWidth());
            if (url == null) {
                ImageLoader.getInstance().displayImage("drawable://" + item.getIcon(), viewHolder.icon, WidgetUtils.iconOptions);
            } else
                ImageLoader.getInstance().displayImage(url, viewHolder.icon, options, animateFirstListener);
        } else {
            ImageLoader.getInstance().displayImage("drawable://" + item.getIcon(), viewHolder.icon, WidgetUtils.iconOptions);
        }

        return view;
    }

    private class Viewholder {
        TextView title, subtitle;
        ImageView icon, action;

        public Viewholder(TextView title, TextView subtitle, ImageView icon, ImageView action) {
            super();
            this.icon = icon;
            this.action = action;
            this.title = title;
            this.subtitle = subtitle;
        }
    }


}
