package com.seafile.seadroid2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import android.content.Context;
import android.graphics.Bitmap;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.display.FadeInBitmapDisplayer;
import com.nostra13.universalimageloader.core.display.RoundedBitmapDisplayer;
import com.nostra13.universalimageloader.core.listener.ImageLoadingListener;
import com.nostra13.universalimageloader.core.listener.SimpleImageLoadingListener;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.AvatarManager;

/**
 * Adapter for showing account in a list view.
 */
public class AccountAdapter extends BaseAdapter {
    private static final String DEBUG_TAG = "AccountAdapter";
    
    private ImageLoadingListener animateFirstListener = new AnimateFirstDisplayListener();
    private DisplayImageOptions options;
    private ArrayList<Account> items;
    private Context context;
    
    public AccountAdapter(Context context) {
        this.context = context;
        if (avatars == null) {
            avatars = Maps.newHashMap();
        }
        items = Lists.newArrayList();
        options = new DisplayImageOptions.Builder()
                .showStubImage(R.drawable.default_avatar)
                .delayBeforeLoading(1000)
                .showImageOnLoading(R.drawable.default_avatar)
                .showImageForEmptyUri(R.drawable.default_avatar)
                .showImageOnFail(R.drawable.default_avatar)
                .resetViewBeforeLoading()
                .cacheInMemory(true)
                .cacheOnDisk(true)
                .considerExifParams(true)
                .displayer(new RoundedBitmapDisplayer(50))
                .build();
    }

    @Override
    public int getCount() {
        return items.size();
    }

    @Override
    public boolean isEmpty() {
        return items.isEmpty();
    }

    public void addEntry(Account entry) {
        items.add(entry);
        notifyDataSetChanged();
    }

    public void add(Account entry) {
        items.add(entry);
    }

    public void notifyChanged() {
        notifyDataSetChanged();
    }

    @Override
    public Account getItem(int position) {
        return items.get(position);
    }

    public void setItem(Account item, int listviewPosition) {
        items.set(listviewPosition, item);
        notifyDataSetChanged();
    }
    public void setItems(List<Account> items) {
        this.items = (ArrayList<Account>) items;
        notifyDataSetChanged();
        
    }
    
    @Override
    public long getItemId(int position) {
        return position;
    }

    public void clear() {
        items.clear();
    }

    private Viewholder viewHolder;
    
    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        if (convertView == null) {
            view = LayoutInflater.from(context).inflate(R.layout.list_item_account_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_account_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_account_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_account_icon);
            viewHolder = new Viewholder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }
        Account item = items.get(position);
        viewHolder.title.setText(item.getServerHost());
        viewHolder.subtitle.setText(item.getEmail());
        if (AvatarManager.getAvatarUrl(item) != null) {
            ImageLoader.getInstance().displayImage(AvatarManager.getAvatarUrl(item), viewHolder.icon, options, animateFirstListener);
        } else {
            // off-line
        }
        // ImageLoader.getInstance().handleSlowNetwork(true);
        
        return view;
    }
    
    private static class AnimateFirstDisplayListener extends SimpleImageLoadingListener {

        static final List<String> displayedImages = Collections.synchronizedList(new LinkedList<String>());

        @Override
        public void onLoadingComplete(String imageUri, View view, Bitmap loadedImage) {
            if (loadedImage != null) {
                ImageView imageView = (ImageView) view;
                boolean firstDisplay = !displayedImages.contains(imageUri);
                if (firstDisplay) {
                    FadeInBitmapDisplayer.animate(imageView, 500);
                    displayedImages.add(imageUri);
                }
            }
        }
    }
    
    private class Viewholder {
        TextView title, subtitle;
        ImageView icon;

        public Viewholder(TextView title, TextView subtitle, ImageView icon) {
            super();
            this.icon = icon;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
