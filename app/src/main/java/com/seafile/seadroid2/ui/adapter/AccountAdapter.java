package com.seafile.seadroid2.ui.adapter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import android.content.Context;
import android.graphics.Bitmap;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.assist.FailReason;
import com.nostra13.universalimageloader.core.display.FadeInBitmapDisplayer;
import com.nostra13.universalimageloader.core.display.RoundedBitmapDisplayer;
import com.nostra13.universalimageloader.core.listener.ImageLoadingListener;
import com.nostra13.universalimageloader.core.listener.SimpleImageLoadingListener;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.avatar.Avatar;
import com.seafile.seadroid2.ui.widget.CircleImageView;

/**
 * Base account adapter
 */
public abstract class AccountAdapter extends BaseAdapter {
    private static final String DEBUG_TAG = "AccountAdapter";
    
    private ArrayList<Account> items;
    private ArrayList<Avatar> avatars;
    private Context context;
    
    public AccountAdapter(Context context) {
        this.context = context;
        items = Lists.newArrayList();
        avatars = Lists.newArrayList();
    }

    @Override
    public int getCount() {
        return items.size();
    }

    @Override
    public boolean isEmpty() {
        return items.isEmpty();
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

    public void setItems(List<Account> items) {
        this.items = (ArrayList<Account>) items;
        notifyDataSetChanged();
        
    }

    public void setAvatars(ArrayList<Avatar> avatars) {
        this.avatars = avatars;
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public void clear() {
        items.clear();
    }

    private Viewholder viewHolder;

    protected abstract int getChildLayout();

    protected abstract int getChildTitleId();

    protected abstract int getChildSubTitleId();

    protected abstract int getChildIconId();

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        if (convertView == null) {
            view = LayoutInflater.from(context).inflate(getChildLayout(), null);
            TextView title = (TextView) view.findViewById(getChildTitleId());
            TextView subtitle = (TextView) view.findViewById(getChildSubTitleId());
            CircleImageView icon = (CircleImageView) view.findViewById(getChildIconId());
            viewHolder = new Viewholder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }
        Account account = items.get(position);
        viewHolder.title.setText(account.getServerHost());
        viewHolder.subtitle.setText(account.getEmail());
        if (getAvatarUrl(account) != null) {
            ImageLoader.getInstance().displayImage(getAvatarUrl(account), viewHolder.icon);
        }
        ImageLoader.getInstance().handleSlowNetwork(true);
        
        return view;
    }

    private String getAvatarUrl(Account account) {
        if (avatars == null) {
            return null;
        }
        for (Avatar avatar : avatars) {
            if (avatar.getSignature().equals(account.getSignature())) {
                return avatar.getUrl();
            }
        }

        return null;
    }

    private class Viewholder {
        TextView title, subtitle;
        CircleImageView icon;

        public Viewholder(TextView title, TextView subtitle, CircleImageView icon) {
            super();
            this.icon = icon;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
