package com.seafile.seadroid2.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.avatar.Avatar;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;

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
            ImageView icon = (ImageView) view.findViewById(getChildIconId());
            viewHolder = new Viewholder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }
        Account account = items.get(position);
        viewHolder.title.setText(account.getServerHost());
//        viewHolder.subtitle.setText(account.getEmail());
        viewHolder.subtitle.setText(account.getName());
        if (getAvatarUrl(account) != null) {
            Utils.glideCircle(context, getAvatarUrl(account), account.token, viewHolder.icon,
                    WidgetUtils.getThumbnailWidth(), WidgetUtils.getThumbnailWidth());
        }
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
        ImageView icon;

        public Viewholder(TextView title, TextView subtitle, ImageView icon) {
            super();
            this.icon = icon;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
