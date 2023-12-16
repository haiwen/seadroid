package com.seafile.seadroid2.ui.account.adapter;

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
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.util.GlideApp;

import java.util.ArrayList;
import java.util.List;

/**
 * Base account adapter
 */
public abstract class AccountAdapter extends BaseAdapter {
    private static final String DEBUG_TAG = "AccountAdapter";

    //    private ImageLoadingListener animateFirstListener = new AnimateFirstDisplayListener();

//    private DisplayImageOptions options;
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

    private ViewHolder viewHolder;

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
            viewHolder = new ViewHolder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (ViewHolder) convertView.getTag();
        }
        Account account = items.get(position);
        viewHolder.title.setText(account.getServerHost());
//        viewHolder.subtitle.setText(account.getEmail());
        viewHolder.subtitle.setText(account.getName());
        if (getAvatarUrl(account) != null) {
            GlideApp.with(viewHolder.icon)
                    .load(GlideLoadConfig.getGlideUrl(getAvatarUrl(account)))
                    .apply(GlideLoadConfig.getOptions())
                    .into(viewHolder.icon);
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

//    private static class AnimateFirstDisplayListener extends SimpleImageLoadingListener {
//
//        static final List<String> displayedImages = Collections.synchronizedList(new LinkedList<String>());
//
//        @Override
//        public void onLoadingComplete(String imageUri, View view, Bitmap loadedImage) {
//            if (loadedImage != null) {
//                ImageView imageView = (ImageView) view;
//                boolean firstDisplay = !displayedImages.contains(imageUri);
//                if (firstDisplay) {
//                    FadeInBitmapDisplayer.animate(imageView, 500);
//                    displayedImages.add(imageUri);
//                }
//            }
//        }
//    }

    private static class ViewHolder {
        TextView title, subtitle;
        ImageView icon;

        public ViewHolder(TextView title, TextView subtitle, ImageView icon) {
            super();
            this.icon = icon;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
