package com.seafile.seadroid2.ui;

import java.util.ArrayList;
import java.util.List;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.SeafDirent;

public class DirentsAdapter extends BaseAdapter {

    private List<SeafDirent> dirents;

    public DirentsAdapter() {
        dirents = new ArrayList<SeafDirent>();
    }

    @Override
    public int getCount() {
        return dirents.size();
    }

    @Override
    public boolean isEmpty() {
        return dirents.isEmpty();
    }

    public void add(SeafDirent entry) {
        dirents.add(entry);
    }

    @Override
    public SeafDirent getItem(int position) {
        return dirents.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public void setDirents(List<SeafDirent> dirents) {
        this.dirents.clear();
        for (SeafDirent dirent : dirents) {
            this.dirents.add(dirent);
        }
        notifyDataSetChanged();
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        Viewholder viewHolder;
        SeafDirent dirent = dirents.get(position);

        if (convertView == null) {
            view = LayoutInflater.from(SeadroidApplication.getAppContext()).
                    inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            ImageView action = (ImageView) view.findViewById(R.id.list_item_action);
            viewHolder = new Viewholder(title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        viewHolder.title.setText(dirent.getTitle());
        if (dirent.isDir()) {
            viewHolder.subtitle.setText("");
            viewHolder.icon.setImageResource(dirent.getIcon());

            viewHolder.title.setTextColor(Color.BLACK);
            viewHolder.subtitle.setTextColor(Color.BLACK);
            if (android.os.Build.VERSION.SDK_INT >= 11) {
                viewHolder.icon.setAlpha(255);
            }
        } else {
            viewHolder.subtitle.setText(dirent.getSubtitle());
            viewHolder.icon.setImageResource(dirent.getIcon());

            viewHolder.title.setTextColor(Color.GRAY);
            viewHolder.subtitle.setTextColor(Color.GRAY);
            if (android.os.Build.VERSION.SDK_INT >= 11) {
                viewHolder.icon.setAlpha(75);
            }
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