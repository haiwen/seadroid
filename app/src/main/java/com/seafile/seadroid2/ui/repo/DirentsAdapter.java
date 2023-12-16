package com.seafile.seadroid2.ui.repo;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.SeafDirent;

import java.util.Collections;
import java.util.List;

public class DirentsAdapter extends BaseAdapter {

    private List<SeafDirent> dirents;

    public DirentsAdapter() {
        dirents = Lists.newArrayList();
    }

    /** sort files type */
    public static final int SORT_BY_NAME = 9;
    /** sort files type */
    public static final int SORT_BY_LAST_MODIFIED_TIME = 10;
    /** sort files order */
    public static final int SORT_ORDER_ASCENDING = 11;
    /** sort files order */
    public static final int SORT_ORDER_DESCENDING = 12;

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

    public void clearDirents() {
        dirents.clear();
    }

    public void setDirents(List<SeafDirent> dirents) {
        clearDirents();
        this.dirents.addAll(dirents);
        notifyDataSetChanged();
    }

    public void sortFiles(int type, int order) {
        List<SeafDirent> folders = Lists.newArrayList();
        List<SeafDirent> files = Lists.newArrayList();

        for (SeafDirent item : dirents) {
            if (item.isDir()) {
                folders.add(item);
            } else {
                files.add(item);
            }
        }

        dirents.clear();

        // sort SeafDirents
        if (type == SORT_BY_NAME) {
            // sort by name, in ascending order
            folders.sort(new SeafDirent.DirentNameComparator());
            files.sort(new SeafDirent.DirentNameComparator());
            if (order == SORT_ORDER_DESCENDING) {
                Collections.reverse(folders);
                Collections.reverse(files);
            }
        } else if (type == SORT_BY_LAST_MODIFIED_TIME) {
            // sort by last modified time, in ascending order
            folders.sort(new SeafDirent.DirentLastMTimeComparator());
            files.sort(new SeafDirent.DirentLastMTimeComparator());
            if (order == SORT_ORDER_DESCENDING) {
                Collections.reverse(folders);
                Collections.reverse(files);
            }
        }
        // Adds the objects in the specified collection to this ArrayList
        dirents.addAll(folders);
        dirents.addAll(files);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        ViewHolder viewHolder;
        SeafDirent dirent = dirents.get(position);

        if (convertView == null) {
            view = LayoutInflater.from(SeadroidApplication.getAppContext()).inflate(R.layout.list_item_entry, null);
            TextView title = view.findViewById(R.id.list_item_title);
            TextView subtitle = view.findViewById(R.id.list_item_subtitle);
            ImageView icon = view.findViewById(R.id.list_item_icon);
            viewHolder = new ViewHolder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (ViewHolder) convertView.getTag();
        }

        viewHolder.title.setText(dirent.getTitle());
        viewHolder.subtitle.setText(dirent.getSubtitle());
        viewHolder.icon.setImageResource(dirent.getIcon());

        int alpha;
        int titleColor;

        if (dirent.isDir()) {
            alpha = 255;
            titleColor = Color.BLACK;
        } else {
            alpha = 75;
            titleColor = Color.GRAY;
        }

        viewHolder.title.setTextColor(titleColor);
        viewHolder.subtitle.setTextColor(Color.GRAY);
        viewHolder.icon.setImageResource(alpha);

        return view;
    }

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