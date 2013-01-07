package com.seafile.seadroid.ui;

import java.io.File;
import java.util.ArrayList;

import com.seafile.seadroid.R;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.SeafCachedFile;
import com.seafile.seadroid.data.SeafDirent;
import com.seafile.seadroid.data.SeafGroup;
import com.seafile.seadroid.data.SeafItem;
import com.seafile.seadroid.data.SeafRepo;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

public class SeafItemAdapter extends BaseAdapter {

    private ArrayList<SeafItem> items;
    private Context context;
    
    public SeafItemAdapter(Context context) {
        this.context = context;
        items = new ArrayList<SeafItem>();
    }
    
    @Override
    public int getCount() {
        return items.size();
    }
    
    @Override 
    public boolean isEmpty() {
        return items.isEmpty();
    }

    public void addEntry(SeafItem entry) {
        items.add(entry);
        // Collections.sort(items);
        notifyDataSetChanged();
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
    
    public void setItem(SeafItem item, int listviewPosition) {
        items.set(listviewPosition, item);
        notifyDataSetChanged();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }
    
    public void clear() {
        items.clear();
    }
    
    public boolean areAllItemsSelectable() {
        return false;
    }
    
    public boolean isEnable(int position) {
        SeafItem item = items.get(position);
        return !(item instanceof SeafGroup);
    }
    
    public boolean isClickable(int position) {
        SeafItem item = items.get(position);
        return !(item instanceof SeafGroup);
    }
    
    
    public int getViewTypeCount() {
        return 2;
    }
    
    public int getItemViewType(int position) {
        SeafItem item = items.get(position);
        if (item instanceof SeafGroup)
            return 0;
        else
            return 1;
    }
    
    private View getRepoView(SeafRepo repo, View convertView, ViewGroup parent) {
        View view = convertView;
        Viewholder viewHolder;
        
        if (convertView == null) {
            view = LayoutInflater.from(context).inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            viewHolder = new Viewholder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }
        
        viewHolder.title.setText(repo.getTitle());
        viewHolder.subtitle.setText(repo.getSubtitle());
        viewHolder.icon.setImageResource(repo.getIcon());
        return view;
    }
    
    private View getGroupView(SeafGroup group) {
        View view = LayoutInflater.from(context).inflate(R.layout.group_item, null);
        TextView tv = (TextView) view.findViewById(R.id.textview_groupname);
        tv.setText(group.getTitle());
        return view;
    }
    
    private View getDirentView(SeafDirent dirent, View convertView, ViewGroup parent) {
        View view = convertView;
        Viewholder viewHolder;
        
        if (convertView == null) {
            view = LayoutInflater.from(context).inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            viewHolder = new Viewholder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }
        
        viewHolder.title.setText(dirent.getTitle());
        if (dirent.isDir()) {
            viewHolder.subtitle.setText("");
        } else {
            File file = DataManager.getFileForFileCache(dirent.name, dirent.id);
            if (file.exists())
                viewHolder.subtitle.setText(dirent.getSubtitle() + " cached");
            else
                viewHolder.subtitle.setText(dirent.getSubtitle());
        }
        viewHolder.icon.setImageResource(dirent.getIcon());
        return view;
    }
    
    private View getCacheView(SeafCachedFile item, View convertView, ViewGroup parent) {
        View view = convertView;
        Viewholder viewHolder;
        
        if (convertView == null) {
            view = LayoutInflater.from(context).inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            viewHolder = new Viewholder(title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }
        
        viewHolder.title.setText(item.getTitle());
        viewHolder.subtitle.setText(item.getSubtitle());
        viewHolder.icon.setImageResource(item.getIcon());
        return view;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {   
        SeafItem item = items.get(position);
        if (item instanceof SeafRepo) {
            return getRepoView((SeafRepo)item, convertView, parent);
        } else if (item instanceof SeafGroup) {
            return getGroupView((SeafGroup)item);
        } else if (item instanceof SeafCachedFile) {
            return getCacheView((SeafCachedFile)item, convertView, parent);
        } else {
            return getDirentView((SeafDirent)item, convertView, parent);
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

