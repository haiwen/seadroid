package com.seafile.seadroid;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.util.Log;
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


    @Override
    public View getView(int position, View convertView, ViewGroup parent) {      
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
        SeafItem item = items.get(position);
        viewHolder.title.setText(item.getTitle());
        if (item instanceof SeafRepo) {
            viewHolder.subtitle.setText(item.getSubtitle());
            viewHolder.icon.setImageResource(R.drawable.repo);
        } else {
            SeafDirent dirent = (SeafDirent)item;
            if (dirent.isDir()) {
                viewHolder.subtitle.setText("");
                viewHolder.icon.setImageResource(R.drawable.folder);
            } else {
                File file = DataManager.getFileForFileCache(dirent.name, dirent.id);
                if (file.exists())
                    viewHolder.subtitle.setText(item.getSubtitle() + " cached");
                else
                    viewHolder.subtitle.setText(item.getSubtitle());
                viewHolder.icon.setImageResource(R.drawable.file);
            }
        }

        return view;
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

