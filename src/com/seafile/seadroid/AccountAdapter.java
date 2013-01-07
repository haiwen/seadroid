package com.seafile.seadroid;

import java.util.ArrayList;

import com.seafile.seadroid.account.Account;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

/**
 * Adapter for showing account in a list view.
 */
public class AccountAdapter extends BaseAdapter {

    private ArrayList<Account> items;
    private Context context;
    
    public AccountAdapter(Context context) {
        this.context = context;
        items = new ArrayList<Account>();
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
        Account item = items.get(position);
        viewHolder.title.setText(item.getServerHost());
        viewHolder.subtitle.setText(item.getEmail());
        viewHolder.icon.setImageResource(R.drawable.account);

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
