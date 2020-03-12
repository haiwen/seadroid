package com.seafile.seadroid2.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafItem;
import com.seafile.seadroid2.ui.NavContext;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class SeafItemCheckableAdapter extends BaseAdapter {

    public interface OnCheckedChangeListener {
        void onCheckedChanged(SeafItem item, boolean isChecked);
    }

    private ArrayList<SeafItemWrap> items;
    private BrowserActivity mActivity;
    private OnCheckedChangeListener listener = null;

    public SeafItemCheckableAdapter(BrowserActivity mActivity) {
        this.mActivity = mActivity;
        items = Lists.newArrayList();
    }

    public void setOnCheckedChangeListener(OnCheckedChangeListener listener) {
        this.listener = listener;
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
        SeafItemWrap w = new SeafItemWrap(entry);
        items.add(w);
        // Collections.sort(items);
        notifyDataSetChanged();
    }

    public void add(SeafItem entry) {
        SeafItemWrap w = new SeafItemWrap(entry);
        items.add(w);
    }

    public void notifyChanged() {
        notifyDataSetChanged();
    }

    @Override
    public SeafItem getItem(int position) {
        return items.get(position).item;
    }

    public void setItem(SeafItem item, int listviewPosition) {
        SeafItemWrap w = new SeafItemWrap(item);
        items.set(listviewPosition, w);
        notifyDataSetChanged();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public int getNumSelected() {
        int i = 0;
        for (SeafItemWrap w : items) {
            if (w.seleted)
                i++;
        }
        return i;
    }


    public List<SeafItem> getSelectedItems() {
        List<SeafItem> r = Lists.newArrayList();
        for (SeafItemWrap w : items) {
            if (w.seleted)
                r.add(w.item);
        }
        return r;
    }

    public void removeSelectedItems() {
        List<SeafItemWrap> tmp = Lists.newArrayList();
        for (SeafItemWrap w : items) {
            if (w.seleted)
                tmp.add(w);
        }
        items.removeAll(tmp);
        notifyDataSetChanged();
    }

    public void clear() {
        items.clear();
    }

    private View getDirentView(SeafItemWrap w, View convertView,
            ViewGroup parent, int position) {
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_entry_check, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            CheckBox ck = (CheckBox) view.findViewById(R.id.list_item_checkbox);
            viewHolder = new Viewholder(title, subtitle, icon, ck);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        viewHolder.title.setText(w.item.getTitle());
        SeafDirent dirent = (SeafDirent) w.item;
        if (dirent.isDir()) {
            viewHolder.subtitle.setText("");
        } else {
            NavContext nav = mActivity.getNavContext();
            DataManager dataManager = mActivity.getDataManager();
            String repoName = nav.getRepoName();
            String repoID = nav.getRepoID();
            String filePath = Utils.pathJoin(nav.getDirPath(), dirent.name);
            File file = dataManager.getLocalRepoFile(repoName, repoID, filePath);
            if (file.exists())
                viewHolder.subtitle.setText(dirent.getSubtitle() + " cached");
            else
                viewHolder.subtitle.setText(dirent.getSubtitle());
        }
        viewHolder.icon.setImageResource(dirent.getIcon());
        viewHolder.checkbox.setChecked(w.isSelected());
        return view;
    }

    private View getCacheView(SeafItemWrap w, View convertView,
            ViewGroup parent, int position) {
        View view = convertView;
        final Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_entry_check, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            CheckBox ck = (CheckBox) view.findViewById(R.id.list_item_checkbox);
            viewHolder = new Viewholder(title, subtitle, icon, ck);
            view.setTag(viewHolder);
            ck.setTag(items.get(position));

            ck.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                    SeafItemWrap w = (SeafItemWrap)viewHolder.checkbox.getTag();
                    w.setSeleted(isChecked);
                    if (listener != null) {
                        listener.onCheckedChanged(w.item, isChecked);
                    }
                }
            });
        } else {
            viewHolder = (Viewholder) convertView.getTag();
            viewHolder.checkbox.setTag(w);
        }

        viewHolder.title.setText(w.item.getTitle());
        viewHolder.subtitle.setText(w.item.getSubtitle());
        viewHolder.icon.setImageResource(w.item.getIcon());
        viewHolder.checkbox.setChecked(w.isSelected());
        return view;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        SeafItemWrap w = items.get(position);
        if (w.item instanceof SeafCachedFile) {
            return getCacheView(w, convertView, parent, position);
        } else {
            return getDirentView(w, convertView, parent, position);
        }
    }


    private class Viewholder {
        TextView title, subtitle;
        ImageView icon;
        CheckBox checkbox;

        public Viewholder(TextView title, TextView subtitle, ImageView icon, CheckBox checkbox) {
            this.icon = icon;
            this.title = title;
            this.subtitle = subtitle;
            this.checkbox = checkbox;
        }
    }

    public class SeafItemWrap {
        SeafItem item;
        boolean seleted;

        public SeafItemWrap(SeafItem item) {
            this.item = item;
            seleted = false;
        }

        public void setSeleted(boolean selected) {
            this.seleted = selected;
        }

        public boolean isSelected() {
            return seleted;
        }
    }

}

