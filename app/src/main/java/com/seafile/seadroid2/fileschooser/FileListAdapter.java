package com.seafile.seadroid2.fileschooser;

import java.util.List;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

public class FileListAdapter extends BaseAdapter {

    private Activity mActivity;
    private List<SelectableFile> mFiles;

    public FileListAdapter(Activity activity) {
        this.mActivity = activity;
        mFiles = Lists.newArrayList();
    }

    @Override
    public int getCount() {
        return mFiles.size();
    }

    @Override
    public SelectableFile getItem(int position) {
        return mFiles.get(position);
    }

    public void addItem(SelectableFile file) {
        mFiles.add(file);
    }

    public void clear() {
        mFiles.clear();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public void setListItems(List<SelectableFile> files) {
        this.mFiles = files;
        notifyDataSetChanged();
    }

    public List<SelectableFile> getListItems() {
        return mFiles;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        SelectableFile item = mFiles.get(position);
        View view = convertView;
        Viewholder viewHolder;
        TextView title;
        TextView subtitle;
        ImageView icon;
        CheckBox checkBox;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_entry_check, null);
            title = (TextView) view.findViewById(R.id.list_item_title);
            subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            icon = (ImageView) view.findViewById(R.id.list_item_icon);
            checkBox = (CheckBox) view.findViewById(R.id.list_item_checkbox);
            viewHolder = new Viewholder(title, subtitle, icon, checkBox);
            view.setTag(viewHolder);

            viewHolder.checkBox.setOnClickListener(new View.OnClickListener() {

                @Override
                public void onClick(View v) {
                    CheckBox cb = (CheckBox) v;
                    SelectableFile file = (SelectableFile) cb.getTag();
                    file.setSelected(cb.isChecked());
                    ((MultiFileChooserActivity)mActivity).onFileChecked(file);

                }
            });

        } else {
            viewHolder = (Viewholder) convertView.getTag();
            title = viewHolder.title;
            subtitle = viewHolder.subtitle;
            icon = viewHolder.icon;
            checkBox = viewHolder.checkBox;
        }

        checkBox.setTag(item);

        checkBox.setChecked(item.isSelected());

        int iconID = item.getIcon();
        viewHolder.icon.setImageResource(iconID);
        viewHolder.title.setText(item.getTitle());
        viewHolder.subtitle.setText(item.getSubtitle());
        viewHolder.checkBox.setVisibility(item.isFile() ? View.VISIBLE : View.GONE);

        return view;
    }


    public class Viewholder {
        TextView title, subtitle;
        ImageView icon;
        CheckBox checkBox;

        public Viewholder(TextView title, TextView subtitle, ImageView icon, CheckBox checkBox) {
            super();
            this.icon = icon;
            this.checkBox = checkBox;
            this.title = title;
            this.subtitle = subtitle;

        }

    }


}
