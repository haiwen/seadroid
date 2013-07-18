package com.seafile.seadroid2.ui;

import java.util.List;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.seafile.seadroid2.R;

/**
 * Choose an app from a list of apps
 */
public class AppChoiceDialog extends DialogFragment {
    private List<ResolveInfo> mAppInfos;
    private OnAppSelectedListener mListener;

    public interface OnAppSelectedListener {
        public void onAppSelected(ResolveInfo appInfo);
    }

    public void init(List<ResolveInfo> appInfos, OnAppSelectedListener listener) {
        mAppInfos = appInfos;
        mListener = listener;
    }

    private void onAppSelected(int index) {
        ResolveInfo info = mAppInfos.get(index);
        dismiss();
        if (mListener != null) {
            mListener.onAppSelected(info);
        }
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        builder.setTitle(R.string.export_file);
        ListView listView = new ListView(getActivity());
        listView.setAdapter(new AppsListAdapter());
        listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        builder.setView(listView);
        listView.setOnItemClickListener(new ListView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> l, View view, int position, long id) {
                onAppSelected(position);
            }
        });

        return builder.create();
    }

    private class AppsListAdapter extends BaseAdapter {
        @Override
        public int getCount() {
            return mAppInfos.size();
        }

        @Override
        public long getItemId(int position) {
            return position;
        }

        @Override
        public Object getItem(int position) {
            return mAppInfos.get(position);
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            View view = convertView;
            Viewholder viewHolder;

            if (convertView == null) {
                view = LayoutInflater.from(getActivity()).inflate(R.layout.app_list_item, null);
                ImageView icon = (ImageView)view.findViewById(R.id.app_icon);
                TextView desc = (TextView)view.findViewById(R.id.app_desc);
                viewHolder = new Viewholder(icon, desc);
                view.setTag(viewHolder);
            } else {
                viewHolder = (Viewholder)convertView.getTag();
            }

            ResolveInfo info = mAppInfos.get(position);

            PackageManager pm = getActivity().getPackageManager();
            CharSequence appDesc = info.activityInfo.loadLabel(pm);
            Drawable appIcon = info.activityInfo.loadIcon(pm);

            viewHolder.icon.setImageDrawable(appIcon);
            viewHolder.desc.setText(appDesc);

            return view;
        }

        private class Viewholder {
            ImageView icon;
            TextView desc;

            Viewholder(ImageView icon, TextView desc) {
                this.icon = icon;
                this.desc = desc;
            }
        }

    }

}