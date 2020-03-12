package com.seafile.seadroid2.ui.dialog;

import java.util.List;

import android.app.Dialog;
import android.content.DialogInterface;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.support.v7.app.AlertDialog;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;

/**
 * Choose an app from a list of apps or custom actions
 */
public class AppChoiceDialog extends DialogFragment {
    private List<ResolveInfo> mAppInfos = Lists.newArrayList();
    private OnItemSelectedListener mListener;
    private String mTitle;

    private List<CustomAction> customActions = Lists.newArrayList();

    public interface OnItemSelectedListener {
        void onAppSelected(ResolveInfo appInfo);
        void onCustomActionSelected(CustomAction action);
    }

    public void init(String title, List<ResolveInfo> appInfos, OnItemSelectedListener listener) {
        mAppInfos = appInfos;
        mListener = listener;
        mTitle = title;
    }

    public void addCustomAction(int id, Drawable icon, String description) {
        customActions.add(new CustomAction(id, icon, description));
    }

    private void onAppSelected(int index) {
        dismiss();
        if (index < customActions.size()) {
            CustomAction action = customActions.get(index);
            if (mListener != null) {
                mListener.onCustomActionSelected(action);
            }
        } else {
            ResolveInfo info = mAppInfos.get(index - customActions.size());
            if (mListener != null) {
                mListener.onAppSelected(info);
            }
        }
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        builder.setTitle(mTitle);
        builder.setSingleChoiceItems(new AppsListAdapter(), 0, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialogInterface, int i) {
                onAppSelected(i);
            }
        });

        return builder.create();
    }

    private class AppsListAdapter extends BaseAdapter {
        @Override
        public int getCount() {
            return customActions.size() + mAppInfos.size();
        }

        @Override
        public long getItemId(int position) {
            return position;
        }

        @Override
        public Object getItem(int position) {
            if (position < customActions.size()) {
                return customActions.get(position);
            }
            return mAppInfos.get(position - customActions.size());
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

            if (position < customActions.size()) {
                setCustomAction(viewHolder, customActions.get(position));
            } else {
                setAppInfo(viewHolder, mAppInfos.get(position - customActions.size()));
            }

            return view;
        }

        private void setCustomAction(Viewholder viewHolder, CustomAction customAction) {
            viewHolder.icon.setImageDrawable(customAction.icon);
            viewHolder.desc.setText(customAction.description);
        }

        private void setAppInfo(Viewholder viewHolder, ResolveInfo info) {
            PackageManager pm = getActivity().getPackageManager();
            CharSequence appDesc = info.activityInfo.loadLabel(pm);
            Drawable appIcon = info.activityInfo.loadIcon(pm);

            viewHolder.icon.setImageDrawable(appIcon);
            viewHolder.desc.setText(appDesc);
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

    public static class CustomAction {
        public final int id;
        public final Drawable icon;
        public final String description;

        public CustomAction(int id, Drawable icon, String description) {
            this.id = id;
            this.icon = icon;
            this.description = description;
        }
    }
}