package com.seafile.seadroid2.loopimages;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.mcxtzhang.swipemenulib.SwipeMenuLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Cloud library adapter
 */
public class ChosenLibraryAdapter extends BaseAdapter {

    private List<DirInfo> dirInfos;

    private Map<String, DataManager> accountDataManager;

    public ChosenLibraryAdapter() {
        dirInfos = Lists.newLinkedList();
        accountDataManager = new HashMap<String, DataManager>();
    }

    /** sort files type */
    public static final int SORT_BY_NAME = 9;
    /** sort files type */
    public static final int SORT_BY_LAST_MODIFIED_TIME = 10;
    /** sort files order */
    public static final int SORT_ORDER_ASCENDING = 11;
    /** sort files order */
    public static final int SORT_ORDER_DESCENDING = 12;

    private DataManager getDataManager(Account account){
        if (!accountDataManager.containsKey(account.getSignature())) {
            accountDataManager.put(account.getSignature(), new DataManager(account));
        }
        return accountDataManager.get(account.getSignature());
    }

    @Override
    public int getCount() {
        return dirInfos.size();
    }

    @Override
    public boolean isEmpty() {
        return dirInfos.isEmpty();
    }

    @Override
    public DirInfo getItem(int position) {
        return dirInfos.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public void clearDirs() {
        dirInfos.clear();
    }

    public void setDirs(List<DirInfo> dirs) {
        clearDirs();
        for (DirInfo dir : dirs) {
            this.dirInfos.add(dir);
        }
        notifyDataSetChanged();
    }

    public void addDir(DirInfo dir){
        this.dirInfos.add(dir);
        notifyDataSetChanged();
    }

    public void sortDirs(int order) {
        Collections.sort(dirInfos, new DirInfo.DirInfoComparator());
        if (order == SORT_ORDER_DESCENDING) {
            Collections.reverse(dirInfos);
        }
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        SwipeMenuLayout view = (SwipeMenuLayout) convertView;
        ChosenLibraryAdapter.Viewholder viewHolder;
        DirInfo dir = dirInfos.get(position);

        if (convertView == null) {
            view = (SwipeMenuLayout) LayoutInflater.from(SeadroidApplication.getAppContext()).
                    inflate(R.layout.loop_images_chosen_library_list_item, null);
            TextView title = (TextView) view.findViewById(R.id.loopimages_list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.loopimages_list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.loopimages_list_item_icon);
            Button action = (Button) view.findViewById(R.id.loopimages_list_item_action);
            viewHolder = new ChosenLibraryAdapter.Viewholder(title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            view.quickClose();
            viewHolder = (ChosenLibraryAdapter.Viewholder) convertView.getTag();
        }

        viewHolder.icon.setImageResource(getDataManager(dir.getAccount()).getCachedRepoByID(dir.getRepoId()).getIcon());
        viewHolder.title.setText(dir.getRepoName() + ":" + dir.getDirPath());
        viewHolder.subtitle.setText(dir.getAccount().getDisplayName());
        viewHolder.action.setTag(position);
        viewHolder.action.setOnClickListener(new DeleteItemOnClickListener());

        viewHolder.title.setTextColor(Color.BLACK);
        viewHolder.subtitle.setTextColor(Color.GRAY);
        if (android.os.Build.VERSION.SDK_INT >= 11) {
            viewHolder.icon.setAlpha(255);
        }
        return view;
    }

    private class DeleteItemOnClickListener implements Button.OnClickListener{
        @Override
        public void onClick(View v) {
            Integer position = (Integer) v.getTag();
            deleteItem(position);
        }
    }

    private void deleteItem(int position){
        dirInfos.remove(position);
        notifyDataSetChanged();
    }

    private class Viewholder {
        TextView title, subtitle;
        ImageView icon;
        Button action;

        public Viewholder(TextView title, TextView subtitle, ImageView icon, Button action) {
            super();
            this.icon = icon;
            this.title = title;
            this.subtitle = subtitle;
            this.action = action;
        }
    }
}
