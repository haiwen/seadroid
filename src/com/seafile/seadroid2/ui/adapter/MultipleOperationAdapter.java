package com.seafile.seadroid2.ui.adapter;

import android.util.SparseBooleanArray;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.*;
import com.seafile.seadroid2.ui.activity.MultipleOperationActivity;

import java.util.Collections;
import java.util.List;

/**
 * Adapter for multiple files operations
 */
public class MultipleOperationAdapter extends BaseAdapter {
    public static final String DEBUG_TAG = "FileMultiOperationAdapter";

    private MultipleOperationActivity mActivity;
    private List<SeafDirent> dirents;

    private SparseBooleanArray mSelectedItemsIds;
    private List<Integer> mSelectedItemsPositions = Lists.newArrayList();
    private List<SeafDirent> mSelectedItemsValues = Lists.newArrayList();
    /**
     * flag to mark if action mode was activated, used to update the state of multi selection buttons
     */
    //private boolean actionModeStarted;

    public MultipleOperationAdapter(MultipleOperationActivity activity, List<SeafDirent> dirents) {
        this.mSelectedItemsIds = new SparseBooleanArray();
        this.mActivity = activity;
        this.dirents = dirents;
    }

    public void setItems(List<SeafDirent> dirents) {
        this.dirents = dirents;
        this.mSelectedItemsIds.clear();
        this.mSelectedItemsPositions.clear();
        this.mSelectedItemsValues.clear();
    }

    @Override
    public int getCount() {
        return dirents.size();
    }

    @Override
    public Object getItem(int position) {
        return dirents.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {
        final SeafDirent dirent = dirents.get(position);
        View view = convertView;
        final Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.file_operation_item, null);
            TextView title = (TextView) view.findViewById(R.id.file_operation_title_tv);
            TextView subtitle = (TextView) view.findViewById(R.id.file_operation_subtitle_tv);
            ImageView icon = (ImageView) view.findViewById(R.id.file_operation_icon_iv);
            ImageView multiSelectBtn = (ImageView) view.findViewById(R.id.file_operation_action_iv);
            viewHolder = new Viewholder(title, subtitle, icon, multiSelectBtn);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        if (mSelectedItemsIds.get(position)) {
            viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_checked);
        } else
            viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_unchecked);

        viewHolder.multiSelectBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (!mSelectedItemsIds.get(position)) {
                    viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_checked);
                    mSelectedItemsIds.put(position, true);
                    mSelectedItemsPositions.add(position);
                    mSelectedItemsValues.add(dirent);
                } else {
                    viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_unchecked);
                    mSelectedItemsIds.delete(position);
                    mSelectedItemsPositions.remove(Integer.valueOf(position));
                    mSelectedItemsValues.remove(dirent);
                }

                mActivity.onItemSelected();
            }
        });

        viewHolder.title.setText(dirent.getTitle());
        viewHolder.subtitle.setText(dirent.getSubtitle());
        viewHolder.icon.setImageResource(dirent.getIcon());
        viewHolder.multiSelectBtn.setVisibility(View.VISIBLE);

        return view;
    }

    private class Viewholder {
        TextView title, subtitle;
        ImageView icon, multiSelectBtn;

        public Viewholder(TextView title, TextView subtitle, ImageView icon, ImageView multiSelectBtn) {
            super();
            this.icon = icon;
            this.multiSelectBtn = multiSelectBtn;
            this.title = title;
            this.subtitle = subtitle;
        }
    }

    public int getCheckedItemCount() {
        return mSelectedItemsIds.size();
    }

    public List<SeafDirent> getSelectedItemsValues() {
        return mSelectedItemsValues;
    }

    public void toggleSelection(int position) {
        if (mSelectedItemsIds.get(position)) {
            // unselected
            mSelectedItemsIds.delete(position);
            mSelectedItemsPositions.remove(Integer.valueOf(position));
            mSelectedItemsValues.remove(dirents.get(position));
        } else {
            mSelectedItemsIds.put(position, true);
            mSelectedItemsPositions.add(position);
            mSelectedItemsValues.add(dirents.get(position));
        }

        mActivity.onItemSelected();
        notifyDataSetChanged();
    }

    public void deselectAllItems() {
        mSelectedItemsIds.clear();
        mSelectedItemsPositions.clear();
        mSelectedItemsValues.clear();
        notifyDataSetChanged();
    }

    public void selectAllItems() {
        mSelectedItemsIds.clear();
        mSelectedItemsPositions.clear();
        mSelectedItemsValues.clear();
        for (int i = 0; i < dirents.size(); i++) {
            mSelectedItemsIds.put(i, true);
            mSelectedItemsPositions.add(i);
            mSelectedItemsValues.add(dirents.get(i));
        }
        notifyDataSetChanged();
    }

    public void sortFiles(int type, int order) {
        List<SeafDirent> folders = Lists.newArrayList();
        List<SeafDirent> files = Lists.newArrayList();

        for (SeafDirent dirent : dirents) {
            if (dirent.isDir())
                folders.add(dirent);
            else
                files.add(dirent);
        }

        dirents.clear();

        // sort SeafDirents
        if (type == SeafItemAdapter.SORT_BY_NAME) {
            // sort by name, in ascending order
            Collections.sort(folders, new SeafDirent.DirentNameComparator());
            Collections.sort(files,   new SeafDirent.DirentNameComparator());
            if (order == SeafItemAdapter.SORT_ORDER_DESCENDING) {
                Collections.reverse(folders);
                Collections.reverse(files);
            }
        } else if (type == SeafItemAdapter.SORT_BY_LAST_MODIFIED_TIME) {
            // sort by last modified time, in ascending order
            Collections.sort(folders, new SeafDirent.DirentLastMTimeComparator());
            Collections.sort(files,   new SeafDirent.DirentLastMTimeComparator());
            if (order == SeafItemAdapter.SORT_ORDER_DESCENDING) {
                Collections.reverse(folders);
                Collections.reverse(files);
            }
        }
        // Adds the objects in the specified collection to this ArrayList
        dirents.addAll(folders);
        dirents.addAll(files);
    }
}
