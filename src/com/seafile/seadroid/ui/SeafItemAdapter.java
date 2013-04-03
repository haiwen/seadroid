package com.seafile.seadroid.ui;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;

import net.londatiga.android.ActionItem;
import net.londatiga.android.QuickAction;

import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.seafile.seadroid.BrowserActivity;
import com.seafile.seadroid.NavContext;
import com.seafile.seadroid.R;
import com.seafile.seadroid.Utils;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.SeafCachedFile;
import com.seafile.seadroid.data.SeafDirent;
import com.seafile.seadroid.data.SeafGroup;
import com.seafile.seadroid.data.SeafItem;
import com.seafile.seadroid.data.SeafRepo;

public class SeafItemAdapter extends BaseAdapter {

    private ArrayList<SeafItem> items;
    private BrowserActivity mActivity;

    public SeafItemAdapter(BrowserActivity activity) {
        this.mActivity = activity;
        items = new ArrayList<SeafItem>();
    }

    private static int ACTION_ID_DELETE = 0;
    private static int ACTION_ID_RENAME = 1;
    private static int ACTION_ID_UPDATE = 2;

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
            view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            ImageView action = (ImageView) view.findViewById(R.id.list_item_action);
            viewHolder = new Viewholder(title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        viewHolder.title.setText(repo.getTitle());
        viewHolder.subtitle.setText(repo.getSubtitle());
        viewHolder.icon.setImageResource(repo.getIcon());
        viewHolder.action.setVisibility(View.INVISIBLE);
        return view;
    }

    private View getGroupView(SeafGroup group) {
        View view = LayoutInflater.from(mActivity).inflate(R.layout.group_item, null);
        TextView tv = (TextView) view.findViewById(R.id.textview_groupname);
        tv.setText(group.getTitle());
        return view;
    }

    private View getDirentView(SeafDirent dirent, View convertView, ViewGroup parent, int position) {
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            ImageView action = (ImageView) view.findViewById(R.id.list_item_action);
            viewHolder = new Viewholder(title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        viewHolder.title.setText(dirent.getTitle());
        if (dirent.isDir()) {
            viewHolder.subtitle.setText("");
            viewHolder.icon.setImageResource(dirent.getIcon());
            viewHolder.action.setVisibility(View.INVISIBLE);
        } else {
            setFileView(dirent, viewHolder, position);
        }

        return view;
    }

    private void setFileView(SeafDirent dirent, Viewholder viewHolder, int position) {
        NavContext nav = mActivity.getNavContext();
        DataManager dataManager = mActivity.getDataManager();
        String repoName = nav.getRepoName();
        String repoID = nav.getRepoID();
        String filePath = Utils.pathJoin(nav.getDirPath(), dirent.name);
        File file = dataManager.getLocalRepoFile(repoName, repoID, filePath);
        boolean modified = false;

        if (file.exists()) {
            // Detect if file is modified locally
            if (dataManager.isLocalFileModified(repoName, repoID, filePath)) {
                viewHolder.subtitle.setText(dirent.getSubtitle() + " modified");
                modified = true;
            } else {
                viewHolder.subtitle.setText(dirent.getSubtitle() + " cached");
            }

            if (Utils.isViewableImage(file.getName())) {
                setImageThumbNail(file, dirent, dataManager, viewHolder);
            } else
                viewHolder.icon.setImageResource(dirent.getIcon());
        } else {
            viewHolder.subtitle.setText(dirent.getSubtitle());
            viewHolder.icon.setImageResource(dirent.getIcon());
        }

        setFileAction(dirent, viewHolder, position, modified);
    }

    private void setImageThumbNail(File file, SeafDirent dirent,
                                   DataManager dataManager, Viewholder viewHolder) {
        if (file.length() < DataManager.MAX_DIRECT_SHOW_THUMB) {
            Bitmap imageBitmap = dataManager.getThumbnail(file);
            if (imageBitmap != null)
                viewHolder.icon.setImageBitmap(imageBitmap);
            else
                viewHolder.icon.setImageResource(dirent.getIcon());
        } else {
            File thumbFile = DataManager.getThumbFile(dirent.id);
            if (thumbFile.exists()) {
                Bitmap imageBitmap;
                try {
                    // setImageURI does not work correctly under high screen density
                    //viewHolder.icon.setScaleType(ImageView.ScaleType.FIT_XY);
                    //viewHolder.icon.setImageURI(Uri.fromFile(thumbFile));
                    imageBitmap = BitmapFactory.decodeStream(new FileInputStream(thumbFile));
                    viewHolder.icon.setImageBitmap(imageBitmap);
                } catch (FileNotFoundException e) {
                    viewHolder.icon.setImageResource(dirent.getIcon());
                }
            } else {
                viewHolder.icon.setImageResource(dirent.getIcon());
            }
        }
    }

    private View getCacheView(SeafCachedFile item, View convertView, ViewGroup parent) {
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_entry, null);
            TextView title = (TextView) view.findViewById(R.id.list_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.list_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.list_item_icon);
            ImageView action = (ImageView) view.findViewById(R.id.list_item_action);
            viewHolder = new Viewholder(title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        viewHolder.title.setText(item.getTitle());
        viewHolder.subtitle.setText(item.getSubtitle());
        viewHolder.icon.setImageResource(item.getIcon());
        viewHolder.action.setVisibility(View.INVISIBLE);
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
            return getDirentView((SeafDirent)item, convertView, parent, position);
        }
    }


    private class Viewholder {
        TextView title, subtitle;
        ImageView icon, action;

        public Viewholder(TextView title, TextView subtitle, ImageView icon, ImageView action) {
            super();
            this.icon = icon;
            this.action = action;
            this.title = title;
            this.subtitle = subtitle;
        }
    }

    private QuickAction prepareDirentAction(final SeafDirent dirent, boolean modified) {
        final QuickAction mQuickAction = new QuickAction(mActivity);
        Resources resources = mActivity.getResources();

        ActionItem deleteAction = new ActionItem(ACTION_ID_DELETE,
                                                 resources.getString(R.string.delete),
                                                 resources.getDrawable(R.drawable.ic_add));
        ActionItem renameAction = new ActionItem(ACTION_ID_RENAME,
                                                 resources.getString(R.string.rename),
                                                 resources.getDrawable(R.drawable.ic_accept));

        mQuickAction.addActionItem(deleteAction);
        mQuickAction.addActionItem(renameAction);

        if (modified) {
            ActionItem updateAction = new ActionItem(ACTION_ID_UPDATE,
                                                     resources.getString(R.string.update),
                                                     resources.getDrawable(R.drawable.ic_up));
            mQuickAction.addActionItem(updateAction);
        }

        //setup the action item click listener
        mQuickAction.setOnActionItemClickListener(new QuickAction.OnActionItemClickListener() {
            @Override
            public void onItemClick(QuickAction quickAction, int pos, int actionId) {
                if (actionId == ACTION_ID_UPDATE) {
                    NavContext nav = mActivity.getNavContext();
                    String repoName = nav.getRepoName();
                    String repoID = nav.getRepoID();
                    String dir = nav.getDirPath();
                    String path = Utils.pathJoin(dir, dirent.name);
                    String localPath = mActivity.getDataManager().getLocalRepoFile(repoName, repoID, path).getPath();
                    mActivity.addUpdateTask(repoID, repoName, dir, localPath);
                }
            }
        });

        // mQuickAction.setOnDismissListener(new QuickAction.OnDismissListener() {
        //     @Override
        //     public void onDismiss() {
        //     }
        // });

        // mQuickAction.setAnimStyle(QuickAction.ANIM_GROW_FROM_CENTER);
        mQuickAction.mAnimateTrack(false);
        return mQuickAction;
    }

    private void setFileAction(SeafDirent dirent, Viewholder viewHolder,
                               int position, final boolean modified) {
        viewHolder.action.setImageResource(R.drawable.drop_down_button);
        viewHolder.action.setVisibility(View.VISIBLE);
        viewHolder.action.setId(position);
        viewHolder.action.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View view) {
                int position = view.getId();
                SeafDirent dirent = (SeafDirent)items.get(position);
                QuickAction mQuickAction = prepareDirentAction(dirent, modified);
                mQuickAction.show(view);
            }
        });
    }
}

