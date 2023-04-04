package com.seafile.seadroid2.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SearchedFile;
import com.seafile.seadroid2.ui.activity.search.SearchActivity;
import com.seafile.seadroid2.util.Utils;

import java.util.List;

/**
 * Adapter for search list
 * <br>
 * Deprecated, use SearchRecyclerViewAdapter
 */
@Deprecated
public class SearchAdapter extends BaseAdapter {

    private List<SearchedFile> items;
    private SearchActivity mActivity;

    public static final int REFRESH_ON_NONE = 0;
    public static final int REFRESH_ON_PULL_DOWN = 1;
    public static final int REFRESH_ON_PULL_UP = 2;
    private int state = REFRESH_ON_NONE;

    public SearchAdapter(SearchActivity activity) {
        this.mActivity = activity;
        items = Lists.newArrayList();
    }

    public void setItems(List<SearchedFile> data) {
        this.items = data;
    }

    @Override
    public int getCount() {
        return items.size();
    }

    @Override
    public Object getItem(int position) {
        return items.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }
    private LinearLayout mFooterView;

    public void setFooterViewLoading(boolean more) {
        ProgressBar progress = (ProgressBar) mFooterView.findViewById(R.id.progressbar);
        TextView text = (TextView) mFooterView.findViewById(R.id.text);
        if (more) {
            mFooterView.setVisibility(View.VISIBLE);
            progress.setVisibility(View.VISIBLE);
            text.setVisibility(View.VISIBLE);
        } else {
            progress.setVisibility(View.GONE);
            mFooterView.setVisibility(View.GONE);
            text.setVisibility(View.GONE);
        }
    }
    public void setState(int state) {
        this.state = state;
    }

    public View getFooterView() {
        return this.mFooterView;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        SearchedFile item = items.get(position);
        View view = convertView;
        ViewHolder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.search_list_item, null);
            TextView path = (TextView) view.findViewById(R.id.search_item_path);
            TextView title = (TextView) view.findViewById(R.id.search_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.search_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.search_item_icon);
            viewHolder = new ViewHolder(path, title, subtitle, icon);
            view.setTag(viewHolder);
        } else {
            viewHolder = (ViewHolder) convertView.getTag();
        }

        viewHolder.icon.setImageResource(item.getIcon());
        viewHolder.path.setText(filePath(item));
        viewHolder.title.setText(item.getTitle());
        viewHolder.subtitle.setText(item.getSubtitle());

        return view;
    }

    private String filePath(SearchedFile searchedFile) {
        String parentPath = Utils.getParentPath(searchedFile.getPath());
        SeafRepo seafRepo = mActivity.getDataManager().getCachedRepoByID(searchedFile.getRepoID());
        if (seafRepo != null)
            return Utils.pathJoin(seafRepo.getName(), parentPath);
        else
            return parentPath;
    }

    public void notifyChanged() {
        notifyDataSetChanged();
    }

    private static class ViewHolder {
        TextView path, title, subtitle;
        ImageView icon;

        public ViewHolder(TextView path, TextView title, TextView subtitle, ImageView icon) {
            super();
            this.icon = icon;
            this.path = path;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
