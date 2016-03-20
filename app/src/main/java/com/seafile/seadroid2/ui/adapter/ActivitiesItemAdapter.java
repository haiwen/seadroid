package com.seafile.seadroid2.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafEvent;
import com.seafile.seadroid2.data.SeafItem;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.widget.CircleImageView;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Adapter for Activities tab
 */
public class ActivitiesItemAdapter extends BaseAdapter {
    public static final String DEBUG_TAG = ActivitiesItemAdapter.class.getSimpleName();

    public static final int REFRESH_ON_NONE = 0;
    public static final int REFRESH_ON_PULL_DOWN = 1;
    public static final int REFRESH_ON_PULL_UP = 2;
    private int state = REFRESH_ON_NONE;

    private ArrayList<SeafEvent> items;
    private BrowserActivity mActivity;
    private ImageLoader loader;
    private DisplayImageOptions options;

    public ActivitiesItemAdapter(BrowserActivity activity) {
        this.mActivity = activity;
        items = Lists.newArrayList();
        loader = ImageLoader.getInstance();
        options = new DisplayImageOptions.Builder()
                .extraForDownloader(mActivity.getAccount())
                .showStubImage(R.drawable.default_avatar)
                .showImageOnLoading(R.drawable.default_avatar)
                .showImageForEmptyUri(R.drawable.default_avatar)
                .showImageOnFail(R.drawable.default_avatar)
                .resetViewBeforeLoading()
                .cacheInMemory(true)
                .cacheOnDisk(true)
                .considerExifParams(true)
                .build();
    }

    @Override
    public int getCount() {
        return items.size() + 1;
    }

    public void clear() {
        items.clear();
    }

    public void add(SeafEvent entry) {
        items.add(entry);
    }

    public void notifyChanged() {
        notifyDataSetChanged();
    }

    @Override
    public SeafItem getItem(int position) {
        return items.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public void setItems(List<SeafEvent> events) {
        items.clear();
        items.addAll(events);
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
        if (position == getCount() - 1) {
            this.mFooterView = (LinearLayout) LayoutInflater.from(parent.getContext()).inflate(R.layout.footer_load_more, null);
            switch (state) {
                case REFRESH_ON_NONE:
                case REFRESH_ON_PULL_DOWN:
                    setFooterViewLoading(false);
                    break;
                case REFRESH_ON_PULL_UP:
                    setFooterViewLoading(true);
                    break;
            }
            return mFooterView;
        }
        if (position < 0) {
            position = 0;
        }

        final SeafEvent item = items.get(position);
        View view = convertView;
        // TODO optimize by setting tags
        final ViewHolder viewHolder;

        view = LayoutInflater.from(mActivity).inflate(R.layout.list_item_activities, null);
        TextView title = (TextView) view.findViewById(R.id.tv_activities_mod_desc);
        TextView nick = (TextView) view.findViewById(R.id.tv_activities_nick);
        TextView date = (TextView) view.findViewById(R.id.tv_activities_date);
        TextView repoName = (TextView) view.findViewById(R.id.tv_activities_repo_name);
        CircleImageView icon = (CircleImageView) view.findViewById(R.id.iv_activities_avatar);
        viewHolder = new ViewHolder(title, nick, date, repoName, icon);
        view.setTag(viewHolder);

        final String raw = item.getAvatar();
        final String split = raw.substring(raw.indexOf("\"") + 1);
        String valid = split.substring(0, split.indexOf("\""));
        // TODO implement this by regex
        final String avatar = Utils.pathJoin(mActivity.getAccount().getServer(), valid);
        loader.displayImage(avatar, viewHolder.icon, options);

        viewHolder.title.setText(item.getDesc());
        viewHolder.nick.setText(item.getNick());

        final String relative = item.getTime_relative();
        final String substring = relative.substring(relative.indexOf(">") + 1);
        final String validTime = substring.substring(0, substring.indexOf("<"));
        // TODO implement this by regex
        viewHolder.date.setText(validTime);
        viewHolder.repoName.setText(item.getRepo_name());
        return view;
    }

    private class ViewHolder {
        TextView title, nick, date, repoName;
        CircleImageView icon;

        public ViewHolder(TextView title, TextView nick, TextView date, TextView repoName, CircleImageView icon) {
            super();
            this.icon = icon;
            this.title = title;
            this.nick = nick;
            this.date = date;
            this.repoName = repoName;
        }
    }

}
