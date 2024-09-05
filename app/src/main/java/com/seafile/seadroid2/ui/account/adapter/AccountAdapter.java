package com.seafile.seadroid2.ui.account.adapter;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.framework.util.GlideApp;

import java.util.ArrayList;
import java.util.List;

/**
 * Base account adapter
 */
public class AccountAdapter extends BaseAdapter {

    private ArrayList<Account> items;
    private Context context;

    public AccountAdapter(Context context) {
        this.context = context;
        items = Lists.newArrayList();
    }

    @Override
    public int getCount() {
        return items.size();
    }

    @Override
    public boolean isEmpty() {
        return items.isEmpty();
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

    public void setItems(List<Account> items) {
        this.items = (ArrayList<Account>) items;
        notifyDataSetChanged();

    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public void clear() {
        items.clear();
    }

    private ViewHolder viewHolder;

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        if (convertView == null) {
            view = LayoutInflater.from(context).inflate(R.layout.list_item_account_entry, null);
            TextView title = view.findViewById(R.id.list_item_account_title);
            TextView subtitle = view.findViewById(R.id.list_item_account_subtitle);
            ImageView icon = view.findViewById(R.id.list_item_account_icon);
            ImageView selectView = view.findViewById(R.id.item_select_view);
            viewHolder = new ViewHolder(title, subtitle, icon, selectView);
            view.setTag(viewHolder);
        } else {
            viewHolder = (ViewHolder) convertView.getTag();
        }

        Account account = items.get(position);
        viewHolder.selectView.setVisibility(account.is_checked ? View.VISIBLE : View.INVISIBLE);

        viewHolder.title.setText(account.getName());
        viewHolder.subtitle.setText(account.getServerHost());

        if (TextUtils.isEmpty(account.avatar_url)) {
            viewHolder.icon.setImageResource(com.seafile.seadroid2.R.drawable.default_avatar);
        } else {
            GlideApp.with(context)
                    .load(account.avatar_url)
                    .apply(GlideLoadConfig.getAvatarOptions())
                    .into(viewHolder.icon);
        }

        return view;
    }


    private static class ViewHolder {
        TextView title, subtitle;
        ImageView icon, selectView;

        public ViewHolder(TextView title, TextView subtitle, ImageView icon, ImageView selectView) {
            super();
            this.icon = icon;
            this.title = title;
            this.subtitle = subtitle;
            this.selectView = selectView;
        }
    }
}
