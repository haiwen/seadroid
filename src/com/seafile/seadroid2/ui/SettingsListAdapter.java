package com.seafile.seadroid2.ui;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.seafile.seadroid2.R;

public class SettingsListAdapter extends BaseAdapter {

	private int[] icons;
	private int[] strs;
	LayoutInflater inflater;
	Context context;
	
	/**
	 * 
	 * @param icons
	 * @param strs
	 * @param context
	 */
	public SettingsListAdapter(int[] icons, int[] strs, Context context) {
		this.icons = icons;
		this.strs = strs;
		this.context = context;
	}

	@Override
	public int getCount() {
		// TODO Auto-generated method stub
		return icons.length;
	}

	@Override
	public Object getItem(int position) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long getItemId(int position) {
		// TODO Auto-generated method stub
		return position;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		LayoutInflater inflater = LayoutInflater.from(context);
		View row = inflater.inflate(R.layout.list_item_settings, parent, false);
		TextView title;
		ImageView icon, arrow;
		icon = (ImageView) row.findViewById(R.id.iv_list_item_settings_icon);
		arrow = (ImageView) row.findViewById(R.id.iv_list_item_settings_arrow);
		title = (TextView) row.findViewById(R.id.tv_list_item_settings);
		icon.setImageResource(icons[position]);
		title.setText(strs[position]);
		return (row);
	}

}
