package com.seafile.seadroid2.transfer;

/**
 *
 */
import android.view.LayoutInflater;
import android.view.View;

public interface TransferItem {
    public int getViewType();
    public View getView(LayoutInflater inflater, View convertView);
}