package com.seafile.seadroid2.transfer;

import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;
import com.seafile.seadroid2.R;

/**
 * Section headers for transfer list
 */
public class TransferSectionHeader implements TransferItem {

    private TaskState taskState;
    private int count;

    public TransferSectionHeader(TaskState taskState, int count) {
        this.taskState = taskState;
        this.count = count;
    }

    @Override
    public int getViewType() {
        return 0;
    }

    @Override
    public View getView(LayoutInflater inflater, View convertView) {
        View view;
        if (convertView == null) {
            view = inflater.inflate(R.layout.listview_section_header, null);
        } else {
            view = convertView;
        }

        TextView tv = (TextView) view.findViewById(R.id.listview_section_header_txt);
        tv.setText(taskState.toString() + " (" + count + ")");

        return view;
    }
}