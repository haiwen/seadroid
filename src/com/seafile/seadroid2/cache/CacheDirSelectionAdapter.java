package com.seafile.seadroid2.cache;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import com.seafile.seadroid2.R;

/**
 *
 */
public class CacheDirSelectionAdapter  extends ArrayAdapter<String> {

    private CacheDirSelectionFragment mFragment;

    public CacheDirSelectionAdapter(CacheDirSelectionFragment fragment) {
        super(fragment.getActivity(), -1, fragment.getDirNamesList());
        mFragment = fragment;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        DirectoryMultiSelectionHolder holder;

        if (convertView == null) {
            convertView = LayoutInflater.from(mFragment.getActivity()).inflate(R.layout.cache_directory_list_item, parent, false);

            TextView fileDirectoryNameText = (TextView) convertView.findViewById(R.id.cache_directory_list_item_title_tv);
            ImageView fileDirectoryImage = (ImageView) convertView.findViewById(R.id.cache_directory_list_item_icon_iv);
            TextView fileDirectorySizeText = (TextView) convertView.findViewById(R.id.cache_directory_list_item_size_tv);
            holder = new DirectoryMultiSelectionHolder(fileDirectoryNameText, fileDirectorySizeText, fileDirectoryImage);
            convertView.setTag(holder);

        } else {
            holder = (DirectoryMultiSelectionHolder) convertView.getTag();
        }

        try {
            holder.fileDirectoryNameText.setText(mFragment.getDirNamesList().get(position));
            holder.fileDirectorySizeText.setText(mFragment.getDirSizesList().get(position));

        } catch (Exception e) {
            e.printStackTrace();
        }

        return convertView;
    }

    private class DirectoryMultiSelectionHolder {
        TextView fileDirectoryNameText, fileDirectorySizeText;
        ImageView fileDirectoryImage;

        public DirectoryMultiSelectionHolder(TextView name,
                                             TextView size,
                                             ImageView imageView) {
            this.fileDirectoryNameText = name;
            this.fileDirectorySizeText = size;
            this.fileDirectoryImage = imageView;
        }
    }

}
