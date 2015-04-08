package com.seafile.seadroid2.cameraupload;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import android.widget.CompoundButton.OnCheckedChangeListener;
import com.seafile.seadroid2.R;

import java.util.Set;

public class LocalDirSelectionAdapter extends ArrayAdapter<String> {

    private Context mContext;
    private LocalDirSelectionFragment mFragment;
    private boolean mDirChecked;

    public LocalDirSelectionAdapter(Context context,
                                    LocalDirSelectionFragment fragment,
                                    boolean dirChecked) {
        super(context, -1, fragment.getDirNamesList());
        mContext = context;
        mFragment = fragment;
        mDirChecked = dirChecked; // Indicates if this entire dir is a photo folder.
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        DirectoryMultiSelectionHolder holder;

        if (convertView == null) {
            convertView = LayoutInflater.from(mContext).inflate(R.layout.cuc_directory_list_item, parent, false);

            TextView fileDirectoryNameText = (TextView) convertView.findViewById(R.id.cuc_directory_list_item_title_tv);
            CheckBox fileDirectoryCheckbox = (CheckBox) convertView.findViewById(R.id.cuc_directory_list_item_cb);
            ImageView fileDirectoryImage = (ImageView) convertView.findViewById(R.id.cuc_directory_list_item_icon_iv);
            TextView fileDirectorySizeText = (TextView) convertView.findViewById(R.id.cuc_directory_list_item_size_tv);
            holder = new DirectoryMultiSelectionHolder(fileDirectoryNameText, fileDirectorySizeText, fileDirectoryCheckbox, fileDirectoryImage);
            convertView.setTag(holder);

        } else {
            holder = (DirectoryMultiSelectionHolder) convertView.getTag();
        }

        try {
            holder.fileDirectoryNameText.setText(mFragment.getDirNamesList().get(position));
            holder.fileDirectorySizeText.setText(mFragment.getDirSizesList().get(position));

            // Set the corresponding path of the checkbox as its tag.
            holder.fileDirectoryCheckbox.setTag(mFragment.getDirList().get(position));

        } catch (Exception e) {
            e.printStackTrace();
        }

        // Set the checkbox status.
        String folderPath = mFragment.getDirList().get(position);
        if (mDirChecked) {
            holder.fileDirectoryCheckbox.setChecked(true);
            if (mFragment.getLocalDirHashMap().get(folderPath) != null &&
                    mFragment.getLocalDirHashMap().get(folderPath) == false) {
                holder.fileDirectoryCheckbox.setChecked(false);
            }

        } else {
            holder.fileDirectoryCheckbox.setChecked(false);
            if (mFragment.getLocalDirHashMap().get(folderPath) != null &&
                    mFragment.getLocalDirHashMap().get(folderPath) == true) {
                holder.fileDirectoryCheckbox.setChecked(true);
            }

        }

        holder.fileDirectoryCheckbox.setOnCheckedChangeListener(mCheckedChangeListener);
        return convertView;
    }

    /**
     * Checkbox status listener.
     */
    private OnCheckedChangeListener mCheckedChangeListener = new OnCheckedChangeListener() {

        @Override
        public void onCheckedChanged(CompoundButton checkBox, boolean isChecked) {

            //Only respond to user presses.
            if (checkBox.isPressed()) {
                String filePath = (String) checkBox.getTag();
                if (isChecked)
                    mFragment.getLocalDirHashMap().put(filePath, true);
                else if (mFragment.getLocalDirHashMap().containsKey(filePath))
                    removeKeyAndSubDirectories(filePath);
                else
                    mFragment.getLocalDirHashMap().put(filePath, false);

            }

        }

    };

    /**
     * Loops through the HashMap and removes the specified key and
     * all other keys that start with the specified key.
     */
    private void removeKeyAndSubDirectories(String key) {
        //Get a list of all file paths (keys).
        Set<String> keySet = mFragment.getLocalDirHashMap().keySet();
        String[] keyArray = new String[keySet.size()];
        keySet.toArray(keyArray);

        if (keyArray == null || keyArray.length == 0)
            return;

        for (int i = 0; i < keyArray.length; i++)
            if (keyArray[i].startsWith(key))
                mFragment.getLocalDirHashMap().remove(keyArray[i]);

    }

    private class DirectoryMultiSelectionHolder {
        TextView fileDirectoryNameText, fileDirectorySizeText;
        CheckBox fileDirectoryCheckbox;
        ImageView fileDirectoryImage;

        public DirectoryMultiSelectionHolder(TextView name,
                                             TextView size,
                                             CheckBox checkBox,
                                             ImageView imageView) {
            this.fileDirectoryNameText = name;
            this.fileDirectorySizeText = size;
            this.fileDirectoryCheckbox = checkBox;
            this.fileDirectoryImage = imageView;
        }
    }

}
