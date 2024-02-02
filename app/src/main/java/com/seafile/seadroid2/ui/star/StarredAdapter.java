package com.seafile.seadroid2.ui.star;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.DiffUtil;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemStarredBinding;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.data.model.star.StarredModel;
import com.seafile.seadroid2.util.GlideApp;
import com.seafile.seadroid2.util.Utils;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;

public class StarredAdapter extends BaseAdapter<StarredModel, StarredViewHolder> {
    private final String SERVER = IO.getSingleton().getServerUrl();

    @NonNull
    @Override
    protected StarredViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemStarredBinding binding = ItemStarredBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new StarredViewHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull StarredViewHolder holder, int i, @Nullable StarredModel model) {
        if (null == model) {
            return;
        }

        holder.binding.itemTitle.setText(model.obj_name);
        if (model.deleted) {
            holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.red));
            holder.binding.itemSubtitle.setText(R.string.deleted);
        } else {
            holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.fancy_black));
            holder.binding.itemSubtitle.setText(model.getSubtitle());
        }


        //set item_icon
        if (model.is_dir) {
            if (TextUtils.equals(model.path, "/")) {
                if (model.repo_encrypted) {
                    holder.binding.itemIcon.setImageResource(R.drawable.repo_encrypted);
                } else {
                    holder.binding.itemIcon.setImageResource(R.drawable.repo);
                }
            } else {
                holder.binding.itemIcon.setImageResource(R.drawable.folder);
            }
        } else {
            if (TextUtils.isEmpty(model.encoded_thumbnail_src)) {
                holder.binding.itemIcon.setImageResource(Utils.getFileIcon(model.obj_name));
            } else {
                String url = convertThumbnailUrl(model.repo_id, model.path);
                GlideApp.with(getContext())
                        .load(GlideLoadConfig.getGlideUrl(url))
                        .apply(GlideLoadConfig.getOptions())
                        .into(holder.binding.itemIcon);
            }
        }
    }

    @SuppressLint("DefaultLocale")
    private String convertThumbnailUrl(String repoId, String filePath) {
        try {
            String pathEnc = URLEncoder.encode(filePath, "UTF-8");
            return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%d", SERVER, repoId, pathEnc, 48);
        } catch (UnsupportedEncodingException e) {
            return null;
        }
    }

    public void notifyDataChanged(List<StarredModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            submitList(list);
            return;
        }

        if (CollectionUtils.isEmpty(getItems())) {
            submitList(list);
            return;
        }

        DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffUtil.Callback() {
            @Override
            public int getOldListSize() {
                return getItems().size();
            }

            @Override
            public int getNewListSize() {
                return list.size();
            }

            @Override
            public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
                StarredModel oldModel = getItems().get(oldItemPosition);
                StarredModel newModel = list.get(newItemPosition);
                String oldFullPath = oldModel.path + oldModel.obj_name;
                String newFullPath = newModel.path + newModel.obj_name;

                return TextUtils.equals(oldFullPath, newFullPath);
            }

            @Override
            public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
                StarredModel oldModel = getItems().get(oldItemPosition);
                StarredModel newModel = list.get(newItemPosition);


                return TextUtils.equals(oldModel.repo_id, newModel.repo_id)
                        && TextUtils.equals(oldModel.repo_name, newModel.repo_name)
                        && TextUtils.equals(oldModel.mtime, newModel.mtime)
                        && TextUtils.equals(oldModel.path, newModel.path)
                        && TextUtils.equals(oldModel.obj_name, newModel.obj_name)
                        && TextUtils.equals(oldModel.user_email, newModel.user_email)
                        && TextUtils.equals(oldModel.user_name, newModel.user_name)
                        && TextUtils.equals(oldModel.user_contact_email, newModel.user_contact_email)
                        && oldModel.repo_encrypted == newModel.repo_encrypted
                        && oldModel.is_dir == newModel.is_dir
                        && oldModel.size == newModel.size;
            }
        });

        setItems(list);
        diffResult.dispatchUpdatesTo(this);
    }
}