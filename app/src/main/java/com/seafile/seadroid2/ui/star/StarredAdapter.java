package com.seafile.seadroid2.ui.star;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.DiffUtil;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.EncryptUtils;
import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.signature.ObjectKey;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemStarredBinding;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class StarredAdapter extends BaseAdapter<StarredModel, StarredViewHolder> {
    private final String SERVER = HttpIO.getCurrentInstance().getServerUrl();
    private boolean isSelectMode = false;

    public void setSelectMode(boolean selectMode) {
        isSelectMode = selectMode;
    }

    @NonNull
    @Override
    protected StarredViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemStarredBinding binding = ItemStarredBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new StarredViewHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull StarredViewHolder holder, int i, @Nullable StarredModel model) {
        if (isSelectMode) {
            onBindForSelectMode(holder, i, model);
        } else {
            onBind(holder, i, model);
        }
    }

    private void onBind(@NonNull StarredViewHolder holder, int i, @Nullable StarredModel model) {
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

        holder.binding.itemCheckbox.setVisibility(View.GONE);
        holder.binding.expandableToggleButton.setVisibility(View.VISIBLE);

        //set item_icon
        if (model.deleted || TextUtils.isEmpty(model.encoded_thumbnail_src) || !Utils.isViewableImage(model.obj_name) || model.repo_encrypted || model.is_dir) {
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
//            String url = Utils.pathJoin(SERVER, model.encoded_thumbnail_src);
            String url = convertThumbnailUrl(model.repo_id, model.path);
            String thumbKey = EncryptUtils.encryptMD5ToString(url);

            GlideApp.with(getContext())
                    .load(url)
                    .signature(new ObjectKey(thumbKey))
                    .apply(GlideLoadConfig.getCustomDrawableOptions(model.getIcon()))
                    .into(holder.binding.itemIcon);
        }
    }

    private void onBindForSelectMode(@NonNull StarredViewHolder holder, int i, @Nullable StarredModel model) {
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

        holder.binding.expandableToggleButton.setVisibility(View.GONE);

        if (model.is_dir && !model.deleted && !model.repo_encrypted) {
            holder.binding.itemTitle.setAlpha(1f);
            holder.binding.itemSubtitle.setAlpha(1f);
            holder.binding.itemIcon.setAlpha(1f);

            holder.binding.itemCheckbox.setVisibility(View.VISIBLE);
        } else {
            holder.binding.itemTitle.setAlpha(0.3f);
            holder.binding.itemSubtitle.setAlpha(0.3f);
            holder.binding.itemIcon.setAlpha(0.3f);

            holder.binding.itemCheckbox.setVisibility(View.GONE);
        }

        if (model.is_checked && holder.binding.itemCheckbox.getVisibility() == View.VISIBLE) {
            holder.binding.itemCheckbox.setImageResource(R.drawable.ic_checkbox_checked);
        } else {
            holder.binding.itemCheckbox.setImageResource(R.drawable.ic_checkbox_unchecked);
        }

        //set item_icon
        if (model.deleted || TextUtils.isEmpty(model.encoded_thumbnail_src) || !Utils.isViewableImage(model.obj_name) || model.repo_encrypted || model.is_dir) {
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
//            String url = Utils.pathJoin(SERVER, model.encoded_thumbnail_src);
            String url = convertThumbnailUrl(model.repo_id, model.path);
            String thumbKey = EncryptUtils.encryptMD5ToString(url);

            GlideApp.with(getContext())
                    .load(url)
                    .signature(new ObjectKey(thumbKey))
                    .apply(GlideLoadConfig.getCustomDrawableOptions(model.getIcon()))
                    .into(holder.binding.itemIcon);
        }
    }

    public int getSingleSelectedItemPosition() {
        List<StarredModel> list = getItems();
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).is_checked) {
                return i;
            }
        }
        return -1;
    }

    private String convertThumbnailUrl(String repoId, String filePath) {
        return String.format(Locale.ROOT, "%sapi2/repos/%s/thumbnail/?p=%s&size=%d", SERVER, repoId, filePath, 128);
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
                        && oldModel.is_dir == newModel.is_dir;
            }
        });

        setItems(list);
        diffResult.dispatchUpdatesTo(this);
    }
}