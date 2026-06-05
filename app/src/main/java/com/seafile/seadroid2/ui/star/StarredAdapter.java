package com.seafile.seadroid2.ui.star;

import android.content.Context;
import android.os.Bundle;
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
import com.bumptech.glide.signature.ObjectKey;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemStarredBinding;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.framework.util.ThumbnailUtils;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;

import java.util.List;

public class StarredAdapter extends BaseAdapter<StarredModel, StarredViewHolder> {
    private static final String KEY_PAYLOAD_TITLE = "title";
    private static final String KEY_PAYLOAD_SUBTITLE = "subtitle";
    private static final String KEY_PAYLOAD_ICON = "icon";
    private static final String KEY_PAYLOAD_SELECT_MODE = "select_mode";
    private static final String KEY_PAYLOAD_CHECK = "check";

    private String serverUrl;
    private int deletedSubtitleColor;
    private int normalSubtitleColor;

    public void setServerUrl(String url) {
        this.serverUrl = url;
    }

    private boolean isSelectMode = false;

    public void setSelectMode(boolean selectMode) {
        isSelectMode = selectMode;
    }

    @Override
    public void onAttachedToRecyclerView(@NonNull androidx.recyclerview.widget.RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);
        deletedSubtitleColor = ContextCompat.getColor(getContext(), R.color.red);
        normalSubtitleColor = ContextCompat.getColor(getContext(), R.color.fancy_black);
    }

    @NonNull
    @Override
    protected StarredViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemStarredBinding binding = ItemStarredBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new StarredViewHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull StarredViewHolder holder, int i, @Nullable StarredModel model) {
        onBind(holder, model);
    }

    @Override
    protected void onBindViewHolder(@NonNull StarredViewHolder holder, int position, @Nullable StarredModel model, @NonNull List<?> payloads) {
        if (null == model) {
            return;
        }

        if (CollectionUtils.isEmpty(payloads)) {
            onBind(holder, model);
            return;
        }

        Bundle payload = mergePayloads(payloads);
        if (payload.containsKey(KEY_PAYLOAD_TITLE)) {
            holder.binding.itemTitle.setText(model.obj_name);
        }
        if (payload.containsKey(KEY_PAYLOAD_SUBTITLE)) {
            bindSubtitle(holder, model);
        }
        if (payload.containsKey(KEY_PAYLOAD_ICON)) {
            bindIcon(holder, model);
        }
        if (payload.containsKey(KEY_PAYLOAD_SELECT_MODE)) {
            bindSelectMode(holder, model);
        }
        if (payload.containsKey(KEY_PAYLOAD_CHECK)) {
            bindCheckbox(holder, model);
        }
    }

    private void onBind(@NonNull StarredViewHolder holder, @NonNull StarredModel model) {
        holder.binding.itemTitle.setText(model.obj_name);
        bindSubtitle(holder, model);
        bindSelectMode(holder, model);
        bindIcon(holder, model);
    }

    private Bundle mergePayloads(@NonNull List<?> payloads) {
        Bundle merged = new Bundle();
        for (Object payload : payloads) {
            if (payload instanceof Bundle bundle) {
                merged.putAll(bundle);
            }
        }
        return merged;
    }

    private void bindSubtitle(@NonNull StarredViewHolder holder, @NonNull StarredModel model) {
        if (model.deleted) {
            holder.binding.itemSubtitle.setTextColor(deletedSubtitleColor);
            holder.binding.itemSubtitle.setText(R.string.deleted);
        } else {
            holder.binding.itemSubtitle.setTextColor(normalSubtitleColor);
            holder.binding.itemSubtitle.setText(model.getSubtitle());
        }
    }

    private void bindSelectMode(@NonNull StarredViewHolder holder, @NonNull StarredModel model) {
        if (!isSelectMode) {
            holder.binding.itemCheckbox.setVisibility(View.GONE);
            holder.binding.expandableToggleButton.setVisibility(View.VISIBLE);
            holder.binding.itemTitle.setAlpha(1f);
            holder.binding.itemSubtitle.setAlpha(1f);
            holder.binding.itemIcon.setAlpha(1f);
            return;
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

        bindCheckbox(holder, model);
    }

    private void bindCheckbox(@NonNull StarredViewHolder holder, @NonNull StarredModel model) {
        if (model.is_checked && holder.binding.itemCheckbox.getVisibility() == View.VISIBLE) {
            holder.binding.itemCheckbox.setImageResource(R.drawable.ic_checkbox_checked);
        } else {
            holder.binding.itemCheckbox.setImageResource(R.drawable.ic_checkbox_unchecked);
        }
    }

    private void bindIcon(@NonNull StarredViewHolder holder, @NonNull StarredModel model) {
        if (model.deleted || TextUtils.isEmpty(model.encoded_thumbnail_src) || !Utils.isViewableImage(model.obj_name) || model.repo_encrypted || model.is_dir) {
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
            String url = convertThumbnailUrl(model.repo_id, model.path);
            if (TextUtils.isEmpty(url)) {
                GlideApp.with(getContext())
                        .load(model.getIcon())
                        .apply(GlideLoadConfig.getCacheableThumbnailOptions())
                        .into(holder.binding.itemIcon);
                return;
            }

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
        return ThumbnailUtils.convertThumbnailUrl(serverUrl, repoId, filePath);
    }

    public void notifySelectModeChanged(boolean selectMode) {
        isSelectMode = selectMode;
        Bundle payload = new Bundle();
        payload.putBoolean(KEY_PAYLOAD_SELECT_MODE, true);
        notifyItemRangeChanged(0, getItemCount(), payload);
    }

    public void clearSelectedItems() {
        for (StarredModel item : getItems()) {
            item.is_checked = false;
        }

        Bundle payload = new Bundle();
        payload.putBoolean(KEY_PAYLOAD_CHECK, true);
        notifyItemRangeChanged(0, getItemCount(), payload);
    }

    public void setItemChecked(int position, boolean checked) {
        StarredModel model = getItems().get(position);
        model.is_checked = checked;

        Bundle payload = new Bundle();
        payload.putBoolean(KEY_PAYLOAD_CHECK, true);
        notifyItemChanged(position, payload);
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
                        && oldModel.deleted == newModel.deleted
                        && oldModel.is_checked == newModel.is_checked
                        && TextUtils.equals(oldModel.encoded_thumbnail_src, newModel.encoded_thumbnail_src);
            }

            @Nullable
            @Override
            public Object getChangePayload(int oldItemPosition, int newItemPosition) {
                StarredModel oldModel = getItems().get(oldItemPosition);
                StarredModel newModel = list.get(newItemPosition);
                Bundle payload = new Bundle();

                if (!TextUtils.equals(oldModel.obj_name, newModel.obj_name)) {
                    payload.putBoolean(KEY_PAYLOAD_TITLE, true);
                }

                if (oldModel.deleted != newModel.deleted
                        || !TextUtils.equals(oldModel.repo_name, newModel.repo_name)
                        || !TextUtils.equals(oldModel.mtime, newModel.mtime)) {
                    payload.putBoolean(KEY_PAYLOAD_SUBTITLE, true);
                }

                if (oldModel.deleted != newModel.deleted
                        || oldModel.repo_encrypted != newModel.repo_encrypted
                        || oldModel.is_dir != newModel.is_dir
                        || !TextUtils.equals(oldModel.obj_name, newModel.obj_name)
                        || !TextUtils.equals(oldModel.repo_id, newModel.repo_id)
                        || !TextUtils.equals(oldModel.path, newModel.path)
                        || !TextUtils.equals(oldModel.encoded_thumbnail_src, newModel.encoded_thumbnail_src)) {
                    payload.putBoolean(KEY_PAYLOAD_ICON, true);
                }

                if (oldModel.deleted != newModel.deleted
                        || oldModel.repo_encrypted != newModel.repo_encrypted
                        || oldModel.is_dir != newModel.is_dir) {
                    payload.putBoolean(KEY_PAYLOAD_SELECT_MODE, true);
                }

                if (oldModel.is_checked != newModel.is_checked) {
                    payload.putBoolean(KEY_PAYLOAD_CHECK, true);
                }

                return payload.isEmpty() ? null : payload;
            }
        });

        setItems(list);
        diffResult.dispatchUpdatesTo(this);
    }
}
