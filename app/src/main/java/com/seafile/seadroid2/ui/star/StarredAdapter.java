package com.seafile.seadroid2.ui.star;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.signature.ObjectKey;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemDirentBinding;
import com.seafile.seadroid2.databinding.ItemStarredBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.ThumbnailUtils;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.repo.vh.DirentViewHolder;
import com.seafile.seadroid2.widget.prefs.background_pref.BackgroundShapeUtils;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

public class StarredAdapter extends BaseMultiAdapter<BaseModel> {
    private Drawable topShapeBackgroundDrawable;
    private Drawable bottomShapeBackgroundDrawable;
    private Drawable allShapeBackgroundDrawable;
    private Drawable noneShapeBackgroundDrawable;
    private Drawable starDrawable;
    private String serverUrl;
    private boolean repoEncrypted = false;

    public void setRepoEncrypted(boolean repoEncrypted) {
        this.repoEncrypted = repoEncrypted;
    }

    public void setServerUrl(String url) {
        this.serverUrl = url;
    }

    private boolean isSelectMode = false;

    public void setSelectMode(boolean selectMode) {
        isSelectMode = selectMode;
    }

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);

        int itemBackColor = ContextCompat.getColor(getContext(), R.color.bar_background_color);

        topShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_TOP, itemBackColor, Constants.DP.DP_8);
        bottomShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_BOTTOM, itemBackColor, Constants.DP.DP_8);
        allShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_ALL, itemBackColor, Constants.DP.DP_8);
        noneShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_NONE, itemBackColor, Constants.DP.DP_8);

        int star_width = SizeUtils.dp2px(12);
        starDrawable = ContextCompat.getDrawable(getContext(), R.drawable.baseline_fav_filled);
        starDrawable.setBounds(0, 0, star_width, star_width);
        starDrawable.setTint(ContextCompat.getColor(getContext(), R.color.light_grey));
    }

    public StarredAdapter() {
        addItemType(AbsLayoutItemType.STARRED, new OnMultiItem<BaseModel, StarredViewHolder>() {
            @NonNull
            @Override
            public StarredViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemStarredBinding binding = ItemStarredBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new StarredViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull StarredViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
                onBindStarred(viewHolder, i, baseModel);
            }
        }).addItemType(AbsLayoutItemType.DIRENT_LIST, new OnMultiItem<BaseModel, DirentViewHolder>() {
            @NonNull
            @Override
            public DirentViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemDirentBinding binding = ItemDirentBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new DirentViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull DirentViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
                onBindDirents(viewHolder, i, baseModel);
            }
        }).onItemViewType(new OnItemViewTypeListener<BaseModel>() {
            @Override
            public int onItemViewType(int i, @NonNull List<? extends BaseModel> list) {
                if (list.get(i) instanceof DirentModel) {
                    return AbsLayoutItemType.DIRENT_LIST;
                }
                return AbsLayoutItemType.STARRED;
            }
        });
    }

    private void onBindStarred(@NonNull StarredViewHolder holder, int i, @Nullable BaseModel baseModel) {
        if (null == baseModel) {
            return;
        }

        StarredModel model = (StarredModel) baseModel;
        holder.binding.itemTitle.setText(model.obj_name);
        if (model.deleted) {
            holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.red));
            holder.binding.itemSubtitle.setText(R.string.deleted);
        } else {
            holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.fancy_black));
            holder.binding.itemSubtitle.setText(model.getSubtitle());
        }

        if (isSelectMode) {
            holder.binding.expandableToggleButton.setVisibility(View.GONE);
        } else {
            holder.binding.expandableToggleButton.setVisibility(View.VISIBLE);
        }

        //set item_icon
        if (model.deleted || !Utils.isViewableImage(model.obj_name) || model.repo_encrypted || model.is_dir) {
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
            String url = convertThumbnailUrl(model.repo_id, model.path);
            String thumbKey = EncryptUtils.encryptMD5ToString(url);

            GlideApp.with(getContext())
                    .load(url)
                    .signature(new ObjectKey(thumbKey))
                    .apply(GlideLoadConfig.getCustomDrawableOptions(model.getIcon()))
                    .into(holder.binding.itemIcon);
        }
    }


    private void onBindDirents(DirentViewHolder holder, int position, @Nullable BaseModel baseModel) {
        if (null == baseModel) {
            return;
        }

        DirentModel model = (DirentModel) baseModel;

        //set background color for item
        if (model.item_position == ItemPositionEnum.START) {
            holder.itemView.setBackground(topShapeBackgroundDrawable);
        } else if (model.item_position == ItemPositionEnum.END) {
            holder.itemView.setBackground(bottomShapeBackgroundDrawable);
        } else if (model.item_position == ItemPositionEnum.ALL) {
            holder.itemView.setBackground(allShapeBackgroundDrawable);
        } else {
            holder.itemView.setBackground(noneShapeBackgroundDrawable);
        }

        //hide divider for bottom item
        if (model.item_position == ItemPositionEnum.END || model.item_position == ItemPositionEnum.ALL) {
            holder.binding.divider.setVisibility(View.GONE);
        } else {
            holder.binding.divider.setVisibility(View.VISIBLE);
        }

        holder.binding.itemTitle.setText(model.name);
        holder.binding.itemSubtitle.setText(model.getSubtitle());

        if (model.isDir() || repoEncrypted || !Utils.availableThumbnail(model.name)) {
            GlideApp.with(getContext())
                    .load(model.getIcon())
                    .apply(GlideLoadConfig.getCacheableThumbnailOptions())
                    .into(holder.binding.itemIcon);
        } else {
            loadImage(model, holder.binding.itemIcon);
        }

        boolean canNotEdit = model.is_freezed || model.is_locked;
        if (canNotEdit) {
            holder.binding.itemIconStatus.setVisibility(View.VISIBLE);
        } else {
            holder.binding.itemIconStatus.setVisibility(View.GONE);
        }

        holder.binding.expandableToggleButton.setVisibility(View.GONE);
        holder.binding.itemMultiSelect.setVisibility(View.GONE);
        holder.binding.itemDownloadStatus.setVisibility(View.GONE);

        holder.binding.itemTitle.setCompoundDrawablePadding(Constants.DP.DP_4);
        holder.binding.itemTitle.setCompoundDrawables(null, null, model.starred ? starDrawable : null, null);

    }

    private String convertThumbnailUrl(String repoId, String filePath) {
        return ThumbnailUtils.convertThumbnailUrl(serverUrl, repoId, filePath);
    }

    private String getServerUrl() {
        return serverUrl;
    }

    private void loadImage(DirentModel direntModel, ImageView imageView) {
        String thumbnailUrl = ThumbnailUtils.convertThumbnailUrl(getServerUrl(), direntModel);
        if (TextUtils.isEmpty(thumbnailUrl)) {
            GlideApp.with(getContext())
                    .load(direntModel.getIcon())
                    .apply(GlideLoadConfig.getCacheableThumbnailOptions())
                    .into(imageView);
            return;
        }

        String thumbKey = EncryptUtils.encryptMD5ToString(thumbnailUrl);
        GlideApp.with(getContext())
                .load(thumbnailUrl)
                .signature(new ObjectKey(thumbKey))
                .apply(GlideLoadConfig.getCustomDrawableOptions(direntModel.getIcon()))
                .into(imageView);
    }
//
//    public void notifyDataChanged(List<StarredModel> list) {
//        if (CollectionUtils.isEmpty(list)) {
//            submitList(list);
//            return;
//        }
//
//        if (CollectionUtils.isEmpty(getItems())) {
//            submitList(list);
//            return;
//        }
//
//        DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffUtil.Callback() {
//            @Override
//            public int getOldListSize() {
//                return getItems().size();
//            }
//
//            @Override
//            public int getNewListSize() {
//                return list.size();
//            }
//
//            @Override
//            public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
//                StarredModel oldModel = getItems().get(oldItemPosition);
//                StarredModel newModel = list.get(newItemPosition);
//                String oldFullPath = oldModel.path + oldModel.obj_name;
//                String newFullPath = newModel.path + newModel.obj_name;
//
//                return TextUtils.equals(oldFullPath, newFullPath);
//            }
//
//            @Override
//            public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
//                StarredModel oldModel = getItems().get(oldItemPosition);
//                StarredModel newModel = list.get(newItemPosition);
//
//
//                return TextUtils.equals(oldModel.repo_id, newModel.repo_id)
//                        && TextUtils.equals(oldModel.repo_name, newModel.repo_name)
//                        && TextUtils.equals(oldModel.mtime, newModel.mtime)
//                        && TextUtils.equals(oldModel.path, newModel.path)
//                        && TextUtils.equals(oldModel.obj_name, newModel.obj_name)
//                        && TextUtils.equals(oldModel.user_email, newModel.user_email)
//                        && TextUtils.equals(oldModel.user_name, newModel.user_name)
//                        && TextUtils.equals(oldModel.user_contact_email, newModel.user_contact_email)
//                        && oldModel.repo_encrypted == newModel.repo_encrypted
//                        && oldModel.is_dir == newModel.is_dir;
//            }
//        });
//
//        setItems(list);
//        diffResult.dispatchUpdatesTo(this);
//    }
}