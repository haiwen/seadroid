package com.seafile.seadroid2.ui.activities;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.SpanUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemActivityBinding;
import com.seafile.seadroid2.databinding.ItemGroupItemBinding;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.util.SystemSwitchUtils;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.viewholder.GroupItemViewHolder;

import java.util.List;

public class ActivityAdapter extends BaseMultiAdapter<BaseModel> {

    int defaultColor;
    int fancyColor;

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);

        defaultColor = ContextCompat.getColor(getContext(), R.color.item_subtitle_color);
        fancyColor = ContextCompat.getColor(getContext(), R.color.fancy_orange);
    }

    public ActivityAdapter() {

        addItemType(AbsLayoutItemType.GROUP_ITEM, new OnMultiItem<BaseModel, RecyclerView.ViewHolder>() {
            @NonNull
            @Override
            public GroupItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemGroupItemBinding binding = ItemGroupItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new GroupItemViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull RecyclerView.ViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {

            }
        }).addItemType(AbsLayoutItemType.ACTIVITY, new OnMultiItem<BaseModel, ActivityViewHolder>() {
            @NonNull
            @Override
            public ActivityViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemActivityBinding binding = ItemActivityBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new ActivityViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull ActivityViewHolder holder, int i, @Nullable BaseModel activityModel) {
                onBindActivity(holder, (ActivityModel) activityModel);
            }
        }).onItemViewType(new OnItemViewTypeListener<BaseModel>() {
            @Override
            public int onItemViewType(int i, @NonNull List<? extends BaseModel> list) {
                if (list.get(i) instanceof ActivityModel) {
                    return AbsLayoutItemType.ACTIVITY;
                } else if (list.get(i) instanceof GroupItemModel) {
                    return AbsLayoutItemType.GROUP_ITEM;
                }
                return AbsLayoutItemType.NOT_SUPPORTED;
            }
        });
    }

    private void onBindActivity(ActivityViewHolder holder, ActivityModel model) {
        //
        holder.binding.itemNickName.setText(model.author_name);
        holder.binding.itemTime.setText(model.getTime());

        // item desc
        String desc = SystemSwitchUtils.obj_type(getContext(), model.obj_type, model.op_type, model.count);
        holder.binding.itemDesc.setText(desc);

        // load avatar
        GlideApp.with(getContext())
                .load(model.avatar_url)
                .apply(GlideLoadConfig.getAvatarOptions())
                .into(holder.binding.itemAvatar);


        SpanUtils spanUtils = SpanUtils.with(holder.binding.itemDetail);

        //
        if (model.obj_type.equals("repo")) {
            spanUtils.append(model.repo_name);
            holder.binding.itemRepoName.setText("");
        } else {
            holder.binding.itemRepoName.setText(model.repo_name);
        }


        if (model.op_type.equals("rename")) {
            spanUtils.append(model.old_name);
            spanUtils.append(" => ");
            spanUtils.append(model.name);
            spanUtils.setForegroundColor(fancyColor);
        } else if (CollectionUtils.isEmpty(model.details)) {// old version field
            if (model.op_type.equals("delete")) {
                spanUtils.append(model.name);
                spanUtils.setForegroundColor(defaultColor);
            } else {
                // create/update/rename/...
                spanUtils.append(model.name);
                spanUtils.setForegroundColor(fancyColor);
            }
        } else if (model.details.size() == 1) {// new version fields
            spanUtils.append(model.name);

            if (model.op_type.equals("delete")) {
                spanUtils.setForegroundColor(defaultColor);
            } else {
                // create/update/edit
                spanUtils.setForegroundColor(fancyColor);
            }
        } else {
            // batch

            String otherStr;
            int c = model.count - 1;
            if (c < 1) {
                c = 1;
            }
            if (model.obj_type.equals("dir")) {
                otherStr = getContext().getString(R.string.and_other_folders, c);
            } else {
                otherStr = getContext().getString(R.string.and_other_files, c);
            }

            int detailColor;
            if (model.op_type.equals("delete") || model.op_type.equals("batch_delete")) {
                detailColor = ContextCompat.getColor(getContext(), R.color.item_subtitle_color);
            } else {
                detailColor = ContextCompat.getColor(getContext(), R.color.fancy_orange);
            }

            spanUtils.append(model.name);
            spanUtils.setForegroundColor(detailColor);
            spanUtils.append(" ");
            spanUtils.append(otherStr);
            spanUtils.setForegroundColor(defaultColor);
        }

        spanUtils.create();

    }


//    switch (event.op_type) {
//        case "create":
//            break;
//        case "edit":
//            break;
//        case "rename":
//            break;
//        case "delete":
//            break;
//        case "recover":
//        case "restore":
//            event.opType = OpType.RESTORE;
//            break;
//        case "move":
//            break;
//        case "update":
//            break;
//        case "public":
//        case "publish":
//            break;
//        case "batch_create":
//            break;
//        case "batch_delete":
//            break;
//    }
}
