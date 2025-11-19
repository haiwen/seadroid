package com.seafile.seadroid2.ui.dialog_fragment.related_users;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemRelatedUserBinding;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class RelatedUsersAdapter extends BaseAdapter<UserModel, RelatedUsersAdapter.Holder> {

    @NonNull
    @Override
    protected Holder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemRelatedUserBinding binding = ItemRelatedUserBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new Holder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull Holder holder, int i, @Nullable UserModel model) {
        if (model == null) {
            return;
        }

        if (TextUtils.isEmpty(model.getAvatarUrl())) {
            holder.binding.listItemAccountIcon.setImageResource(R.drawable.default_avatar);
        } else {
            GlideApp.with(getContext())
                    .load(model.getAvatarUrl())
                    .apply(GlideLoadConfig.getAvatarOptions())
                    .into(holder.binding.listItemAccountIcon);
        }
        holder.binding.listItemAccountTitle.setText(model.getName());
    }


    public static class Holder extends BaseViewHolder {
        ItemRelatedUserBinding binding;

        public Holder(@NonNull ItemRelatedUserBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
