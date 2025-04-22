package com.seafile.seadroid2.ui.docs_comment;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.bumptech.glide.Glide;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.databinding.ItemUserAvatarBinding;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;

@Todo("uncompleted feature")
public class DocsCommentUserAdapter extends BaseAdapter<UserModel, DocsCommentUserViewHolder> {
    @Override
    protected void onBindViewHolder(@NonNull DocsCommentUserViewHolder holder, int i, @Nullable UserModel model) {

        if (i == 0) {
            setMargins(holder.binding.itemUserContainer, 0, 0, 0, 0);
        }

        if (model == null || TextUtils.isEmpty(model.getAvatarUrl())) {
            //
            Glide.with(holder.binding.imageView)
                    .load(R.drawable.default_avatar)
                    .into(holder.binding.imageView);
        } else {
            //
            Glide.with(holder.binding.imageView)
                    .load(model.getAvatarUrl())
                    .into(holder.binding.imageView);
        }

    }

    @NonNull
    @Override
    protected DocsCommentUserViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemUserAvatarBinding binding = ItemUserAvatarBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new DocsCommentUserViewHolder(binding);
    }

    public void setMargins(View v, int l, int t, int r, int b) {
        if (v.getLayoutParams() instanceof ViewGroup.MarginLayoutParams) {
            ViewGroup.MarginLayoutParams p = (ViewGroup.MarginLayoutParams) v.getLayoutParams();
            p.setMargins(l, t, r, b);
            v.requestLayout();
        }
    }
}
