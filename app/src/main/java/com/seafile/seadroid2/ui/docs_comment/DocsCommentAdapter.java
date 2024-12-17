package com.seafile.seadroid2.ui.docs_comment;

import static com.seafile.seadroid2.config.Constants.DP.DP_4;
import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.text.TextUtils;
import android.text.method.LinkMovementMethod;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.DiffUtil;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.Glide;
import com.google.android.flexbox.FlexboxLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemFileCommentBinding;
import com.seafile.seadroid2.databinding.LayoutImageBinding;
import com.seafile.seadroid2.framework.data.model.docs_comment.DocsCommentModel;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.media.image_preview2.OnlyImagePreviewActivity;
import com.seafile.seadroid2.view.rich_edittext.RichEditText;
import com.seafile.seadroid2.widget.SimpleMarkdownParser;
import com.yydcdut.markdown.MarkdownConfiguration;
import com.yydcdut.markdown.MarkdownProcessor;
import com.yydcdut.markdown.MarkdownTextView;
import com.yydcdut.markdown.loader.DefaultLoader;
import com.yydcdut.markdown.syntax.text.TextFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

public class DocsCommentAdapter extends BaseAdapter<DocsCommentModel, DocsCommentViewHolder> {
    public static int SCREEN_WIDTH = ScreenUtils.getScreenWidth();
    public static int WIDTH = SizeUtils.dp2px(120);
    public static int IMAGE_WIDTH = (SCREEN_WIDTH - WIDTH) / 3;

    private MarkdownProcessor processor;

    private MarkdownProcessor getProcessor() {
        if (processor != null) {
            return processor;
        }

        MarkdownConfiguration rxMDConfiguration = new MarkdownConfiguration.Builder(getContext())
                .setRxMDImageLoader(new DefaultLoader(getContext()))//default image loader
                .setDefaultImageSize(100, 100)//default image width & height
                .build();
        processor = new MarkdownProcessor(getContext());
        processor.config(rxMDConfiguration);
        processor.factory(TextFactory.create());
        return processor;
    }

    @Override
    protected void onBindViewHolder(@NonNull DocsCommentViewHolder holder, int i, @Nullable DocsCommentModel model) {
        if (model == null) {
            return;
        }

        //
        Glide.with(holder.binding.commentUserAvatar)
                .load(model.avatar_url)
                .into(holder.binding.commentUserAvatar);

        holder.binding.commentNickName.setText(model.user_name);
        holder.binding.commentTime.setText(model.getCreatedAtFriendlyText());

        if (model.resolved) {
            holder.binding.container.setBackgroundResource(R.color.comment_resolved_color);
            holder.binding.commentResolved.setVisibility(View.VISIBLE);
        } else {
            holder.binding.commentResolved.setVisibility(View.INVISIBLE);
            holder.binding.container.setBackgroundResource(R.color.window_background_color);
        }

        holder.binding.commentContentContainer.removeAllViews();

        addViews(holder, model);
    }


    //![](https://dev.seafile.com/e-Hek4ng6iRbCw7h-bXsc6jA.png)\n是是是\n
    private void addViews(DocsCommentViewHolder holder, DocsCommentModel model) {
        int index = 0;
        for (RichEditText.RichContentModel commentModel : model.commentList) {
//            //0 is text, 1 is image
            if (commentModel.type == 0) {
                appendMovementTextToFlex(holder.binding.commentContentContainer, commentModel.content);
            } else {
                appendImageToFlex(holder.binding.commentContentContainer, model.commentList, commentModel.content, index);
                index++;
            }
        }
    }


    @NonNull
    @Override
    protected DocsCommentViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemFileCommentBinding binding = ItemFileCommentBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new DocsCommentViewHolder(binding);
    }


    public void appendImageToFlex(FlexboxLayout parent, List<RichEditText.RichContentModel> commentList, String url, int position) {
        LayoutImageBinding fileBinding = LayoutImageBinding.inflate(LayoutInflater.from(getContext()));
        fileBinding.uploadImage.setScaleType(ImageView.ScaleType.CENTER_CROP);

        FlexboxLayout.LayoutParams f = new FlexboxLayout.LayoutParams(IMAGE_WIDTH, IMAGE_WIDTH);
        f.setMargins(DP_4, DP_4, DP_4, DP_4);
        fileBinding.getRoot().setLayoutParams(f);

        GlideApp.with(getContext())
                .load(url)
                .apply(GlideLoadConfig.getOptions())
                .into(fileBinding.uploadImage);

        fileBinding.uploadImage.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                OnlyImagePreviewActivity.startThis(getContext(), url);

            }
        });


        parent.addView(fileBinding.getRoot());
    }


    public void appendMovementTextToFlex(FlexboxLayout parent, String text) {

        MarkdownTextView textView = new MarkdownTextView(getContext());
        textView.setPadding(DP_4, DP_8, DP_4, DP_8);
        textView.setText(getProcessor().parse(text));
//        textView.setText(text);
        textView.setMovementMethod(LinkMovementMethod.getInstance());
        textView.setId(android.R.id.text1);
        textView.setTextSize(14);
        textView.setTextColor(ContextCompat.getColor(getContext(), R.color.item_title_color));

        FlexboxLayout.LayoutParams f = new FlexboxLayout.LayoutParams(-1, -2);
        parent.addView(textView, f);
    }

    public void submitData(List<DocsCommentModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            submitList(list);
            return;
        }

        if (getItems().isEmpty()) {
            submitList(list);
        } else {
            DiffUtil.DiffResult result = DiffUtil.calculateDiff(new DiffUtil.Callback() {
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
                    DocsCommentModel newT = getItems().get(oldItemPosition);
                    DocsCommentModel oldT = list.get(newItemPosition);
                    return newT.id == oldT.id;
                }

                @Override
                public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
                    DocsCommentModel newT = getItems().get(oldItemPosition);
                    DocsCommentModel oldT = list.get(newItemPosition);

                    return newT.id == oldT.id
                            && Objects.equals(newT.resolved, oldT.resolved)
                            && Objects.equals(newT.created_at, oldT.created_at)
                            && Objects.equals(newT.updated_at, oldT.updated_at)
                            && TextUtils.equals(newT.avatar_url, oldT.avatar_url)
                            && TextUtils.equals(newT.user_contact_email, oldT.user_contact_email)
                            && TextUtils.equals(newT.user_email, oldT.user_email)
                            && TextUtils.equals(newT.user_name, oldT.user_name)
                            && TextUtils.equals(newT.comment, oldT.comment)
                            && TextUtils.equals(newT.parent_path, oldT.parent_path)
                            && TextUtils.equals(newT.item_name, oldT.item_name);
                }
            });

            setItems(list);
            result.dispatchUpdatesTo(this);
        }
    }
}
