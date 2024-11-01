package com.seafile.seadroid2.ui.sdoc.comments;

import static com.seafile.seadroid2.config.Constants.DP.DP_4;
import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.text.TextUtils;
import android.text.method.LinkMovementMethod;
import android.view.LayoutInflater;
import android.view.ViewGroup;

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
import com.seafile.seadroid2.databinding.ItemSdocCommentBinding;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocCommentModel;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.yydcdut.markdown.MarkdownConfiguration;
import com.yydcdut.markdown.MarkdownProcessor;
import com.yydcdut.markdown.MarkdownTextView;
import com.yydcdut.markdown.loader.DefaultLoader;
import com.yydcdut.markdown.syntax.text.TextFactory;

import java.util.List;
import java.util.Locale;
import java.util.Objects;

public class SDocCommentAdapter extends BaseAdapter<SDocCommentModel, SDocCommentViewHolder> {
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
    protected void onBindViewHolder(@NonNull SDocCommentViewHolder holder, int i, @Nullable SDocCommentModel model) {
        if (model == null) {
            return;
        }

        //
        Glide.with(holder.binding.commentUserAvatar)
                .load(model.avatar_url)
                .into(holder.binding.commentUserAvatar);

        holder.binding.commentNickName.setText(model.user_name);
        holder.binding.commentTime.setText(model.getCreatedAtFriendlyText());

//        if (TextUtils.isEmpty(model.resolved) || "false".equals(model.resolved.toLowerCase(Locale.getDefault()))) {
//            holder.binding.commentContentContainer.setBackgroundResource(R.drawable.shape_stroke1_radius8_solid_grey);
//        } else {
            holder.binding.commentContentContainer.setBackgroundResource(R.drawable.shape_stroke1_radius8_solid_white);
//        }

        holder.binding.commentContentContainer.removeAllViews();
        appendTextToFlex(holder.binding.commentContentContainer, model.comment);

    }


//    //![](https://dev.seafile.com/e-Hek4ng6iRbCw7h-bXsc6jA.png)\n是是是\n
//    private void addViews(SDocCommentModel model, SDocCommentViewHolder holder) {
//        int index = 0;
//        for (RichEditText.RichContentModel commentModel : model.commentList) {
//            //0 is text, 1 is image
//            if (commentModel.type == 0) {
//                appendTextToFlex(holder.binding.commentContentContainer, commentModel.content, model.isContainImage);
//            } else {
//                appendImageToFlex(holder.binding.commentContentContainer, model.commentList, commentModel.content, index);
//                index++;
//            }
//        }
//    }


    @NonNull
    @Override
    protected SDocCommentViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemSdocCommentBinding binding = ItemSdocCommentBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new SDocCommentViewHolder(binding);
    }


//    public void appendImageToFlex(FlexboxLayout parent, List<RichContentModel> commentList, String url, int position) {
//        LayoutImageBinding fileBinding = LayoutImageBinding.inflate(LayoutInflater.from(getContext()));
//        fileBinding.uploadImage.setScaleType(ImageView.ScaleType.CENTER_CROP);
//
//        FlexboxLayout.LayoutParams f = new FlexboxLayout.LayoutParams(IMAGE_WIDTH, IMAGE_WIDTH);
//        f.setMargins(DP_4, DP_4, DP_4, DP_4);
//        fileBinding.getRoot().setLayoutParams(f);
//
//        Glide.with(getContext())
//                .load(GlideLoadConfig.getGlideUrl(url))
//                .apply(GlideLoadConfig.getOptions(IMAGE_WIDTH, IMAGE_WIDTH))
//                .into(fileBinding.uploadImage);
//        fileBinding.uploadImage.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                List<String> ll = commentList.stream().filter(f -> f.type == 1).map(m -> m.content).collect(Collectors.toList());
//                MediaPlayerModel model = new MediaPlayerModel();
//                model.index = position;
//                model.urls = ll;
//                MediaPlayerActivity.startThis(getContext(), model);
//            }
//        });
//
//
//        parent.addView(fileBinding.getRoot());
//    }

    public void appendTextToFlex(FlexboxLayout parent, String comment) {
        MarkdownTextView textView = new MarkdownTextView(getContext());
        textView.setPadding(DP_4, DP_8, DP_4, DP_8);
//        textView.setText(getProcessor().parse(comment));
        textView.setText(comment);
        textView.setMovementMethod(LinkMovementMethod.getInstance());
        textView.setId(android.R.id.text1);
        textView.setTextSize(14);
        textView.setTextColor(ContextCompat.getColor(getContext(), R.color.material_blue_grey_900));

        FlexboxLayout.LayoutParams f = new FlexboxLayout.LayoutParams(-1, -2);
        parent.addView(textView, f);
    }

    public void submitData(List<SDocCommentModel> list) {
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
                    SDocCommentModel newT = getItems().get(oldItemPosition);
                    SDocCommentModel oldT = list.get(newItemPosition);
                    return newT.id == oldT.id;
                }

                @Override
                public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
                    SDocCommentModel newT = getItems().get(oldItemPosition);
                    SDocCommentModel oldT = list.get(newItemPosition);

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
