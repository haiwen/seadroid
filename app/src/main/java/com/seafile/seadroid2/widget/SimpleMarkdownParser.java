package com.seafile.seadroid2.widget;

import android.graphics.Typeface;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.text.Spannable;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.BulletSpan;
import android.text.style.ImageSpan;
import android.text.style.StyleSpan;
import android.text.style.URLSpan;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.bumptech.glide.Glide;
import com.bumptech.glide.request.target.CustomTarget;
import com.bumptech.glide.request.transition.Transition;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SimpleMarkdownParser {
    public static void setMarkdown(TextView textView, String markdown) {
        SpannableStringBuilder spannable = parseMarkdown(textView, markdown);
        textView.setText(spannable);
    }

    private static SpannableStringBuilder parseMarkdown(TextView textView, String markdown) {
        SpannableStringBuilder builder = new SpannableStringBuilder();

        // 1. Parse bold italic text: ***rich text***
        applyPattern(builder, "\\*\\*\\*(.*?)\\*\\*\\*", match -> {
            builder.setSpan(new StyleSpan(Typeface.BOLD_ITALIC),
                    match.start(1), match.end(1),
                    Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        });

        // 2. Parse block quotes: > ***quote***
        applyPattern(builder, "^>\\s?(.*)", match -> {
            builder.setSpan(new StyleSpan(Typeface.ITALIC),
                    match.start(1), match.end(1),
                    Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        });

        // 3. Parse unordered lists: * 1
        applyPattern(builder, "^\\*\\s(.*)", match -> {
            builder.setSpan(new BulletSpan(20),
                    match.start(1), match.end(1),
                    Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        });

        // 4. Parse ordered lists: 1. 1
        applyPattern(builder, "^\\d+\\.\\s(.*)", match -> {
            builder.setSpan(new StyleSpan(Typeface.BOLD),
                    match.start(1), match.end(1),
                    Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        });

        // 5. Parse links: [https://xxx](xxx)
        applyPattern(builder, "\\[(.*?)\\]\\((.*?)\\)", match -> {
            builder.setSpan(new URLSpan(match.group(2)),
                    match.start(1), match.end(1),
                    Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
            builder.replace(match.start(2), match.end(2), ""); // Remove the URL portion
        });

        // 6. Parse images: ![1.jpg](https://xxx)
        applyPattern(builder, "!\\[(.*?)\\]\\((.*?)\\)", match -> {
            String imageUrl = match.group(2);
            builder.replace(match.start(), match.end(), match.group(1)); // Replace with placeholder text
            int start = match.start();
            int end = start + match.group(1).length();

            // Load the image asynchronously
            loadImageAsync(textView, imageUrl, drawable -> {
                drawable.setBounds(0, 0, drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight());
                ImageSpan imageSpan = new ImageSpan(drawable, ImageSpan.ALIGN_BOTTOM);
                builder.setSpan(imageSpan, start, end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
                textView.setText(builder); // Update the TextView
            });
        });
        return builder;
    }

    private static void applyPattern(SpannableStringBuilder stringBuilder, String regex, MatchHandler handler) {
        Pattern pattern = Pattern.compile(regex, Pattern.MULTILINE);
        Matcher matcher = pattern.matcher(stringBuilder);
        while (matcher.find()) {
            handler.handleMatch(matcher);
        }
    }

    private static void loadImageAsync(TextView textView, String imageUrl, ImageLoaderCallback callback) {

//        Glide.with(textView.getContext())
//                .load(imageUrl)
//                .into(new CustomTarget<Drawable>() {
//                    @Override
//                    public void onResourceReady(@NonNull Drawable resource, Transition<? super Drawable> transition) {
//                        // Set image dimensions to fit the TextView
//                        int maxWidth = textView.getWidth() - textView.getPaddingLeft() - textView.getPaddingRight();
//                        int width = Math.min(resource.getIntrinsicWidth(), maxWidth);
//                        int height = width * resource.getIntrinsicHeight() / resource.getIntrinsicWidth();
//                        resource.setBounds(0, 0, width, height);
//
//                        // Replace text with the image
//                        ImageSpan imageSpan = new ImageSpan(resource, ImageSpan.ALIGN_BOTTOM);
//                        builder.setSpan(imageSpan, start, end, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
//
//                        // Update the TextView content
//                        textView.setText(builder);
//                    }
//
//                    @Override
//                    public void onLoadCleared(Drawable placeholder) {
//                        // Optional: handle image loading cancellation
//                    }
//                });
    }

    interface MatchHandler {
        void handleMatch(Matcher match);
    }

    interface ImageLoaderCallback {
        void onImageLoaded(@NonNull Drawable drawable);
    }
}
