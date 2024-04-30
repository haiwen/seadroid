//package com.seafile.seadroid2.ui.markdown;
//
//import android.content.ActivityNotFoundException;
//import android.content.Context;
//import android.content.Intent;
//import android.content.pm.PackageManager;
//import android.graphics.Color;
//import android.graphics.drawable.Drawable;
//import android.net.Uri;
//import android.os.Bundle;
//import android.text.TextUtils;
//import android.view.Menu;
//import android.view.MenuItem;
//import android.view.View;
//
//import androidx.annotation.NonNull;
//import androidx.annotation.Nullable;
//import androidx.appcompat.widget.Toolbar;
//import androidx.core.content.ContextCompat;
//import androidx.core.content.FileProvider;
//
//import com.blankj.utilcode.util.SizeUtils;
//import com.blankj.utilcode.util.ToastUtils;
//import com.bumptech.glide.Glide;
//import com.bumptech.glide.RequestBuilder;
//import com.bumptech.glide.request.target.Target;
//import com.seafile.seadroid2.BuildConfig;
//import com.seafile.seadroid2.R;
//import com.seafile.seadroid2.config.GlideLoadConfig;
//import com.seafile.seadroid2.databinding.ActivityMarkwonBinding;
//import com.seafile.seadroid2.framework.util.FileMimeUtils;
//import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
//import com.seafile.seadroid2.ui.editor.EditorActivity;
//import com.seafile.seadroid2.ui.editor.EditorViewModel;
//
//import org.commonmark.node.Image;
//import org.commonmark.node.Link;
//
//import java.io.File;
//import java.util.Locale;
//
//import io.noties.markwon.AbstractMarkwonPlugin;
//import io.noties.markwon.LinkResolver;
//import io.noties.markwon.Markwon;
//import io.noties.markwon.MarkwonPlugin;
//import io.noties.markwon.MarkwonSpansFactory;
//import io.noties.markwon.core.CorePlugin;
//import io.noties.markwon.core.CoreProps;
//import io.noties.markwon.core.MarkwonTheme;
//import io.noties.markwon.core.spans.LinkSpan;
//import io.noties.markwon.ext.latex.JLatexMathPlugin;
//import io.noties.markwon.ext.strikethrough.StrikethroughPlugin;
//import io.noties.markwon.ext.tables.TablePlugin;
//import io.noties.markwon.ext.tables.TableTheme;
//import io.noties.markwon.ext.tasklist.TaskListDrawable;
//import io.noties.markwon.ext.tasklist.TaskListPlugin;
//import io.noties.markwon.html.HtmlPlugin;
//import io.noties.markwon.image.AsyncDrawable;
//import io.noties.markwon.image.ImageProps;
//import io.noties.markwon.image.glide.GlideImagesPlugin;
//import io.noties.markwon.inlineparser.MarkwonInlineParserPlugin;
//import io.noties.markwon.linkify.LinkifyPlugin;
//import io.reactivex.functions.Consumer;
//
//public class MarkwonActivity extends BaseActivityWithVM<EditorViewModel> implements Toolbar.OnMenuItemClickListener {
//
//    ActivityMarkwonBinding binding;
//    private String path;
//    private Markwon markwon;
//
//    public static void start(Context context, String localPath) {
//        Intent starter = new Intent(context, MarkwonActivity.class);
//        starter.putExtra("path", localPath);
//        context.startActivity(starter);
//    }
//
//    @Override
//    protected void onCreate(Bundle savedInstanceState) {
//        super.onCreate(savedInstanceState);
//
//        binding = ActivityMarkwonBinding.inflate(getLayoutInflater());
//        setContentView(binding.getRoot());
//
//        Intent intent = getIntent();
//        path = intent.getStringExtra("path");
//
//        if (TextUtils.isEmpty(path)) {
//            throw new IllegalArgumentException("miss path");
//        }
//
//        Toolbar toolbar = getActionBarToolbar();
//        toolbar.setOnMenuItemClickListener(this);
//        setSupportActionBar(toolbar);
//        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
//    }
//
//    @Override
//    protected void onPostCreate(@Nullable Bundle savedInstanceState) {
//        super.onPostCreate(savedInstanceState);
//
//        initMarkwon();
//    }
//
//    private void initMarkwon() {
//        getMarkwon();
//    }
//
//    @Override
//    protected void onResume() {
//        super.onResume();
//
//        loadMarkContent();
//    }
//
//    @Override
//    public boolean onCreateOptionsMenu(Menu menu) {
//        getActionBarToolbar().inflateMenu(R.menu.markdown_view_menu);
//        return true;
//    }
//
//    @Override
//    public boolean onOptionsItemSelected(MenuItem item) {
//        switch (item.getItemId()) {
//            case android.R.id.home:
//                finish();
//                break;
//            case R.id.edit_markdown:
//                edit();
//                break;
//        }
//        return super.onOptionsItemSelected(item);
//    }
//
//    @Override
//    public boolean onMenuItemClick(MenuItem item) {
//        return super.onOptionsItemSelected(item);
//    }
//
//    private void loadMarkContent() {
//        getViewModel().read(path, new Consumer<String>() {
//            @Override
//            public void accept(String s) throws Exception {
//                markwon.setMarkdown(binding.markwonView, s);
//            }
//        });
//    }
//
//    private void edit() {
//        PackageManager pm = getPackageManager();
//
//        // First try to find an activity who can handle markdown edit
//        Intent editAsMarkDown = new Intent(Intent.ACTION_EDIT);
//        Uri uri = FileProvider.getUriForFile(this, BuildConfig.FILE_PROVIDER_AUTHORITIES, new File(path));
//        editAsMarkDown.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
//
//        String mime = FileMimeUtils.getMimeType(new File(path));
//        editAsMarkDown.setDataAndType(uri, mime);
//
//        if ("text/plain".equals(mime)) {
//            Intent intent = new Intent(this, EditorActivity.class);
//            intent.putExtra("path", path);
//            startActivity(intent);
//        } else if (pm.queryIntentActivities(editAsMarkDown, 0).size() > 0) {
//            // Some activity can edit markdown
//            startActivity(editAsMarkDown);
//        } else {
//            // No activity to handle markdown, take it as text
//            Intent editAsText = new Intent(Intent.ACTION_EDIT);
//            mime = "text/plain";
//            editAsText.setDataAndType(uri, mime);
//
//            try {
//                startActivity(editAsText);
//            } catch (ActivityNotFoundException e) {
//                ToastUtils.showLong(R.string.activity_not_found);
//            }
//        }
//    }
//
//    public Markwon getMarkwon() {
//        if (markwon == null) {
//            markwon = getRenderedMarkdownText(this);
//        }
//        return markwon;
//    }
//
//    /**
//     * get markdown text
//     */
//    protected Markwon getRenderedMarkdownText(Context context) {
//        final TaskListDrawable drawable = new TaskListDrawable(Color.GRAY, Color.GRAY, Color.WHITE);
//
//        final TableTheme tableTheme = new TableTheme.Builder()
//                .tableBorderColor(Color.GRAY)
//                .tableBorderWidth(1)
//                .tableCellPadding(2)
//                .tableHeaderRowBackgroundColor(Color.GRAY)
//                .tableEvenRowBackgroundColor(Color.LTGRAY)
//                .tableOddRowBackgroundColor(Color.WHITE)
//                .build();
//
//        final MarkwonPlugin glidePlugin = GlideImagesPlugin.create(new GlideImagesPlugin.GlideStore() {
//            @NonNull
//            @Override
//            public RequestBuilder<Drawable> load(@NonNull AsyncDrawable drawable) {
//                final Drawable placeholder = ContextCompat.getDrawable(context, R.drawable.loading_anim);
//                placeholder.setBounds(0, 0, 100, 100);
//                final Drawable errorPlaceholder = ContextCompat.getDrawable(context, R.drawable.icon_image_error_filled);
//                errorPlaceholder.setBounds(0, 0, 100, 100);
//                return Glide.with(MarkwonActivity.this)
//                        .load(GlideLoadConfig.getGlideUrl(drawable.getDestination()))
//                        .placeholder(placeholder)
//                        .error(errorPlaceholder);
//            }
//
//            @Override
//            public void cancel(@NonNull Target<?> target) {
//                Glide.with(MarkwonActivity.this).clear(target);
//            }
//        });
//
//        return Markwon.builder(context)
//                .usePlugin(CorePlugin.create())
//                .usePlugin(TablePlugin.create(context))
//                .usePlugin(TaskListPlugin.create(drawable))
//                .usePlugin(StrikethroughPlugin.create())
//                .usePlugin(HtmlPlugin.create())
//                .usePlugin(LinkifyPlugin.create())
//                .usePlugin(MarkwonInlineParserPlugin.create())
//                .usePlugin(glidePlugin)
//                .usePlugin(JLatexMathPlugin.create(SizeUtils.px2sp(16), new JLatexMathPlugin.BuilderConfigure() {
//                    @Override
//                    public void configureBuilder(@NonNull JLatexMathPlugin.Builder builder) {
//                        builder.inlinesEnabled(true);
//                    }
//                }))
//                .usePlugin(new AbstractMarkwonPlugin() {
//                    @Override
//                    public void configureSpansFactory(@NonNull MarkwonSpansFactory.Builder builder) {
//                        super.configureSpansFactory(builder);
//                        builder.setFactory(Link.class, (configuration, props) ->
//                                // create a subclass of markwon LinkSpan
//                                new ClickSelfSpan(
//                                        configuration.theme(),
//                                        CoreProps.LINK_DESTINATION.require(props),
//                                        configuration.linkResolver()
//                                )
//                        );
//
//                        builder.appendFactory(Image.class, (configuration, props) -> {
//                            String url = ImageProps.DESTINATION.require(props);
//                            return new LinkSpan(
//                                    configuration.theme(),
//                                    url,
//                                    new ImageLinkResolver(context, configuration.linkResolver()));
//                        });
//                    }
//                })
//                .build();
//    }
//
//    public static class ImageLinkResolver implements LinkResolver {
//        Context context;
//        LinkResolver original;
//
//        public ImageLinkResolver(Context context, LinkResolver original) {
//            this.context = context;
//            this.original = original;
//        }
//
//        @Override
//        public void resolve(@NonNull View view, @NonNull String link) {
//            ToastUtils.showLong("ImageLinkResolver打开此链接：" + link);
//        }
//    }
//
//    public class ClickSelfSpan extends LinkSpan {
//        public ClickSelfSpan(
//                @NonNull MarkwonTheme theme,
//                @NonNull String link,
//                @NonNull LinkResolver resolver) {
//            super(theme, link, resolver);
//        }
//
//        @Override
//        public void onClick(View widget) {
//            String link = getLink();
//            if (TextUtils.isEmpty(link) || !link.toLowerCase(Locale.getDefault()).startsWith("http")) {
//                ToastUtils.showLong("此链接不支持：" + link);
//                return;
//            }
//
//            ToastUtils.showLong("ClickSelfSpan打开此链接：" + link);
//        }
//    }
//
//}
