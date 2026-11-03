package com.seafile.seadroid2.ui.previewer.pdf;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.ActionBar;

import com.github.barteksc.pdfviewer.listener.OnErrorListener;
import com.github.barteksc.pdfviewer.listener.OnLoadCompleteListener;
import com.github.barteksc.pdfviewer.listener.OnPageChangeListener;
import com.github.barteksc.pdfviewer.scroll.DefaultScrollHandle;
import com.github.barteksc.pdfviewer.util.FitPolicy;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.ActivityPdfPreviewBinding;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivity;

import java.io.File;

/**
 * Preview a local PDF file.
 */
public class PdfPreviewActivity extends BaseActivity {
    private static final String TAG = "PdfPreviewActivity";

    public final static String EXTRA_PDF_PATH = "EXTRA_PDF_PATH";
    private static final String EXTRA_PAGE = "EXTRA_PAGE";

    private ActivityPdfPreviewBinding binding;

    private String pdfPath;
    private int currentPage = 0;

    public static void start(Context context, String localFile) {
        Intent intent = new Intent(context, PdfPreviewActivity.class);
        intent.putExtra(EXTRA_PDF_PATH, localFile);
        context.startActivity(intent);
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityPdfPreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        initToolbar();
        applyEdgeToEdge(binding.getRoot());

        if (savedInstanceState != null) {
            pdfPath = savedInstanceState.getString(EXTRA_PDF_PATH);
            currentPage = savedInstanceState.getInt(EXTRA_PAGE, 0);
        } else {
            pdfPath = getIntent().getStringExtra(EXTRA_PDF_PATH);
        }

        getActionBarToolbar().setNavigationOnClickListener(v -> getOnBackPressedDispatcher().onBackPressed());

        if (TextUtils.isEmpty(pdfPath)) {
            Toasts.show(R.string.not_found);
            finish();
            return;
        }

        File file = new File(pdfPath);
        if (!file.exists()) {
            Toasts.show(getString(R.string.file_not_found, file.getName()));
            finish();
            return;
        }

        ActionBar actionBar = getSupportActionBar();
        if (actionBar != null) {
            actionBar.setTitle(file.getName());
        }

        loadPdf(file);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString(EXTRA_PDF_PATH, pdfPath);
        outState.putInt(EXTRA_PAGE, currentPage);
    }

    private void loadPdf(File file) {
        binding.progressBar.setVisibility(View.VISIBLE);

        binding.pdfView.fromFile(file)
                .defaultPage(currentPage)
                .pageFitPolicy(FitPolicy.WIDTH)
                .swipeHorizontal(false)
                .enableAntialiasing(true)
                .enableAnnotationRendering(true)
                .scrollHandle(new DefaultScrollHandle(this))
                .onLoad(new OnLoadCompleteListener() {
                    @Override
                    public void loadComplete(int nbPages) {
                        binding.progressBar.setVisibility(View.GONE);
                    }
                })
                .onPageChange(new OnPageChangeListener() {
                    @Override
                    public void onPageChanged(int page, int pageCount) {
                        currentPage = page;
                    }
                })
                .onError(new OnErrorListener() {
                    @Override
                    public void onError(Throwable t) {
                        binding.progressBar.setVisibility(View.GONE);
                        SLogs.e(TAG, "Failed to load pdf: " + t.getMessage());
                        Toasts.show(R.string.error_image_load);
                    }
                })
                .load();
    }
}
