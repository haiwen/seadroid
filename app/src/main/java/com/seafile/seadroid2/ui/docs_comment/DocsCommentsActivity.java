package com.seafile.seadroid2.ui.docs_comment;

import android.content.ContentResolver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.PopupMenu;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.KeyboardUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.databinding.ActivityDocCommentBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarBinding;
import com.seafile.seadroid2.framework.model.docs_comment.DocsCommentModel;
import com.seafile.seadroid2.framework.model.docs_comment.DocsCommentsWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseMediaSelectorActivity;
import com.seafile.seadroid2.view.rich_edittext.RichEditText;

import java.util.List;

import io.reactivex.functions.Consumer;

public class DocsCommentsActivity extends BaseMediaSelectorActivity<DocsCommentViewModel> {
    private ActivityDocCommentBinding binding;
    private ToolbarActionbarBinding bindingOfToolbar;

    private DocsCommentAdapter adapter;
    private DocsCommentUserAdapter userAdapter;

    private SDocPageOptionsModel pageOptionsModel;

    public static void start(Context context, SDocPageOptionsModel pageModel) {
        Intent starter = new Intent(context, DocsCommentsActivity.class);
        starter.putExtra("pageOption", pageModel);
        context.startActivity(starter);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putParcelable("pageOption", pageOptionsModel);
        Parcelable listParcelable = linearLayoutManager.onSaveInstanceState();
        outState.putParcelable("listParcelable", listParcelable);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityDocCommentBinding.inflate(getLayoutInflater());
        bindingOfToolbar = ToolbarActionbarBinding.bind(binding.toolbar.getRoot());

        getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE);
        setContentView(binding.getRoot());

        initView();

        initViewModel();

        initAdapter();


        if (savedInstanceState != null) {
            pageOptionsModel = savedInstanceState.getParcelable("pageOption");
            if (pageOptionsModel == null) {
                throw new IllegalArgumentException("pageOption is null");
            }
            bindingOfToolbar.toolbarActionbar.setTitle(pageOptionsModel.docName);

            Parcelable listParcelable = savedInstanceState.getParcelable("listParcelable");
            if (listParcelable!= null) {
                linearLayoutManager.onRestoreInstanceState(listParcelable);
            }
        } else {
            if (getIntent() == null || !getIntent().hasExtra("pageOption")) {
                throw new IllegalArgumentException("pageOption is null");
            }

            pageOptionsModel = getIntent().getParcelableExtra("pageOption");
            if (pageOptionsModel == null) {
                throw new IllegalArgumentException("pageOption is null");
            }
            bindingOfToolbar.toolbarActionbar.setTitle(pageOptionsModel.docName);
        }

        refreshData();
    }


    private final LinearLayoutManager linearLayoutManager = new LinearLayoutManager(this);

    private void initView() {
        Toolbar toolbar = bindingOfToolbar.toolbarActionbar;

        toolbar.setTitle("");
        setSupportActionBar(toolbar);

        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

        //refresh listener
        binding.swipeRefreshLayout.setOnRefreshListener(this::refreshData);

        binding.rv.setLayoutManager(linearLayoutManager);

        binding.photoView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showPickPhotoSheetDialog(false);
            }
        });

//        //
//        LinearLayoutManager linearLayoutManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
//        binding.rvUserList.setLayoutManager(linearLayoutManager);

        binding.submit.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                submitData();
            }
        });
//
//        binding.richEditText.setOnRichAtListener(new OnRichAtListener() {
//            @Override
//            public void onCall(EditText editText) {
//                showCollaboratorSelector(editText);
//            }
//        });
    }


    protected void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getPostCommentLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean model) {
                //remove all
                binding.richEditText.removeAllViews();

                refreshData();
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                Toasts.show(e.getMessage());
            }
        });

//        getViewModel().getUserListLiveData().observe(this, new Observer<List<RelatedUserModel>>() {
//            @Override
//            public void onChanged(List<RelatedUserModel> relatedUserModels) {
//                userAdapter.submitList(relatedUserModels);
//            }
//        });
        getViewModel().getSdocCommentLiveData().observe(this, new Observer<DocsCommentsWrapperModel>() {
            @Override
            public void onChanged(DocsCommentsWrapperModel model) {
                adapter.setStateViewEnable(true);
                adapter.submitData(model.comments);

                linearLayoutManager.smoothScrollToPosition(binding.rv, null, adapter.getItemCount() - 1);

            }
        });
    }


    private void initAdapter() {
//        userAdapter = new DocsCommentUserAdapter();
//        userAdapter.setAnimationEnable(true);
//
//        userAdapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<UserModel>() {
//            @Override
//            public void onClick(@NonNull BaseQuickAdapter<UserModel, ?> baseQuickAdapter, @NonNull View view, int i) {
//                if (CollectionUtils.isEmpty(userAdapter.getItems())) {
//                    return;
//                }
//
////                showCollaboratorSelector();
//            }
//        });
//        binding.rvUserList.setAdapter(userAdapter);
//
//        if (CollectionUtils.isEmpty(strategyModel.participants)) {
//            binding.rvUserList.setVisibility(View.GONE);
//        } else {
//            userAdapter.submitList(strategyModel.participants);
//        }

        adapter = new DocsCommentAdapter();
        adapter.setStateViewLayout(this, R.layout.layout_empty);
        adapter.setStateViewEnable(false);
        adapter.setAnimationEnable(true);

        adapter.addOnItemChildClickListener(R.id.comment_more, new BaseQuickAdapter.OnItemChildClickListener<DocsCommentModel>() {
            @Override
            public void onItemClick(@NonNull BaseQuickAdapter<DocsCommentModel, ?> baseQuickAdapter, @NonNull View view, int i) {
//                buildMoreDialog(i);
                initPopupMenu(i, view);
            }
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());
    }

    private void initPopupMenu(int position, View showView) {
        DocsCommentModel model = adapter.getItems().get(position);

        PopupMenu popupMenu = new PopupMenu(this, showView);
        popupMenu.getMenuInflater().inflate(R.menu.menu_comment_mark_delete, popupMenu.getMenu());
        popupMenu.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
            @Override
            public boolean onMenuItemClick(MenuItem item) {
                if (item.getItemId() == R.id.mark_resolve) {
                    getViewModel().markResolve(pageOptionsModel.seadocServerUrl, pageOptionsModel.seadocAccessToken, pageOptionsModel.docUuid, model.id, new Consumer<Long>() {
                        @Override
                        public void accept(Long aLong) throws Exception {
                            model.resolved = true;
                            adapter.set(position, model);
                        }
                    });
                } else if (item.getItemId() == R.id.delete) {
                    showDeleteDialog(position, model.id);
                }
                return false;
            }
        });
        popupMenu.show();
    }

    private void showDeleteDialog(int position, int cId) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
        builder.setTitle(R.string.delete_confirm);
        builder.setPositiveButton(R.string.delete, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                getViewModel().delete(pageOptionsModel.seadocServerUrl, pageOptionsModel.seadocAccessToken, pageOptionsModel.docUuid, cId, new Consumer<Long>() {
                    @Override
                    public void accept(Long aLong) throws Exception {
                        adapter.removeAt(position);
                    }
                });
            }
        });

        builder.setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                dialog.dismiss();
            }
        });
        builder.show();
    }

    private void refreshData() {
        getViewModel().loadDocComments(pageOptionsModel);
    }

    public void onMediaPicked(Uri uri) {
        super.onMediaPicked(uri);
        uploadFile(uri);
    }


    private void uploadFile(Uri o) {
        if (null == o) {
            return;
        }

        SLogs.d(o.toString());

        binding.richEditText.insertImage(o);

        ContentResolver contentResolver = getContentResolver();

        //upload file
        getViewModel().uploadFile(contentResolver, o, pageOptionsModel.docUuid, pageOptionsModel.seadocAccessToken, new Consumer<String>() {
            @Override
            public void accept(String absUrl) {
                binding.richEditText.updateUploadState(o.toString(), absUrl);
            }
        }, new Consumer<String>() {
            @Override
            public void accept(String s) throws Exception {
                Toasts.show(R.string.upload_failed);
            }
        });
    }

    private void submitData() {
        //
        KeyboardUtils.hideSoftInput(getWindow());

        List<RichEditText.RichContentModel> models = binding.richEditText.buildRichEditData();
        if (CollectionUtils.isEmpty(models)) {
            return;
        }

        StringBuilder sb = new StringBuilder();

        models.forEach(f -> {
            if (f.type == 0) {
                sb.append(f.content).append("\n\n");
            } else if (f.type == 1) {
                sb.append("![](").append(f.content).append(")");
            }
        });

        if (TextUtils.isEmpty(sb.toString())) {
            return;
        }

        // 0 is root comment
        getViewModel().postComment(pageOptionsModel, sb.toString(), "0");
    }
}
