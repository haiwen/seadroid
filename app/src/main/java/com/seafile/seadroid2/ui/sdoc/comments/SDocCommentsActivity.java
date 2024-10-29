package com.seafile.seadroid2.ui.sdoc.comments;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.ActivitySdocCommentBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarBinding;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocCommentModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocCommentWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.sdoc.SDocViewModel;

public class SDocCommentsActivity extends BaseActivityWithVM<SDocViewModel> {
    private ActivitySdocCommentBinding binding;
    private ToolbarActionbarBinding bindingOfToolbar;

    private Toolbar toolbar;

    private SDocCommentAdapter adapter;
    private SDocCommentUserAdapter userAdapter;

    private SDocPageOptionsModel pageOptionsModel;

    public static void start(Context context, SDocPageOptionsModel pageModel) {
        Intent starter = new Intent(context, SDocCommentsActivity.class);
        starter.putExtra("pageOption", pageModel);
        context.startActivity(starter);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySdocCommentBinding.inflate(getLayoutInflater());
        bindingOfToolbar = ToolbarActionbarBinding.bind(binding.toolbar.getRoot());

        getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE);

        setContentView(binding.getRoot());

        if (getIntent() == null || !getIntent().hasExtra("pageOption")) {
            throw new IllegalArgumentException("pageOption is null");
        }

        pageOptionsModel = getIntent().getParcelableExtra("pageOption");

        initView();

        initViewModel();

        initAdapter();

        refreshData();
    }

    private void initView() {
        toolbar = bindingOfToolbar.toolbarActionbar;

        toolbar.setTitle("");
        setSupportActionBar(toolbar);
        toolbar.setTitle(pageOptionsModel.docName);

        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

        //refresh listener
        binding.swipeRefreshLayout.setOnRefreshListener(this::refreshData);

        binding.rv.setLayoutManager(new LinearLayoutManager(this));
//        //
//        LinearLayoutManager linearLayoutManager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
//        binding.rvUserList.setLayoutManager(linearLayoutManager);
//        binding.photoView.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                showPickPhotoSheetDialog(true);
//            }
//        });
//
//        binding.submit.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                submitData();
//            }
//        });
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

//        getViewModel().getPostCommentLiveData().observe(this, new Observer<TableRowCommentModel>() {
//            @Override
//            public void onChanged(TableRowCommentModel model) {
//                //remove all
//                binding.richEditText.removeAllViews();
//
//                refreshData(true);
//            }
//        });

//        getViewModel().getUserListLiveData().observe(this, new Observer<List<RelatedUserModel>>() {
//            @Override
//            public void onChanged(List<RelatedUserModel> relatedUserModels) {
//                userAdapter.submitList(relatedUserModels);
//            }
//        });
        getViewModel().getSdocCommentLiveData().observe(this, new Observer<SDocCommentWrapperModel>() {
            @Override
            public void onChanged(SDocCommentWrapperModel sDocCommentWrapperModel) {
                adapter.setStateViewEnable(true);

                adapter.submitData(sDocCommentWrapperModel.comments);
            }
        });
    }


    private void initAdapter() {
//        userAdapter = new SDocCommentUserAdapter();
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

        adapter = new SDocCommentAdapter();
        adapter.setStateViewLayout(this, R.layout.layout_empty);
        adapter.setStateViewEnable(false);
        adapter.setAnimationEnable(true);

        adapter.addOnItemChildClickListener(R.id.comment_more, new BaseQuickAdapter.OnItemChildClickListener<SDocCommentModel>() {
            @Override
            public void onItemClick(@NonNull BaseQuickAdapter<SDocCommentModel, ?> baseQuickAdapter, @NonNull View view, int i) {

            }
        });

        QuickAdapterHelper helper = new QuickAdapterHelper.Builder(adapter).build();
        binding.rv.setAdapter(helper.getAdapter());
    }

    private void refreshData() {
        getViewModel().getSDocComments(pageOptionsModel);
    }
}
