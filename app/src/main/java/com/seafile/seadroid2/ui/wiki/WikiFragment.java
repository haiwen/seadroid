package com.seafile.seadroid2.ui.wiki;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.GridLayoutManager;

import com.chad.library.adapter4.BaseQuickAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.WikiType;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.wiki.WikiGroupModel;
import com.seafile.seadroid2.framework.model.wiki.WikiInfoModel;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.bottomsheetmenu.BottomSheetHelper;
import com.seafile.seadroid2.ui.bottomsheetmenu.OnMenuClickListener;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.dialog_fragment.wiki.BottomSheetPublishWikiDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.wiki.BottomSheetRenameWikiDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.wiki.DeleteWikiDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.wiki.UnpublishWikiDialogFragment;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;

import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class WikiFragment extends BaseFragmentWithVM<WikiViewModel> {
    private LayoutFrameSwipeRvBinding binding;
    private WikiAdapter adapter;

    public static WikiFragment newInstance() {

        Bundle args = new Bundle();

        WikiFragment fragment = new WikiFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initLiveData();

        init();

        loadData();
    }

    private void init() {
        adapter = new WikiAdapter();
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<BaseModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<BaseModel, ?> baseQuickAdapter, @NonNull View view, int i) {

                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return;
                }

                BaseModel model = adapter.getItems().get(i);
                WikiInfoModel wikiInfoModel = (WikiInfoModel) model;
                String url;
                if (StringUtils.equalsIgnoreCase(wikiInfoModel.type, WikiType.TYPE_OLD)) {
                    url = Utils.pathJoin(account.getServer(), "published", wikiInfoModel.slug);
                }else {
                    url = Utils.pathJoin(account.getServer(), "wikis", wikiInfoModel.id);
                }

                SeaWebViewActivity.openUrl(requireContext(), url, true);
            }
        });

        adapter.addOnItemChildClickListener(R.id.item_wiki_more, new BaseQuickAdapter.OnItemChildClickListener<BaseModel>() {
            @Override
            public void onItemClick(@NonNull BaseQuickAdapter<BaseModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                showBottomDialog((WikiInfoModel) adapter.getItems().get(i));
            }
        });

        binding.rv.setAdapter(adapter);

        GridLayoutManager manager = new GridLayoutManager(requireContext(), 2);
        manager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(int position) {
                BaseModel model = adapter.getItems().get(position);
                if (model instanceof WikiGroupModel) {
                    return 2;
                }
                return 1;
            }
        });

        binding.rv.setLayoutManager(manager);
        binding.rv.setClipToPadding(false);
        binding.rv.setPadding(Constants.DP.DP_8, Constants.DP.DP_8, Constants.DP.DP_8, Constants.DP.DP_8);
    }

    private void initLiveData() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);

            }
        });
        getViewModel().getWikisLiveData().observe(getViewLifecycleOwner(), new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> baseModels) {
                adapter.submitList(baseModels);
            }
        });
    }

    private void loadData() {
        getViewModel().loadWikis();
    }


    private void showBottomDialog(WikiInfoModel model) {

        int removeId;
        if (model.is_published) {
            removeId = R.id.action_publish;
        } else {
            removeId = R.id.action_unpublish;
        }
        BottomSheetHelper.buildSheet(requireActivity(), R.menu.bottom_wiki_more_menu, new OnMenuClickListener() {
                    @Override
                    public void onMenuClick(MenuItem menuItem) {

                        if (menuItem.getItemId() == R.id.action_rename) {
                            rename(model);
                        } else if (menuItem.getItemId() == R.id.action_publish) {
                            publish(model);
                        } else if (menuItem.getItemId() == R.id.action_unpublish) {
                            publish(model);
                        } else if (menuItem.getItemId() == R.id.action_delete) {
                            delete(model);
                        }
                    }
                })
                .removeMenu(removeId)
                .show(getChildFragmentManager());
    }

    public void rename(WikiInfoModel model) {
        BottomSheetRenameWikiDialogFragment dialogFragment = BottomSheetRenameWikiDialogFragment.newInstance(model.id, model.name);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                loadData();
            }
        });
        dialogFragment.show(getChildFragmentManager(), BottomSheetRenameWikiDialogFragment.class.getSimpleName());
    }

    public void publish(WikiInfoModel model) {
        if (model.is_published) {
            UnpublishWikiDialogFragment dialogFragment = UnpublishWikiDialogFragment.newInstance(model.id);
            dialogFragment.setRefreshListener(new OnRefreshDataListener() {
                @Override
                public void onActionStatus(boolean isDone) {
                    loadData();
                }
            });
            dialogFragment.show(getChildFragmentManager(), UnpublishWikiDialogFragment.class.getSimpleName());
        } else {
            BottomSheetPublishWikiDialogFragment dialogFragment = BottomSheetPublishWikiDialogFragment.newInstance(model.id);
            dialogFragment.setRefreshListener(new OnRefreshDataListener() {
                @Override
                public void onActionStatus(boolean isDone) {
                    loadData();
                }
            });
            dialogFragment.show(getChildFragmentManager(), BottomSheetPublishWikiDialogFragment.class.getSimpleName());
        }

    }

    public void delete(WikiInfoModel model) {
        DeleteWikiDialogFragment dialogFragment = DeleteWikiDialogFragment.newInstance(model.id);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                loadData();
            }
        });
        dialogFragment.show(getChildFragmentManager(), DeleteWikiDialogFragment.class.getSimpleName());
    }
}
