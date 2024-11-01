package com.seafile.seadroid2.ui.search;

import android.app.Activity;
import android.app.SearchManager;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;
import androidx.media3.common.util.UnstableApi;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.ActivitySearch2Binding;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.base.adapter.LogicLoadMoreAdapter;
import com.seafile.seadroid2.ui.file.FileActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewActivity;
import com.seafile.seadroid2.ui.media.player.exoplayer.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.ui.sdoc.SDocWebViewActivity;
import com.seafile.seadroid2.view.TipsViews;

import java.io.File;
import java.util.List;
import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;

/**
 * Search Activity
 */
public class Search2Activity extends BaseActivityWithVM<SearchViewModel> implements Toolbar.OnMenuItemClickListener {
    private ActivitySearch2Binding binding;

    private QuickAdapterHelper helper;
    private SearchRecyclerViewAdapter adapter;

    private int page = 0;
    private final int PAGE_SIZE = 20;

    public static void start(Context context, String search) {
        Intent starter = new Intent(context, Search2Activity.class);
        starter.putExtra(SearchManager.QUERY, search);
        starter.setAction(Intent.ACTION_SEARCH);
        context.startActivity(starter);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySearch2Binding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        initView(savedInstanceState);
        initViewModel();
        initAdapter();


        handleIntent(getIntent());

        //firebase - event -login
        Bundle eventBundle = new Bundle();
        eventBundle.putString(FirebaseAnalytics.Param.METHOD, Search2Activity.class.getSimpleName());
        FirebaseAnalytics.getInstance(this).logEvent(FirebaseAnalytics.Event.SEARCH, eventBundle);

    }

    private void initView(Bundle bundle) {
        binding.swipeRefreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                loadNext(lastQuery, true);
            }
        });

        binding.searchView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                if (s != null && s.length() > 0) {
                    delayLoad();
                } else {
                    loadNext(null, true);
                }
            }
        });
        //
        binding.searchView.requestFocus();
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                page--;

                if (e == SeafException.notFoundException) {
                    showAdapterTip(R.string.search_server_not_support);
                } else {
                    showAdapterTip(e.getMessage());
                }
            }
        });

        getViewModel().getListLiveData().observe(this, new Observer<List<SearchModel>>() {
            @Override
            public void onChanged(List<SearchModel> result) {
                if (CollectionUtils.isEmpty(result)) {
                    helper.setTrailingLoadState(new LoadState.NotLoading(true));
                    if (helper.getTrailingLoadStateAdapter() != null) {
                        helper.getTrailingLoadStateAdapter().checkDisableLoadMoreIfNotFullPage();
                    }

                    showAdapterTip(R.string.search_content_empty);

                    return;
                }

                if (page == 1) {
                    adapter.submitList(result);
                } else {
                    adapter.addAll(result);
                }

                if (CollectionUtils.isEmpty(result) || result.size() < PAGE_SIZE) {
                    helper.setTrailingLoadState(new LoadState.NotLoading(true));
                    if (helper.getTrailingLoadStateAdapter() != null) {
                        helper.getTrailingLoadStateAdapter().checkDisableLoadMoreIfNotFullPage();
                    }
                } else {
                    helper.setTrailingLoadState(new LoadState.NotLoading(false));
                }

            }
        });
    }

    private void initAdapter() {
        adapter = new SearchRecyclerViewAdapter();
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<SearchModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<SearchModel, ?> baseQuickAdapter, @NonNull View view, int i) {


                onItemClick(adapter.getItems().get(i));
            }
        });

        LogicLoadMoreAdapter logicLoadMoreAdapter = getLogicLoadMoreAdapter();

        helper = new QuickAdapterHelper.Builder(adapter)
                .setTrailingLoadStateAdapter(logicLoadMoreAdapter)
                .build();

        binding.rv.setAdapter(helper.getAdapter());
    }

    @NonNull
    private LogicLoadMoreAdapter getLogicLoadMoreAdapter() {
        LogicLoadMoreAdapter logicLoadMoreAdapter = new LogicLoadMoreAdapter();
        logicLoadMoreAdapter.setOnLoadMoreListener(new TrailingLoadStateAdapter.OnTrailingListener() {
            @Override
            public void onFailRetry() {
                loadNext(lastQuery, false);
            }

            @Override
            public void onLoad() {
                loadNext(lastQuery, false);
            }

            @Override
            public boolean isAllowLoading() {
                return !binding.swipeRefreshLayout.isRefreshing();
            }
        });
        return logicLoadMoreAdapter;
    }

    private Disposable disposable;

    private void delayLoad() {
        if (disposable != null && !disposable.isDisposed()) {
            disposable.dispose();
        }

        disposable = Observable.timer(1000, TimeUnit.MILLISECONDS)
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(aLong -> {
                    loadNext(binding.searchView.getText().toString(), true);
                });

    }

    private void showAdapterTip(int textRes) {
        showAdapterTip(getString(textRes));
    }

    private void showAdapterTip(String textRes) {
        adapter.submitList(null);

        TextView tipView = TipsViews.getTipTextView(this);
        tipView.setText(textRes);
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(true);

        helper.setTrailingLoadState(new LoadState.NotLoading(true));
        if (helper.getTrailingLoadStateAdapter() != null) {
            helper.getTrailingLoadStateAdapter().checkDisableLoadMoreIfNotFullPage();
        }
    }

    @Override
    protected void onDestroy() {

        if (disposable != null && !disposable.isDisposed()) {
            disposable.dispose();
        }

        super.onDestroy();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        handleIntent(intent);
    }

    private void handleIntent(Intent intent) {
        if (intent == null) {
            return;
        }

        if (Intent.ACTION_SEARCH.equals(intent.getAction())) {
            String query = intent.getStringExtra(SearchManager.QUERY);
            if (!TextUtils.isEmpty(query)) {
                binding.searchView.setText(query);
                binding.searchView.setSelection(query.length());
                binding.searchView.clearFocus();
                loadNext(query, true);
            }
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getActionBarToolbar().setOnMenuItemClickListener(this);
        return true;
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return onOptionsItemSelected(item);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    private String lastQuery = null;

    private void loadNext(String query, boolean isRefresh) {
        if (!Utils.isNetworkOn()) {
            ToastUtils.showLong(R.string.network_down);
            return;
        }

        lastQuery = query;

        if (TextUtils.isEmpty(query)) {
            adapter.submitList(null);

            helper.setTrailingLoadState(new LoadState.NotLoading(true));
            if (helper.getTrailingLoadStateAdapter() != null) {
                helper.getTrailingLoadStateAdapter().checkDisableLoadMoreIfNotFullPage();
            }

            return;
        }

//        SearchRecentSuggestions searchRecentSuggestions = new SearchRecentSuggestions(this, RecentSearchSuggestionsProvider.AUTHORITY, RecentSearchSuggestionsProvider.MODE);
//        searchRecentSuggestions.saveRecentQuery(query, null);

        if (isRefresh) {
            page = 0;
        }

        page++;

        getViewModel().loadNext(query, page, PAGE_SIZE);
    }

    public void onItemClick(SearchModel searchedFile) {
        final String repo_id = searchedFile.repo_id;
        final String repo_name = searchedFile.repo_name;
        final String fileName = searchedFile.name;
        final String filePath = searchedFile.fullpath;

        if (searchedFile.is_dir) {
            navTo(searchedFile);
            return;
        }

        getViewModel().getRepoModel(repo_id, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel == null) {
                    return;
                }

                open(repoModel, searchedFile, fileName, filePath);
            }
        });
    }

    private void open(RepoModel repoModel, SearchModel searchedFile, String fileName, String filePath) {
        if (fileName.endsWith(Constants.Format.DOT_SDOC)) {
            SDocWebViewActivity.openSdoc(this, repoModel.repo_name, repoModel.repo_id, filePath);
        } else if (Utils.isViewableImage(fileName) && !repoModel.encrypted) {
            // Encrypted repo does not support gallery,
            // because pic thumbnail under encrypted repo was not supported at the server side
            ImagePreviewActivity.startThisFromSearch(this, searchedFile);
        } else if (Utils.isVideoFile(fileName)) { // is video file
            final MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setItems(R.array.video_download_array, new DialogInterface.OnClickListener() {
                @OptIn(markerClass = UnstableApi.class)
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    if (which == 0) {
                        CustomExoVideoPlayerActivity.startThis(Search2Activity.this, fileName, repoModel.repo_id, filePath);
                    } else if (which == 1) {
                        Intent intent = FileActivity.startFromSearch(Search2Activity.this, searchedFile, "video_download");
                        fileActivityLauncher.launch(intent);
                    }
                }
            }).show();
        } else {
            checkFileAndStartFileActivity(searchedFile, true);
        }
    }

    private void checkFileAndStartFileActivity(SearchModel searchedFile, boolean isOpenWith) {

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        File localFile = DataManager.getLocalRepoFile(account, searchedFile.repo_id, searchedFile.repo_name, searchedFile.fullpath);
        if (localFile.exists()) {
            WidgetUtils.openWith(this, localFile);
        } else {
            Intent intent = FileActivity.startFromSearch(this, searchedFile, "search");
            fileActivityLauncher.launch(intent);
        }
    }

    private void navTo(SearchModel model) {
        MainActivity.navToThis(this, model.repo_id, model.repo_name, model.fullpath, model.is_dir);
    }


    private final ActivityResultLauncher<Intent> fileActivityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK || o.getData() == null) {
                return;
            }

            String action = o.getData().getStringExtra("action");
            String path = o.getData().getStringExtra("path");

            if ("search".equals(action) && !TextUtils.isEmpty(path)) {
                WidgetUtils.openWith(Search2Activity.this, new File(path));
            }
        }
    });


}
