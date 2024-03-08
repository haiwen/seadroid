package com.seafile.seadroid2.ui.search;

import static androidx.cursoradapter.widget.CursorAdapter.FLAG_AUTO_REQUERY;

import android.app.SearchManager;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.SearchRecentSuggestions;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.AutoCompleteTextView;
import android.widget.TextView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.SearchView;
import androidx.appcompat.widget.Toolbar;
import androidx.core.view.OnApplyWindowInsetsListener;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.cursoradapter.widget.CursorAdapter;
import androidx.cursoradapter.widget.SimpleCursorAdapter;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.chad.library.adapter4.QuickAdapterHelper;
import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.search.SearchBar;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.model.search.SearchModel;
import com.seafile.seadroid2.databinding.ActivitySearch2Binding;
import com.seafile.seadroid2.play.exoplayer.CustomExoVideoPlayerActivity;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.activity.FileActivity;
import com.seafile.seadroid2.ui.base.adapter.CustomLoadMoreAdapter;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.media.image_preview.ImagePreviewActivity;
import com.seafile.seadroid2.util.SearchUtils;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.view.TipsViews;

import java.io.File;
import java.util.List;
import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;

/**
 * Search Activity
 */
public class Search2Activity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private ActivitySearch2Binding binding;
    private SearchViewModel viewModel;

    private QuickAdapterHelper helper;
    private SearchRecyclerViewAdapter adapter;
    private DataManager dataManager;
    private TransferService txService = null;
    private Account account;

    public static final int DOWNLOAD_FILE_REQUEST = 0;
    private int page = 0;
    private final int PAGE_SIZE = 20;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySearch2Binding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        initView(savedInstanceState);
        initViewModel();
        initAdapter();

        initData();

        handleIntent(getIntent());
    }

    private void initView(Bundle bundle) {
        binding.swipeRefreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                loadNext(lastQuery, true);
            }
        });
        binding.searchView.setSubmitButtonEnabled(false);

        binding.searchView.findViewById(R.id.search_src_text).setBackground(null);
//        Cursor cursor = getSuggestionsCursor();
//        SimpleCursorAdapter simpleCursorAdapter = new SimpleCursorAdapter(this, R.layout.item_search_suggestion, cursor,
//                new String[]{"suggest_text_1"},
//                new int[]{R.id.cat_searchbar_suggestion_title});
//
//        binding.searchView.setSuggestionsAdapter(simpleCursorAdapter);

        binding.searchView.setOnSuggestionListener(new SearchView.OnSuggestionListener() {
            @Override
            public boolean onSuggestionSelect(int position) {

                return false;
            }

            @Override
            public boolean onSuggestionClick(int position) {
                return false;
            }
        });

        binding.searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
            @Override
            public boolean onQueryTextSubmit(String query) {
                loadNext(query, true);
                return false;
            }

            @Override
            public boolean onQueryTextChange(String newText) {
                return false;
            }
        });

        AutoCompleteTextView autoCompleteTextView = binding.searchView.findViewById(R.id.search_src_text);
        autoCompleteTextView.addTextChangedListener(new TextWatcher() {
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
        viewModel = new ViewModelProvider(this).get(SearchViewModel.class);

        viewModel.getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        viewModel.getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
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

        viewModel.getListLiveData().observe(this, new Observer<List<SearchModel>>() {
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
        adapter = new SearchRecyclerViewAdapter(this);
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<SearchModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<SearchModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                onSearchedFileSelected(adapter.getItems().get(i));
            }
        });

        CustomLoadMoreAdapter customLoadMoreAdapter = new CustomLoadMoreAdapter();
        customLoadMoreAdapter.setOnLoadMoreListener(new TrailingLoadStateAdapter.OnTrailingListener() {
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

        helper = new QuickAdapterHelper.Builder(adapter)
                .setTrailingLoadStateAdapter(customLoadMoreAdapter)
                .build();

        binding.rv.setAdapter(helper.getAdapter());
    }

    private Cursor getSuggestionsCursor() {

        Uri.Builder uriBuilder = new Uri.Builder()
                .scheme(ContentResolver.SCHEME_CONTENT)
                .authority(RecentSearchSuggestionsProvider.AUTHORITY);
        uriBuilder.appendPath(SearchManager.SUGGEST_URI_PATH_QUERY);

        String selection = " ?";
        String[] selArgs = new String[]{""};
        Uri uri = uriBuilder.build();
        Cursor cursor = getContentResolver().query(uri, null, null, selArgs, null);
        return cursor;
    }


    private Disposable disposable;

    private void delayLoad() {
        if (disposable != null && !disposable.isDisposed()) {
            disposable.dispose();
        }

        disposable = Observable.timer(1000, TimeUnit.MILLISECONDS)
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(aLong -> {
                    loadNext(binding.searchView.getQuery().toString(), true);
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
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }

        if (disposable != null && !disposable.isDisposed()) {
            disposable.dispose();
        }

        super.onDestroy();
    }

    private void initData() {
        account = SupportAccountManager.getInstance().getCurrentAccount();
        dataManager = new DataManager(account);

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
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
            Log.d("SEARCH", "Search query was: $query");
            binding.searchView.setQuery(query, false);
            loadNext(query, true);
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

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case DOWNLOAD_FILE_REQUEST:
                if (resultCode == RESULT_OK) {
                    File file = new File(data.getStringExtra("path"));
                    WidgetUtils.showFile(this, file);
                }
            default:
                break;
        }
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

        SearchRecentSuggestions searchRecentSuggestions = new SearchRecentSuggestions(this, RecentSearchSuggestionsProvider.AUTHORITY, RecentSearchSuggestionsProvider.MODE);
        searchRecentSuggestions.saveRecentQuery(query, null);


        if (isRefresh) {
            page = 0;
        }

        page++;

        viewModel.loadNext(query, page, PAGE_SIZE);
    }

    public void onSearchedFileSelected(SearchModel searchedFile) {
        final String repoID = searchedFile.repo_id;
        final String repoName = searchedFile.repo_name;
        final String fileName = searchedFile.name;
        final String filePath = searchedFile.fullpath;

        if (searchedFile.is_dir) {
//            if (repo == null) {
//                ToastUtils.showLong(R.string.search_library_not_found);
//                return;
//            }

            navTo(searchedFile);
            return;
        }

        // Encrypted repo doesn\`t support gallery,
        // because pic thumbnail under encrypted repo was not supported at the server side
        //TODO && !repo.encrypted
        if (Utils.isViewableImage(searchedFile.name)) {
            ImagePreviewActivity.startThis(this, repoID, searchedFile.fullpath);
            return;
        }

        final File localFile = dataManager.getLocalCachedFile(repoName, repoID, filePath, null);
        if (localFile != null) {
            WidgetUtils.showFile(this, localFile);
            return;
        }

        boolean videoFile = Utils.isVideoFile(fileName);
        if (videoFile) { // is video file
            final MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setItems(R.array.video_download_array, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    if (which == 0) // create file
                        startPlayActivity(fileName, repoID, filePath);
                    else if (which == 1) // create folder
                        startFileActivity(repoName, repoID, filePath);
                }
            }).show();
            return;
        }

        startFileActivity(repoName, repoID, filePath);
    }

    private void navTo(SearchModel model) {
        MainActivity.navToThis(this, model.repo_id, model.repo_name, model.fullpath, model.is_dir);
    }

    private void startFileActivity(String repoName, String repoID, String filePath) {
        final int taskID = txService.addDownloadTask(account, repoName, repoID, filePath);
        Intent intent = new Intent(this, FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
        intent.putExtra("taskID", taskID);
        startActivityForResult(intent, DOWNLOAD_FILE_REQUEST);
    }

    private void startPlayActivity(String fileName, String repoID, String filePath) {
        Intent intent = new Intent(this, CustomExoVideoPlayerActivity.class);
        intent.putExtra("fileName", fileName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
        startActivity(intent);
    }

    private final ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };


}
