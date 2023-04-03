package com.seafile.seadroid2.ui.activity.search;

import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v7.app.AlertDialog;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.support.v7.widget.Toolbar;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import com.blankj.utilcode.util.CollectionUtils;
import com.jcodecraeer.xrecyclerview.ProgressStyle;
import com.jcodecraeer.xrecyclerview.XRecyclerView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SearchedFile;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.play.PlayActivity;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.activity.FileActivity;
import com.seafile.seadroid2.ui.widget.SupportRecyclerView;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.ArrayList;

/**
 * Search Activity
 */
public class Search2Activity extends BaseActivity implements View.OnClickListener, Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "SearchActivity";

    private static final String STATE_SEARCHED_RESULT = "searched_result";
    private String mSearchedRlt;
    private EditText mTextField;
    private ImageView mTextClearBtn;
    private View mSearchBtn;
    private SupportRecyclerView mRecyclerView;

    private SearchRecyclerViewAdapter mAdapter;
    private DataManager dataManager;
    private TransferService txService = null;
    private Account account;

    public static final int DOWNLOAD_FILE_REQUEST = 0;
    private boolean hasMore = true;
    private int page = 1;
    private int PAGE_SIZE = 20;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.search2);
        mSearchBtn = findViewById(R.id.btn_search);
        mSearchBtn.setOnClickListener(this);
        mTextClearBtn = findViewById(R.id.btn_clear);
        mTextClearBtn.setOnClickListener(this);

        mTextField = findViewById(R.id.et_content);
        mTextField.setOnClickListener(this);
        mTextField.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
        mTextField.addTextChangedListener(new SearchTextWatcher());
        mTextField.setOnEditorActionListener(new EditorActionListener());
        mTextField.requestFocus();

        mRecyclerView = findViewById(R.id.lv_search);
        mAdapter = new SearchRecyclerViewAdapter(this);
        mAdapter.setOnItemClickListener(new OnItemClickListener<SearchedFile>() {
            @Override
            public void onItemClick(SearchedFile searchedFile, int position) {
                onSearchedFileSelected(searchedFile);
            }
        });
        mRecyclerView.setAdapter(mAdapter);
        mRecyclerView.setLayoutManager(new LinearLayoutManager(this, RecyclerView.VERTICAL, false));
        View t = getLayoutInflater().inflate(R.layout.search_empty, (ViewGroup) mRecyclerView.getParent(), false);
        mRecyclerView.setEmptyView(t);
        mRecyclerView.setLoadingMoreEnabled(true);
        mRecyclerView.setLoadingMoreProgressStyle(ProgressStyle.BallScaleMultiple);
        mRecyclerView.setPullRefreshEnabled(false);
        mRecyclerView.setLoadingListener(new XRecyclerView.LoadingListener() {
            @Override
            public void onRefresh() {

            }

            @Override
            public void onLoadMore() {
                Log.d(DEBUG_TAG, "onLoadMore");

                if (isHasMore()) {
                    loadNext(false);
                }
            }
        });

        setSupportActionBar(getActionBarToolbar());
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.search_menu_item);
        initData();
    }

    @Override
    public void onSaveInstanceState(Bundle savedInstanceState) {
        // Save the searched result
        savedInstanceState.putString(STATE_SEARCHED_RESULT, mSearchedRlt);

        // Always call the superclass so it can save the view hierarchy state
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        // Always call the superclass so it can restore the view hierarchy
        super.onRestoreInstanceState(savedInstanceState);

        // Restore state members from saved instance
        mSearchedRlt = savedInstanceState.getString(STATE_SEARCHED_RESULT);

        // update ui
        if (dataManager != null) {
            ArrayList<SearchedFile> files = dataManager.parseSearchResult(mSearchedRlt);
            if (files != null) {
                mAdapter.notifyDataClear();
                mAdapter.notifyDataChanged(files);
            }
        }
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }

        super.onDestroy();
    }

    private void initData() {
        AccountManager accountManager = new AccountManager(this);
        account = accountManager.getCurrentAccount();
        dataManager = new DataManager(account);

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getActionBarToolbar().setOnMenuItemClickListener(this);
        return true;
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
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
    public void onClick(View v) {
        final int id = v.getId();
        if (id == R.id.btn_search) {
            loadNext(true);
        } else if (id == R.id.btn_clear) {
            mTextField.getText().clear();
        }

    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
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

    public boolean isHasMore() {
        return hasMore;
    }

    public int getPageSize() {
        return PAGE_SIZE;
    }

    public int getPage() {
        return page;
    }

    public void increasePage() {
        this.page++;
    }

    private void loadNext(boolean isRefresh) {
        if (!Utils.isNetworkOn()) {
            showShortToast(this, R.string.network_down);
            return;
        }

        if (isRefresh) {
            page = 1;
            mRecyclerView.setLoadingMoreEnabled(true);
            mAdapter.notifyDataClear();
        }

        String searchText = mTextField.getText().toString().trim();
        if (!TextUtils.isEmpty(searchText)) {

            search(searchText, getPage(), getPageSize());

            Utils.hideSoftKeyboard(mTextField);
        } else {
            showShortToast(this, R.string.search_txt_empty);
        }
    }

    private void search(String content, int page, int pageSize) {
        // start asynctask
        ConcurrentAsyncTask.execute(new SearchLibrariesTask(dataManager, content, page, pageSize));
    }

    private class SearchLibrariesTask extends AsyncTask<Void, Void, ArrayList<SearchedFile>> {

        private DataManager dataManager;
        private String query;
        private int pageSize;
        private int page;

        private SeafException seafException;

        @Override
        protected void onPreExecute() {
            // show loading view
            mSearchBtn.setEnabled(false);
        }

        public SearchLibrariesTask(DataManager dataManager, String query, int page, int pageSize) {
            this.dataManager = dataManager;
            this.query = query;
            this.pageSize = pageSize;
            this.page = page;
        }

        @Override
        protected ArrayList<SearchedFile> doInBackground(Void... params) {
            try {
                mSearchedRlt = dataManager.search(query, page, pageSize);
                return dataManager.parseSearchResult(mSearchedRlt);
            } catch (SeafException e) {
                seafException = e;
                return null;
            }
        }

        @Override
        protected void onPostExecute(ArrayList<SearchedFile> result) {
            // stop loading view
            mSearchBtn.setEnabled(true);

            if (CollectionUtils.isEmpty(result) || result.size() < getPageSize()) {
                hasMore = false;
            }

            mRecyclerView.loadMoreComplete();

            if (!isHasMore()) {
                mRecyclerView.setLoadingMoreEnabled(false);
            }

            if (result == null) {
                if (seafException != null) {
                    if (seafException.getCode() == 404)
                        showShortToast(Search2Activity.this, R.string.search_server_not_support);

                    Log.d(DEBUG_TAG, seafException.getMessage() + " code " + seafException.getCode());
                }

                return;
            }

            if (result.size() == 0) {
                showShortToast(Search2Activity.this, R.string.search_content_empty);
                return;
            }

            increasePage();

            mAdapter.notifyDataChanged(result);
        }
    }

    public DataManager getDataManager() {
        if (dataManager == null) {
            AccountManager accountManager = new AccountManager(this);
            account = accountManager.getCurrentAccount();
            dataManager = new DataManager(account);
        }
        return dataManager;
    }

    class SearchTextWatcher implements TextWatcher {

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {
        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            if (mTextField.getText().toString().length() > 0) {
                mTextClearBtn.setVisibility(View.VISIBLE);
                mSearchBtn.setVisibility(View.VISIBLE);
            } else {
                mTextClearBtn.setVisibility(View.GONE);
                mSearchBtn.setVisibility(View.GONE);
            }
        }

        @Override
        public void afterTextChanged(Editable s) {
        }
    }

    class EditorActionListener implements TextView.OnEditorActionListener {
        @Override
        public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
            if (actionId == EditorInfo.IME_ACTION_SEARCH
                    || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)) {
                // pass 0 to disable page loading
                loadNext(true);
                return true;
            }
            return false;
        }
    }

    public void onSearchedFileSelected(SearchedFile searchedFile) {
        final String repoID = searchedFile.getRepoID();
        final String fileName = searchedFile.getTitle();
        final SeafRepo repo = dataManager.getCachedRepoByID(repoID);
        final String repoName = repo.getName();
        final String filePath = searchedFile.getPath();

        if (searchedFile.isDir()) {
            if (repo == null) {
                showShortToast(this, R.string.search_library_not_found);
                return;
            }
            WidgetUtils.showRepo(this, repoID, repoName, filePath, null);
            return;
        }

        // Encrypted repo doesn\`t support gallery,
        // because pic thumbnail under encrypted repo was not supported at the server side
        if (Utils.isViewableImage(searchedFile.getTitle())
                && repo != null && !repo.encrypted) {
            WidgetUtils.startGalleryActivity(this, repoName, repoID, Utils.getParentPath(filePath), searchedFile.getTitle(), account);
            return;
        }

        final File localFile = dataManager.getLocalCachedFile(repoName, repoID, filePath, null);
        if (localFile != null) {
            WidgetUtils.showFile(this, localFile);
            return;
        }
        boolean videoFile = Utils.isVideoFile(fileName);
        if (videoFile) { // is video file
            final AlertDialog.Builder builder = new AlertDialog.Builder(this);
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
        Intent intent = new Intent(this, PlayActivity.class);
        intent.putExtra("fileName", fileName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
//        DOWNLOAD_PLAY_REQUEST
        startActivity(intent);
    }

    private final ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
            Log.d(DEBUG_TAG, "bind TransferService");
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };
}
