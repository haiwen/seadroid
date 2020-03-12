package com.seafile.seadroid2.ui.activity;

import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v7.app.AlertDialog;
import android.support.v7.widget.Toolbar;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.animation.AnimationUtils;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SearchedFile;
import com.seafile.seadroid2.play.PlayActivity;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.adapter.SearchAdapter;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Search Activity
 *
 */
public class SearchActivity extends BaseActivity implements View.OnClickListener, Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "SearchActivity";

    private static final String STATE_SEARCHED_RESULT = "searched_result";
    private String mSearchedRlt;
    private EditText mTextField;
    private View mSearchContent;
    private ImageButton mTextClearBtn;
    private View mSearchBtn;
    private ListView mListView;
    private LinearLayout mProgressContainer;
    private LinearLayout mMessageContainer;
    private ImageView mEmptyText;
    private ImageView mErrorText;
    private SearchAdapter mAdapter;
    private List<SearchedFile> mSearchedFiles = Lists.newArrayList();
    private DataManager dataManager;
    private TransferService txService = null;
    private Account account;

    public static final int DOWNLOAD_FILE_REQUEST = 0;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.search);
        mSearchBtn = findViewById(R.id.btn_search);
        mSearchBtn.setOnClickListener(this);
        mTextClearBtn = (ImageButton) findViewById(R.id.btn_clear);
        mTextClearBtn.setOnClickListener(this);
        mTextField = (EditText) findViewById(R.id.et_content);
        mTextField.setOnClickListener(this);
        mTextField.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
        mTextField.addTextChangedListener(new SearchTextWatcher());
        mTextField.setOnEditorActionListener(new EditorActionListener());
        mTextField.requestFocus();
        mSearchContent = findViewById(R.id.search_content);
        mListView = (ListView) findViewById(R.id.lv_search);
        mProgressContainer = (LinearLayout) findViewById(R.id.progressContainer);
        mMessageContainer = (LinearLayout) findViewById(R.id.ll_message_content);
        mEmptyText = (ImageView) findViewById(R.id.iv_empty_txt);
        mErrorText = (ImageView) findViewById(R.id.iv_error_txt);

        mAdapter = new SearchAdapter(this);
        mAdapter.setItems(mSearchedFiles);
        mListView.setAdapter(mAdapter);
        mListView.setOnItemClickListener(new SearchListClickListener());
        //TODO mListView load more data

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
            mSearchContent.setVisibility(View.VISIBLE);
            mMessageContainer.setVisibility(View.GONE);
            mEmptyText.setVisibility(View.GONE);
            mErrorText.setVisibility(View.GONE);
            ArrayList<SearchedFile> files = dataManager.parseSearchResult(mSearchedRlt);
            if(files != null) {
                mAdapter.setItems(files);
                mAdapter.notifyChanged();
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
    protected void onResume() {
        super.onResume();
        if (mSearchedFiles.isEmpty()) {
            mMessageContainer.setVisibility(View.VISIBLE);
        } else {
            mMessageContainer.setVisibility(View.GONE);
        }

    }

    @Override
    public void onClick(View v) {
        final int id = v.getId();
        if (id == R.id.btn_search) {
            // pass 0 to disable page loading
            handleSearch(0);
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

    private void handleSearch(int page) {
        // TODO page loading instead of only display top 100 search result
        page = 100;
        if (!Utils.isNetworkOn()) {
            showShortToast(this, R.string.network_down);
            mMessageContainer.setVisibility(View.VISIBLE);
            return;
        } else
            mMessageContainer.setVisibility(View.GONE);

        String searchText = mTextField.getText().toString().trim();
        if (!TextUtils.isEmpty(searchText)) {
            // mSearchedFiles.clear();

            search(searchText, page);

            Utils.hideSoftKeyboard(mTextField);
        } else {
            showShortToast(this, R.string.search_txt_empty);
        }
    }

    private void search(String content, int page) {
        // start asynctask
        ConcurrentAsyncTask.execute(new SearchLibrariesTask(dataManager, content, page));
    }

    class SearchLibrariesTask extends AsyncTask<Void, Void, ArrayList<SearchedFile>> {

        private DataManager dataManager;
        private String query;
        private int page;
        private SeafException seafException;

        @Override
        protected void onPreExecute() {
            // show loading view
            showLoading(true);
            mSearchBtn.setEnabled(false);
            mMessageContainer.setVisibility(View.GONE);
        }

        public SearchLibrariesTask(DataManager dataManager, String query, int page) {
            this.dataManager = dataManager;
            this.query = query;
            this.page = page;
        }

        @Override
        protected ArrayList<SearchedFile> doInBackground(Void... params) {
            try {
                mSearchedRlt = dataManager.search(query, page);
                return dataManager.parseSearchResult(mSearchedRlt);
            } catch (SeafException e) {
                seafException = e;
                return null;
            }
        }

        @Override
        protected void onPostExecute(ArrayList<SearchedFile> result) {
            // stop loading view
            showLoading(false);
            mSearchBtn.setEnabled(true);

            if (result == null) {
                if (seafException != null) {
                    mMessageContainer.setVisibility(View.VISIBLE);
                    mEmptyText.setVisibility(View.GONE);
                    mErrorText.setVisibility(View.VISIBLE);

                    if (seafException.getCode() == 404)
                        showShortToast(SearchActivity.this, R.string.search_server_not_support);

                    Log.d(DEBUG_TAG, seafException.getMessage() + " code " + seafException.getCode());
                } else {
                    mEmptyText.setVisibility(View.VISIBLE);
                    mErrorText.setVisibility(View.GONE);
                }

                return;
            }

            if (result.size() == 0) {
                mMessageContainer.setVisibility(View.VISIBLE);
                showShortToast(SearchActivity.this, R.string.search_content_empty);
                return;
            }

            // update ui
            mSearchContent.setVisibility(View.VISIBLE);
            mMessageContainer.setVisibility(View.GONE);
            mSearchedFiles = result;
            mAdapter.setItems(mSearchedFiles);
            mAdapter.notifyChanged();
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

    private void showLoading(boolean show) {
        if (show) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_in));
            mListView.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            mListView.setVisibility(View.INVISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_out));
            mListView.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_in));

            mProgressContainer.setVisibility(View.GONE);
            mListView.setVisibility(View.VISIBLE);
        }
    }

    class SearchListClickListener implements AdapterView.OnItemClickListener {

        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            final SearchedFile searchedFile = (SearchedFile) mAdapter.getItem(position);
            onSearchedFileSelected(searchedFile);
        }
    }

    class SearchTextWatcher implements TextWatcher {

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

        @Override
        public void onTextChanged(CharSequence s, int start, int before,
                                  int count) {
            if (mTextField.getText().toString().length() > 0) {
                mTextClearBtn.setVisibility(View.VISIBLE);
                mSearchBtn.setVisibility(View.VISIBLE);
            } else {
                mTextClearBtn.setVisibility(View.GONE);
                mSearchBtn.setVisibility(View.GONE);
            }
        }

        @Override
        public void afterTextChanged(Editable s) {}
    }

    class EditorActionListener implements TextView.OnEditorActionListener {

        @Override
        public boolean onEditorAction(TextView v, int actionId,
                                      KeyEvent event) {
            if (actionId == EditorInfo.IME_ACTION_SEARCH
                    || (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER)) {
                // pass 0 to disable page loading
                handleSearch(0);
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
        startActivity(intent );
    }

    ServiceConnection mConnection = new ServiceConnection() {
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
