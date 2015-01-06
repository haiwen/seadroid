package com.seafile.seadroid2.ui.fragment;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.app.Activity;
import android.content.Intent;
import android.net.http.SslCertificate;
import android.net.http.SslCertificate.DName;
import android.net.http.SslError;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.webkit.JsResult;
import android.webkit.SslErrorHandler;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.FrameLayout;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragment;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.CertsManager;
import com.seafile.seadroid2.ui.activity.FileActivity;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafRepo;

public class ActivitiesFragment extends SherlockFragment {
    private static final String DEBUG_TAG = "ActivitiesFragment";

    private static final String ACTIVITIES_URL = "api2/html/events/";

    private WebView webView = null;
    private FrameLayout mWebViewContainer = null;
    private View mProgressContainer;

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "ActivitiesFragment Attached");
    }

    @Override
    public void onDetach() {
        super.onDetach();
    }

    private BrowserActivity getBrowserActivity() {
        return (BrowserActivity)getActivity();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        return inflater.inflate(R.layout.activities_fragment, container, false);
    }

    @Override
    public void onPause() {
        Log.d(DEBUG_TAG, "onPause");
        super.onPause();

        if (webView != null) {
            mWebViewContainer.removeView(webView);
        }
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onActivityCreated");

        mWebViewContainer = (FrameLayout)getView().findViewById(R.id.webViewContainer);
        mProgressContainer = getView().findViewById(R.id.progressContainer);

        if (webView == null) {
            webView = new WebView(getBrowserActivity());
            initWebView();
            loadActivitiesPage();
        }

        getBrowserActivity().supportInvalidateOptionsMenu();

        super.onActivityCreated(savedInstanceState);
    }

    @Override
    public void onResume() {
        // We add the webView on create and remove it on pause, so as to make
        // it retain state. Otherwise the webview will reload the url every
        // time the tab is switched.
        super.onResume();
        mWebViewContainer.addView(webView);
    }

    public void refreshView() {
        loadActivitiesPage();
    }

    private void initWebView() {
        webView.getSettings().setJavaScriptEnabled(true);
        webView.setWebViewClient(new MyWebViewClient());

        webView.setWebChromeClient(new MyWebChromeClient());
    }

    private void loadActivitiesPage() {
        showPageLoading(true);
        Account account = getBrowserActivity().getAccount();
        String url = account.getServer() + ACTIVITIES_URL;

        webView.loadUrl(url, getExtraHeaders());
    }

    private Map<String, String> getExtraHeaders() {
        Account account = getBrowserActivity().getAccount();
        String token = "Token " + account.getToken();
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("Authorization", token);

        return headers;
    }

    private void showPageLoading(boolean pageLoading) {
        if (getBrowserActivity() == null) {
            return;
        }

        if (!pageLoading) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                                    getBrowserActivity(), android.R.anim.fade_out));
            webView.startAnimation(AnimationUtils.loadAnimation(
                                getBrowserActivity(), android.R.anim.fade_in));
            mProgressContainer.setVisibility(View.GONE);
            webView.setVisibility(View.VISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    getBrowserActivity(), android.R.anim.fade_in));
            webView.startAnimation(AnimationUtils.loadAnimation(
                    getBrowserActivity(), android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            webView.setVisibility(View.INVISIBLE);
        }
    }

    private void viewRepo(String repoID) {
        SeafRepo repo = getBrowserActivity().getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            getBrowserActivity().showToast("Couldn't find this library. It may be deleted");
            return;
        }

        NavContext nav = getBrowserActivity().getNavContext();

        nav.setRepoID(repoID);
        nav.setRepoName(repo.getName());
        nav.setDir("/", repo.getRootDirID());

        // switch to LIBRARY TAB
        getBrowserActivity().setCurrentPosition(0);
    }

    private void viewFile(String repoID, String path) {
        SeafRepo repo = getBrowserActivity().getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            getBrowserActivity().showToast(R.string.library_not_found);
            return;
        }
        int taskID = getBrowserActivity().getTransferService().addDownloadTask(getBrowserActivity().getAccount(), repo.getName(), repoID, path);
        Intent intent = new Intent(getActivity(), FileActivity.class);
        intent.putExtra("repoName", repo.getName());
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", path);
        intent.putExtra("account", getBrowserActivity().getAccount());
        intent.putExtra("taskID", taskID);
        startActivity(intent);
    }

    private class MyWebViewClient extends WebViewClient {
        // Display error messages
        @Override
        public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
            if (getBrowserActivity() != null) {
                Toast.makeText(getBrowserActivity(), "Error: " + description, Toast.LENGTH_SHORT).show();
                showPageLoading(false);
            }
        }

        // Ignore SSL certificate validate
        @Override
        public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
            BrowserActivity mActivity = getBrowserActivity();
            if (mActivity == null) {
                return;
            }

            Account account = mActivity.getAccount();

            SslCertificate sslCert = error.getCertificate();
            X509Certificate savedCert = CertsManager.instance().getCertificate(account);

            if (isSameCert(sslCert, savedCert)) {
                Log.d(DEBUG_TAG, "trust this cert");
                handler.proceed();
            } else {
                Log.d(DEBUG_TAG, "cert is not trusted");
                mActivity.showToast(R.string.ssl_error);
                showPageLoading(false);
            }
        }

        @Override
        public boolean shouldOverrideUrlLoading(WebView webView, String url) {
            Log.d(DEBUG_TAG, "loading url " + url);
            String API_URL_PREFIX= "api://";
            if (!url.startsWith(API_URL_PREFIX)) {
                return false;
            }

            String req = url.substring(API_URL_PREFIX.length(), url.length());

            Pattern REPO_PATTERN = Pattern.compile("repos/([-a-f0-9]{36})/?");
            Pattern REPO_FILE_PATTERN = Pattern.compile("repo/([-a-f0-9]{36})/files/\\?p=(.+)");
            Matcher matcher;

            if ((matcher = fullMatch(REPO_PATTERN, req)) != null) {
                String repoID = matcher.group(1);
                viewRepo(repoID);

            } else if ((matcher = fullMatch(REPO_FILE_PATTERN, req)) != null) {
                String repoID = matcher.group(1);

                try {
                    String path = URLDecoder.decode(matcher.group(2), "UTF-8");
                    viewFile(repoID, path);
                } catch (UnsupportedEncodingException e) {
                    // Ignore
                }
            }

            return true;
        }

        @Override
        public void onPageFinished(WebView webView, String url) {
            Log.d(DEBUG_TAG, "onPageFinished " + url);
            if (getBrowserActivity() != null) {
                String js = String.format("javascript:setToken('%s')",
                                          getBrowserActivity().getAccount().getToken());
                webView.loadUrl(js);
            }
            showPageLoading(false);
        }
    }

    private static Matcher fullMatch(Pattern pattern, String str) {
        Matcher matcher = pattern.matcher(str);
        return matcher.matches() ? matcher : null;
    }

    private class MyWebChromeClient extends WebChromeClient {

        // For debug js
        @Override
        public boolean onJsAlert(WebView view, String url, String message, JsResult result) {
            Log.d(DEBUG_TAG, "alert: " + message);
            return super.onJsAlert(view, url, message, result);
        }
    }

    /**
     * SslCertificate class does not has a public getter for the underlying
     * X509Certificate, we can only do this by hack. This only works for andorid 4.0+
     * @see https://groups.google.com/forum/#!topic/android-developers/eAPJ6b7mrmg
     */
    private static X509Certificate getX509CertFromSslCertHack(SslCertificate sslCert) {
        X509Certificate x509Certificate = null;

        Bundle bundle = SslCertificate.saveState(sslCert);
        byte[] bytes = bundle.getByteArray("x509-certificate");

        if (bytes == null) {
            x509Certificate = null;
        } else {
            try {
                CertificateFactory certFactory = CertificateFactory.getInstance("X.509");
                Certificate cert = certFactory.generateCertificate(new ByteArrayInputStream(bytes));
                x509Certificate = (X509Certificate) cert;
            } catch (CertificateException e) {
                x509Certificate = null;
            }
        }

        return x509Certificate;
    }

    private static boolean isSameCert(SslCertificate sslCert, X509Certificate x509Cert) {
        if (sslCert == null || x509Cert == null) {
            return false;
        }

        X509Certificate realCert = getX509CertFromSslCertHack(sslCert);
        if (realCert != null) {
            // for android 4.0+
            return realCert.equals(x509Cert);
        } else {
            // for andorid < 4.0
            return SslCertificateComparator.compare(sslCert,
                                                    new SslCertificate(x509Cert));
        }
    }

    /**
     * Compare SslCertificate objects for android before 4.0
     */
    private static class SslCertificateComparator {
        private SslCertificateComparator() {
        }

        public static boolean compare(SslCertificate cert1, SslCertificate cert2) {
            return isSameDN(cert1.getIssuedTo(), cert2.getIssuedTo())
                && isSameDN(cert1.getIssuedBy(), cert2.getIssuedBy())
                && isSameDate(cert1.getValidNotBeforeDate(), cert2.getValidNotBeforeDate())
                && isSameDate(cert1.getValidNotAfterDate(), cert2.getValidNotAfterDate());
        }

        private static boolean isSameDate(Date date1, Date date2) {
            if (date1 == null && date2 == null) {
                return true;
            } else if (date1 == null || date2 == null) {
                return false;
            }

            return date1.equals(date2);
        }

        private static boolean isSameDN(DName dName1, DName dName2) {
            if (dName1 == null && dName2 == null) {
                return true;
            } else if (dName1 == null || dName2 == null) {
                return false;
            }

            return dName1.getDName().equals(dName2.getDName());
        }
    }
}
