package com.seafile.seadroid2.play;

import android.os.AsyncTask;

import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

/**
 * get video reused link task
 */
public class VideoLinkTask extends AsyncTask<Void, Long, String> {
    public static final String DEBUG_TAG = "VideoLinkTask";
    private final Account account;
    private final String repoID;
    private final String path;
    private final VideoLinkStateListener listener;

    private SeafConnection sc;
    private SeafException err;

    public VideoLinkTask(Account account, String repoID, String path, VideoLinkStateListener listener) {
        this.account = account;
        this.repoID = repoID;
        this.path = path;
        this.listener = listener;
    }


    @Override
    protected String doInBackground(Void... params) {
        try {
            sc = new SeafConnection(account);
            String dlink = sc.getReUsedFileLink(repoID, path);
            int i = dlink.lastIndexOf('/');
            String link = dlink.substring(0, i) + "/" + URLEncoder.encode(dlink.substring(i + 1), "UTF-8");
            return  link;
        } catch (SeafException e) {
            err = e;
            return null;
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    protected void onPostExecute(String fileLink) {
        if (listener != null) {
            if (fileLink != null) {
                listener.onSuccess(fileLink);
            } else {
                if (err == null) {
                    err = SeafException.unknownException;
                }
                listener.onError(err.getMessage());
            }
        }

    }
}