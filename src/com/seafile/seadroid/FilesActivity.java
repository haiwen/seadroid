package com.seafile.seadroid;

import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;

import java.net.HttpURLConnection;
import java.net.URL;
import java.io.InputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.Reader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;

public class FilesActivity extends Activity {

    private static final String SEAHUB_URL = "http://gonggeng.org/seahub/";

    private String sid;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Get the message from the intent
        Intent intent = getIntent();
        sid = intent.getStringExtra("sessionid");

        // Create the text view
        TextView textView = new TextView(this);
        textView.setTextSize(40);
        textView.setText(sid);

        // Set the text view as the activity layout
        setContentView(textView);
    }


}
