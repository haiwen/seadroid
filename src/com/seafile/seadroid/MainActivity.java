package com.seafile.seadroid;

import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;
import android.content.Context;
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

public class MainActivity extends Activity {

    private static final String SEAHUB_URL = "http://gonggeng.org/seahub/";
    private TextView statusView;

    private String email;
    private String passwd;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        
        statusView = (TextView) findViewById(R.id.status_view);
    }

    /** Called when the user clicks the Login button */
    public void login(View view) {
        EditText emailText = (EditText) findViewById(R.id.email_address);
        email = emailText.getText().toString();

        EditText passwdText = (EditText) findViewById(R.id.password);
        passwd = passwdText.getText().toString();

        ConnectivityManager connMgr = (ConnectivityManager) 
            getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();
        if (networkInfo != null && networkInfo.isConnected()) {
            new LoginTask().execute(email, passwd);
        } else {
            statusView.setText("No network connection available.");
        }
    }

    private class LoginTask extends AsyncTask<String, Void, String> {
        @Override
        protected String doInBackground(String... params) {              
            if (params.length != 2)
                return "Error number of parameter";

            try {
                return doLogin(params[0], params[1]);
            } catch (IOException e) {
                return "Unable to retrieve web page. URL may be invalid.";
            }
        }
        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(String result) {
            statusView.setText(result);
        }

        private String doLogin(String username, String passwd) throws IOException {
            InputStream is = null;
            // Only display the first 500 characters of the retrieved
            // web page content.
            int len = 500;
        
            try {
                URL url = new URL(SEAHUB_URL + "api/login/");
                HttpURLConnection conn = (HttpURLConnection) url.openConnection();
                conn.setReadTimeout(10000 /* milliseconds */);
                conn.setConnectTimeout(15000 /* milliseconds */);

                List<NameValuePair> params = new ArrayList<NameValuePair>();
                params.add(new BasicNameValuePair("username", username));
                params.add(new BasicNameValuePair("password", passwd));

                try {
                    Utils.doPost(conn, params);
                } catch (IOException e) {
                    return e.getMessage();
                }

                // Starts the query
                int response = conn.getResponseCode();
                is = conn.getInputStream();
                
                // Convert the InputStream into a string
                String contentAsString = readIt(is, len);
                return contentAsString;
                
                // Makes sure that the InputStream is closed after the app is
                // finished using it.
            } finally {
                if (is != null) {
                    is.close();
                } 
            }
        }

        public String readIt(InputStream stream, int len) throws IOException,
            UnsupportedEncodingException {
            Reader reader = null;
            reader = new InputStreamReader(stream, "UTF-8");        
            char[] buffer = new char[len];
            reader.read(buffer);
            return new String(buffer);
        }

    }

}
