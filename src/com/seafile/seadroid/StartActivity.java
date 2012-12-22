package com.seafile.seadroid;

import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.util.Log;


public class StartActivity extends Activity {

    private static final String DEBUG_TAG = "StartActivity";
    
    private TextView statusView;
    private Button loginButton;

    private String serverURL;  // like "http://cloud.seafile.com"

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);
        
        statusView = (TextView) findViewById(R.id.status_view);
        loginButton = (Button) findViewById(R.id.login_button);
    }

    /** Called when the user clicks the Login button */
    public void login(View view) {
        EditText serverText = (EditText) findViewById(R.id.server_url);
        serverURL = serverText.getText().toString();
        
        EditText emailText = (EditText) findViewById(R.id.email_address);
        String email = emailText.getText().toString();

        EditText passwdText = (EditText) findViewById(R.id.password);
        String passwd = passwdText.getText().toString();

        ConnectivityManager connMgr = (ConnectivityManager) 
            getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();

        if (networkInfo != null && networkInfo.isConnected()) {
            loginButton.setEnabled(false);
            new LoginTask().execute(serverURL, email, passwd);
        } else {
            statusView.setText("No network connection available.");
        }
    }

    private void startFilesActivity() {
        Intent intent = new Intent(this, BrowserActivity.class);
        intent.putExtra("server", serverURL);
        startActivity(intent);
    }

    private class LoginTask extends AsyncTask<String, Void, String> {

        @Override
        protected String doInBackground(String... params) {              
            if (params.length != 3)
                return "Error number of parameter";

            return doLogin(params[0], params[1], params[2]);
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(String result) {
            if (result.equals("Success")) {
                startFilesActivity();
            } else {
                statusView.setText(result);
            }
            loginButton.setEnabled(true);
        }

        private String doLogin(String serverURL, String username, String passwd) {   
            SeafConnection sc = SeafConnection.getSeafConnection(serverURL);
            //if (sc.ping() == false)
            //    return "ping failed";
            
            if (sc.doLogin(username, passwd) == false)
                return "Login failed";
            
            return "Success";
        }
    }

}
