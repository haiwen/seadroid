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


public class MainActivity extends Activity {

    private static final String DEBUG_TAG = "MainActivity";

    private static final String SEAHUB_URL = "http://gonggeng.org/seahub/";
    
    private TextView statusView;
    private Button loginButton;

    private String email;
    private String passwd;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        
        statusView = (TextView) findViewById(R.id.status_view);
        loginButton = (Button) findViewById(R.id.login_button);
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
            loginButton.setEnabled(false);
            new LoginTask().execute(email, passwd);
        } else {
            statusView.setText("No network connection available.");
        }
    }

    private void startFilesActivity() {
        Intent intent = new Intent(this, FilesActivity.class);
        startActivity(intent);
    }

    private class LoginTask extends AsyncTask<String, Void, String> {

        @Override
        protected String doInBackground(String... params) {              
            if (params.length != 2)
                return "Error number of parameter";

            return doLogin(params[0], params[1]);
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

        private String doLogin(String username, String passwd) {   
            SeafConnection sc = SeafConnection.getSeafConnection(SEAHUB_URL);
            if (sc.doLogin(username, passwd) == false)
                return "Login failed";
            
            if (sc.ping() == false)
                return "ping failed";
            
            return "Success";
        }
    }

}
