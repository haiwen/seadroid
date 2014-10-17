package com.seafile.seadroid2.ui;

import java.io.UnsupportedEncodingException;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import android.annotation.SuppressLint;
import android.app.ProgressDialog;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;

@SuppressLint("NewApi")
public class FeedbackActivity extends SherlockFragmentActivity {
    private TextView feedbackTitle;
    private EditText feedback;
    private Button send;
    private String emailSubject;
    private static final String username = "username";
    private static final String password = "password";
    @Override
    protected void onCreate(Bundle bundle) {
        super.onCreate(bundle);
        setContentView(R.layout.feedback);
        feedbackTitle = (TextView) findViewById(R.id.settings_feedback_hint_tv);
        feedback = (EditText) findViewById(R.id.settings_feedback_input_et);
        send = (Button) findViewById(R.id.settings_feedback_send_btn);
        send.setOnClickListener(new OnClickListener() {
            
            @Override
            public void onClick(View v) {
                if (!feedback.getText().toString().trim().equals("")) {
                    // send email to developers
                    sendMail("logan676@163.com", emailSubject, feedback.getText().toString().trim());
                    
                } else {
                    feedback.setError(getResources().getString(R.string.settings_about_feedback_input_error));
                    return;
                }
            }
        });
        Intent intent = getIntent();
        int flag = intent.getFlags();
        switch (flag) {
        case SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_LIKE:
            feedbackTitle.setText(R.string.settings_about_feedback_like);
            emailSubject = "user likes features in Seadroid";
            break;
        case SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_DISLIKE:
            feedbackTitle.setText(R.string.settings_about_feedback_dislike);
            emailSubject = "Oops, user dislike features in Seadroid";
            break;
        case SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_BUG:
            feedbackTitle.setText(R.string.settings_about_feedback_bug);
            emailSubject = "Boom, bugs occur in Seadroid";
            break;
        case SettingsPreferenceFragment.SETTINGS_FEEDBACK_NEED_HELP:
            feedbackTitle.setText(R.string.settings_about_feedback_help);
            emailSubject = "help user get through issues";
            break;
        case SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_OTHERS:
            feedbackTitle.setText(R.string.settings_about_feedback_others);
            emailSubject = "time for free talk and brain storm";
            break;
        
        default:
            break;
        }
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);
        actionBar.setTitle(R.string.settings_about_feedback_title);
        // actionBar.setDisplayOptions();
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
         switch (item.getItemId()) {
            case android.R.id.home:
                this.finish();
            default:
                return super.onOptionsItemSelected(item);
        }
    }
    private void sendMail(String emailAddress, String subject, String messageBody) {
        Session session = createSessionObject();

        try {
            Message message = createMessage(emailAddress, subject, messageBody, session);
            new SendMailTask().execute(message);
        } catch (AddressException e) {
            e.printStackTrace();
        } catch (MessagingException e) {
            e.printStackTrace();
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
    }

    private Message createMessage(String email, String subject, String messageBody, Session session) throws MessagingException, UnsupportedEncodingException {
        Message message = new MimeMessage(session);
        message.setFrom(new InternetAddress("logan676@163.com", "Logan Guo"));
        message.addRecipient(Message.RecipientType.TO, new InternetAddress(email, email));
        message.setSubject(subject);
        message.setText(messageBody);
        return message;
    }

    private Session createSessionObject() {
        Properties properties = new Properties();
        properties.put("mail.smtp.auth", "true");
        properties.put("mail.smtp.starttls.enable", "true");
        properties.put("mail.smtp.host", "smtp.163.com ");
        properties.put("mail.smtp.port", "465");

        return Session.getInstance(properties, new javax.mail.Authenticator() {
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(username, password);
            }
        });
    }

    private class SendMailTask extends AsyncTask<Message, Void, Void> {
        private ProgressDialog progressDialog;

        @Override
        protected void onPreExecute() {
            super.onPreExecute();
            progressDialog = ProgressDialog.show(FeedbackActivity.this, "Please wait", "Sending feedback", true, false);
        }

        @Override
        protected void onPostExecute(Void aVoid) {
            super.onPostExecute(aVoid);
            progressDialog.dismiss();
        }

        @Override
        protected Void doInBackground(Message... messages) {
            try {
                Transport.send(messages[0]);
            } catch (MessagingException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
