package com.seafile.seadroid2.ui;

import java.net.MalformedURLException;
import java.net.URL;
import java.security.cert.CertificateParsingException;
import java.security.cert.X509Certificate;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.text.Html;
import android.util.Log;
import android.view.LayoutInflater;
import android.widget.CheckBox;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.CertsManager;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SSLTrustManager;
import com.seafile.seadroid2.SSLTrustManager.SslFailureReason;
import com.seafile.seadroid2.account.Account;


public class SslConfirmDialog extends DialogFragment {
    public interface Listener {
        void onAccepted(boolean rememberChoice);
        void onRejected();
    }

    public static final String FRAGMENT_TAG = "SslConfirmDialog";
    public static final String DEBUG_TAG = "SslConfirmDialog";

    private Account account;
    private Listener listener;
    private TextView messageText;
    private TextView commonNameText;
    private TextView orgnizationText;
    private TextView stateText;
    private TextView countryText;
    private TextView versionText;
    private TextView serialNumberText;
    private TextView signatureAlgorithmText;
    private TextView notBeforeText;
    private TextView notAfterText;
    private CheckBox rememberChoiceCheckbox;

    public SslConfirmDialog() {
    }

    public SslConfirmDialog(Account account, Listener listener) {
        this.listener = listener;
        this.account = account;
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        LinearLayout view = (LinearLayout)inflater.inflate(R.layout.dialog_ssl_confirm, null);

        messageText = (TextView)view.findViewById(R.id.message);
        commonNameText = (TextView) view.findViewById(R.id.common_name);
        orgnizationText = (TextView) view.findViewById(R.id.orgnization);
        stateText = (TextView) view.findViewById(R.id.state);
        countryText = (TextView) view.findViewById(R.id.country);
        
        versionText = (TextView) view.findViewById(R.id.version);
        serialNumberText = (TextView) view.findViewById(R.id.serial_number);
        signatureAlgorithmText = (TextView) view.findViewById(R.id.signature_algorithm);
        notBeforeText = (TextView) view.findViewById(R.id.not_before);
        notAfterText = (TextView) view.findViewById(R.id.not_after);
        rememberChoiceCheckbox = (CheckBox)view.findViewById(R.id.remember_choice);

        String host = null;

        try {
            host = new URL(account.server).getHost();
        } catch (MalformedURLException e) {
            // ignore
        }

        SslFailureReason reason = SSLTrustManager.instance().getFailureReason(account);
        X509Certificate cert = null;
        try {
            cert = SSLTrustManager.instance().getCertificateInfo(account);
        } catch (CertificateParsingException e) {
            e.printStackTrace();
        }
        String msg = "";
        if (reason == SslFailureReason.CERT_NOT_TRUSTED) {
            Log.d(DEBUG_TAG, cert.getIssuerDN().getName());
            Log.d(DEBUG_TAG, cert.getType());
            Log.d(DEBUG_TAG, String.valueOf(cert.getVersion()));
            
            msg =getActivity().getString(R.string.ssl_confirm, host);
            Log.d(DEBUG_TAG, "formated msg is " + msg);
        } else {
            msg = getActivity().getString(R.string.ssl_confirm_cert_changed, host);
            Log.d(DEBUG_TAG, "formated msg is " + msg);
        }
        messageText.setText(msg);
        commonNameText.setText("Common Name " + cert.getIssuerX500Principal().getName().toString());
        orgnizationText.setText("Orgnization " + cert.getIssuerDN().getName().substring(cert.getIssuerDN().getName().indexOf("O="), cert.getIssuerDN().getName().indexOf(",ST")));
        stateText.setText("State ");
        countryText.setText("Country ");
        versionText.setText("Version " + String.valueOf(cert.getVersion()));
        serialNumberText.setText("Serial Number " + cert.getSerialNumber().toString());
        notBeforeText.setText("Not before " + cert.getNotBefore().toGMTString());
        notAfterText.setText("Not After " + cert.getNotAfter().toLocaleString());
        builder.setTitle(R.string.ssl_confirm_title);
        builder.setView(view);

        builder.setPositiveButton(R.string.yes, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Log.d(DEBUG_TAG, "listener.onAccepted is called");
                listener.onAccepted(rememberChoiceCheckbox.isChecked());
                Log.d(DEBUG_TAG, "checbox is " + rememberChoiceCheckbox.isChecked());
            }
        });
        builder.setNegativeButton(R.string.no, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Log.d(DEBUG_TAG, "listener.onRejected is called");
                listener.onRejected();
            }
        });

        Dialog dialog = builder.create();

        return dialog;
    }

    @Override
    public void onCancel(DialogInterface dialog) {
        Log.d(DEBUG_TAG, "listener.onRejected is called");
        listener.onRejected();
    }
}
