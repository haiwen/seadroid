package com.seafile.seadroid2.ui.dialog;

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
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.CertsManager;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SSLTrustManager;
import com.seafile.seadroid2.SSLTrustManager.SslFailureReason;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.CertificateInfo;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;


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
    // private TextView altSubjNamesText;
    private TextView sha1Text;
    private TextView md5Text;
    private TextView serialNumberText;
    private TextView notBeforeText;
    private TextView notAfterText;

    public SslConfirmDialog() {
    }

    public SslConfirmDialog(Account account, Listener listener) {
        this.listener = listener;
        this.account = account;
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        LinearLayout view = (LinearLayout)inflater.inflate(R.layout.dialog_ssl_confirm, null);

        messageText = (TextView)view.findViewById(R.id.message);
        commonNameText = (TextView) view.findViewById(R.id.common_name);
        // altSubjNamesText = (TextView) view.findViewById(R.id.alt_subj_name);
        sha1Text = (TextView) view.findViewById(R.id.sha1);
        md5Text = (TextView) view.findViewById(R.id.md5);
        serialNumberText = (TextView) view.findViewById(R.id.serial_number);
        notBeforeText = (TextView) view.findViewById(R.id.not_before);
        notAfterText = (TextView) view.findViewById(R.id.not_after);

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
            msg =getActivity().getString(R.string.ssl_confirm, host);
        } else {
            msg = getActivity().getString(R.string.ssl_confirm_cert_changed, host);
        }
        messageText.setText(msg);

        if (cert != null) {
            CertificateInfo certInfo = new CertificateInfo(cert);
            commonNameText.setText(certInfo.getSubjectName());
            // String[] subjAltNames = certInfo.getSubjectAltNames();
            // altSubjNamesText.setText((subjAltNames.length > 0) ? StringUtils.join(subjAltNames, ", ") : "—");
            sha1Text.setText(getActivity().getString(R.string.sha1, certInfo.getSignature("SHA-1")));
            md5Text.setText(getActivity().getString(R.string.md5, certInfo.getSignature("MD5")));
            serialNumberText.setText(getActivity().getString(R.string.serial_number, certInfo.getSerialNumber()));
            notBeforeText.setText(getActivity().getString(R.string.not_before, certInfo.getNotBefore().toLocaleString()));
            notAfterText.setText(getActivity().getString(R.string.not_after, certInfo.getNotAfter().toLocaleString()));
        } else {
            String not_available = getActivity().getString(R.string.not_available);
            commonNameText.setText(not_available);
            sha1Text.setText(not_available);
            md5Text.setText(not_available);
            serialNumberText.setText(not_available);
            notBeforeText.setText(not_available);
            notAfterText.setText(not_available);
        }

        builder.setPositiveButton(R.string.yes, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Log.d(DEBUG_TAG, "listener.onAccepted is called");
                listener.onAccepted(true);
            }
        });
        
        builder.setNegativeButton(R.string.no, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Log.d(DEBUG_TAG, "listener.onRejected is called");
                listener.onRejected();
            }
        });
        builder.setView(view);
        final AlertDialog dialog = builder.show();
        Button okButton = dialog.getButton(AlertDialog.BUTTON_POSITIVE);
        okButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, getResources().getDimension(R.dimen.dialog_btn_txt_size));
        Button cancelButton = dialog.getButton(AlertDialog.BUTTON_NEGATIVE);
        cancelButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, getResources().getDimension(R.dimen.dialog_btn_txt_size));

        return dialog;
    }

    @Override
    public void onCancel(DialogInterface dialog) {
        Log.d(DEBUG_TAG, "listener.onRejected is called");
        listener.onRejected();
    }
}
