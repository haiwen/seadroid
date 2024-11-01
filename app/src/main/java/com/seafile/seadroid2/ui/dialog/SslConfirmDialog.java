package com.seafile.seadroid2.ui.dialog;

import android.annotation.SuppressLint;

import androidx.appcompat.app.AlertDialog;

import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;

import androidx.fragment.app.DialogFragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.google.android.gms.common.internal.Objects;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.common.base.Preconditions;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.Collections2;
import com.google.common.collect.ComparisonChain;
import com.google.common.collect.Ordering;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.databinding.DialogSslConfirmBinding;
import com.seafile.seadroid2.framework.data.CertificateInfo;
import com.seafile.seadroid2.ssl.SSLTrustManager;
import com.seafile.seadroid2.ssl.SSLTrustManager.SslFailureReason;

import java.net.MalformedURLException;
import java.net.URL;
import java.security.cert.CertificateParsingException;
import java.security.cert.X509Certificate;


public class SslConfirmDialog extends DialogFragment {
    public interface Listener {
        void onAccepted(boolean rememberChoice);

        void onRejected();
    }

    public static final String FRAGMENT_TAG = "SslConfirmDialog";
    public static final String DEBUG_TAG = "SslConfirmDialog";

    private Account account;
    private Listener listener;
    private X509Certificate certificate;

    public SslConfirmDialog() {
    }

    @SuppressLint("ValidFragment")
    public SslConfirmDialog(Account account, Listener listener) {
        this.listener = listener;
        this.account = account;
    }

    @SuppressLint("ValidFragment")
    public SslConfirmDialog(Account account, X509Certificate certificate, Listener listener) {
        this.listener = listener;
        this.account = account;
        this.certificate = certificate;
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        DialogSslConfirmBinding binding = DialogSslConfirmBinding.inflate(getLayoutInflater());

        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(getActivity());
        builder.setTitle(getString(R.string.ssl_confirm_title));

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
            msg = getActivity().getString(R.string.ssl_confirm, host);
        } else {
            msg = getActivity().getString(R.string.ssl_confirm_cert_changed, host);
        }

        binding.message.setText(msg);

        if (cert != null) {
            CertificateInfo certInfo = new CertificateInfo(cert);
            binding.commonName.setText(certInfo.getSubjectName());
            // String[] subjAltNames = certInfo.getSubjectAltNames();
            // altSubjNamesText.setText((subjAltNames.length > 0) ? StringUtils.join(subjAltNames, ", ") : "—");
            binding.sha256.setText(getActivity().getString(R.string.sha256, certInfo.getSignature("SHA-256")));
            binding.sha1.setText(getActivity().getString(R.string.sha1, certInfo.getSignature("SHA-1")));
            binding.md5.setText(getActivity().getString(R.string.md5, certInfo.getSignature("MD5")));
            binding.serialNumber.setText(getActivity().getString(R.string.serial_number, certInfo.getSerialNumber()));
            binding.notBefore.setText(getActivity().getString(R.string.not_before, certInfo.getNotBefore().toLocaleString()));
            binding.notAfter.setText(getActivity().getString(R.string.not_after, certInfo.getNotAfter().toLocaleString()));
        } else if (certificate != null) {
            CertificateInfo certInfo = new CertificateInfo(certificate);
            binding.commonName.setText(certInfo.getSubjectName());
            binding.sha256.setText(getActivity().getString(R.string.sha256, certInfo.getSignature("SHA-256")));
            binding.sha1.setText(getActivity().getString(R.string.sha1, certInfo.getSignature("SHA-1")));
            binding.md5.setText(getActivity().getString(R.string.md5, certInfo.getSignature("MD5")));
            binding.serialNumber.setText(getActivity().getString(R.string.serial_number, certInfo.getSerialNumber()));
            binding.notBefore.setText(getActivity().getString(R.string.not_before, certInfo.getNotBefore().toLocaleString()));
            binding.notAfter.setText(getActivity().getString(R.string.not_after, certInfo.getNotAfter().toLocaleString()));
        } else {
            String not_available = getActivity().getString(R.string.not_available);
            binding.commonName.setText(not_available);
            binding.sha256.setText(not_available);
            binding.sha1.setText(not_available);
            binding.md5.setText(not_available);
            binding.serialNumber.setText(not_available);
            binding.notBefore.setText(not_available);
            binding.notAfter.setText(not_available);
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

        builder.setView(binding.getRoot());
        return builder.show();
    }

    @Override
    public void onCancel(DialogInterface dialog) {
        Log.d(DEBUG_TAG, "listener.onRejected is called");
        listener.onRejected();
    }
}
