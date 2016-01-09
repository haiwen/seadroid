package com.seafile.seadroid2.data;

import java.security.MessageDigest;
import java.security.cert.CertificateParsingException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.security.auth.x500.X500Principal;

import android.util.Log;

import com.google.common.collect.Lists;

/*
 * 
 * reference {@link cadroid #https://github.com/bitfireAT/cadroid}
 * 
 * PKIX x509 v3 (RFC 5280)
 * 2.5.29.19 - Basic Constraints
 * BasicConstraintsSyntax ::= SEQUENCE {
 *   cA  BOOLEAN DEFAULT FALSE,
 *   pathLenConstraint INTEGER (0..MAX) OPTIONAL
 *}
*/

public class CertificateInfo {
    private static final String DEBUG_TAG = "CertificateInfo";

    X509Certificate certificate;

    public CertificateInfo(X509Certificate certificate) {
        this.certificate = certificate;
    }

    // general info

    public String getSubjectName() {
        return certificate.getSubjectX500Principal().getName(X500Principal.RFC1779);
    }

    public String[] getSubjectAltNames() {
        try {
            LinkedList<String> altNames = Lists.newLinkedList();
            if (certificate.getSubjectAlternativeNames() != null)
                for (List<?> asn1Name : certificate.getSubjectAlternativeNames()) {
                    int type = (Integer) asn1Name.get(0);
                    String value;
                    try {
                        value = (String) asn1Name.get(1);
                    } catch (Exception e) {
                        value = "?";
                        Log.w(DEBUG_TAG, "Couldn't cast alternative subject name to String", e);
                    }
                    altNames.add(value + " [" + type + "]");
                }
            return altNames.toArray(new String[0]);
        } catch (CertificateParsingException e) {
            Log.w(DEBUG_TAG, "Couldn't parse Subject Alternative Names from certificate", e);
            return null;
        }
    }

    public String getSerialNumber() {
        return certificate.getSerialNumber().toString(16);
    }

    public String getSignature(String algorithm) {
        try {
            MessageDigest digest = MessageDigest.getInstance(algorithm);
            digest.update(certificate.getEncoded());
            String sig = "";
            for (byte b : digest.digest())
                sig += Integer.toHexString(b & 0xFF);
            return sig;
        } catch (Exception e) {
            Log.e(DEBUG_TAG, "Couldn't calculate certificate digest", e);
            return e.getMessage();
        }
    }

    public Date getNotBefore() {
        return certificate.getNotBefore();
    }

    public Date getNotAfter() {
        return certificate.getNotAfter();
    }

    public boolean isCurrentlyValid() {
        try {
            certificate.checkValidity();
            return true;
        } catch (Exception e) {
            return false;
        }
    }
}