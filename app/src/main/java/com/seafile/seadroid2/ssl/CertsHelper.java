package com.seafile.seadroid2.ssl;

import android.util.Base64;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.security.cert.X509Certificate;

public class CertsHelper {

    public static X509Certificate convertToCert(String text) {
        X509Certificate cert = null;

        ByteArrayInputStream bis = null;
        ObjectInputStream ois = null;
        byte[] data = null;

        data = Base64.decode(text, Base64.DEFAULT);

        try {
            bis = new ByteArrayInputStream(data);
            ois = new ObjectInputStream(bis);
            cert = (X509Certificate) ois.readObject();
            return cert;
        } catch (ClassNotFoundException e) {
            return null;
        } catch (IOException e) {
            return null;
        } finally {
            if (bis != null) {
                try {
                    bis.close();
                } catch (IOException e) {
                }
            }

            if (ois != null) {
                try {
                    ois.close();
                } catch (IOException e) {
                }
            }
        }
    }


    public static String getCertBase64(X509Certificate cert) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ObjectOutputStream out = null;
        String text;
        try {
            out = new ObjectOutputStream(bos);
            out.writeObject(cert);
            byte[] data = bos.toByteArray();
            text = Base64.encodeToString(data, Base64.DEFAULT);
        } catch (IOException e) {
            return null;
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException ex) {
                // ignore close exception
            }
            try {
                bos.close();
            } catch (IOException ex) {
                // ignore close exception
            }
        }

        return text;
    }
}
