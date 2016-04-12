package com.seafile.seadroid2.data;

public class EncryptResult {
    private String cipherText;
    private String initVector;

    public EncryptResult() {
    }

    public EncryptResult(final String cipherText, final String initVector) {
        super();
        this.cipherText = cipherText;
        this.initVector = initVector;
    }

    public String getCipherText() {
        return cipherText;
    }

    public void setCipherText(final String cipherText) {
        this.cipherText = cipherText;
    }

    public String getInitVector() {
        return initVector;
    }

    public void setInitVector(final String initVector) {
        this.initVector = initVector;
    }
}
