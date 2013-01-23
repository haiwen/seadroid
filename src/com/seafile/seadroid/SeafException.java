package com.seafile.seadroid;

public class SeafException extends Exception {
    
    private static final long serialVersionUID = 1L;

    static public final int OTHER_EXCEPTION = 599; 
    
    private int code;
    
    public SeafException(int code, String msg) {
        super(msg);
        this.code = code;
    }
    
    public int getCode() {
        return code;
    }
    
}
