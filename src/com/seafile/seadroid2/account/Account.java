package com.seafile.seadroid2.account;

public class Account {
    
    // The full URL of the server, like 'http://gonggeng.org/seahub/' or 'http://gonggeng.org/'
    public String server;
    
    public String email;
    public String token;
    public String passwd;
    
    public Account() {
        
    }
    
    public Account(String server, String email) {
        this.server = server;
        this.email = email;
    }
    
    public Account(String server, String email, String passwd) {
        this.server = server;
        this.email = email;
        this.passwd = passwd;
    }
    
    public Account(String server, String email, String passwd, String token) {
        this.server = server;
        this.email = email;
        this.passwd = passwd;
        this.token = token;
    }
    
    public String getServerHost() {
        String s = server.substring(server.indexOf("://") + 3);
        return s.substring(0, s.indexOf('/'));
    }
    
    public String getEmail() {
        return email;
    }
    
    public String getServer() {
        return server;
    }
    
    public String getServerNoProtocol() {
        String result = server.substring(server.indexOf("://") + 3);
        if (result.endsWith("/"))
            result = result.substring(0, result.length() - 1);
        return result;
    }
    
    public boolean isHttps() {
        return server.startsWith("https");
    }

    
    @Override
    public int hashCode() {
        return server.hashCode() + email.hashCode();
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;
        
        Account a = (Account)obj;
        if (a.server == null || a.email == null)
            return false;
        
        return a.server.equals(this.server) && a.email.equals(this.email);
    }

    public String getSignature() {
        return email.substring(0, 4) + " " + hashCode();
    }

}