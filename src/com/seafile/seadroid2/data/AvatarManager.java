package com.seafile.seadroid2.data;

import java.util.List;

import org.json.JSONObject;

import android.util.Log;

import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

public class AvatarManager {
    private static final String DEBUG_TAG = "AvatarManager";
    private SeafConnection sc;
    private List<Account> accounts;
    private Account account;
    
    public AvatarManager(Account account) {
        this.account = account;
        this.sc = new SeafConnection(account);
    }
    
    public AvatarManager(List<Account> accounts) {
        this.accounts = accounts;
        this.sc = new SeafConnection(account);
    }

    public synchronized Avatar getAvatar(int size) throws SeafException {
        // First decide if use cache
        if (!Utils.isNetworkOn()) {
            throw SeafException.networkException;
        }
        Log.v(DEBUG_TAG, "request email : " + account.email);
        String avatarRawData = sc.getAvatar(account.email, size);
        Log.v(DEBUG_TAG, "response Avatar : " + avatarRawData);
        return parseAvatar(avatarRawData);
    }
    
    private Avatar parseAvatar(String json) {
        JSONObject obj = Utils.parseJsonObject(json);
        if (obj == null)
            return null;
        Avatar avatar = Avatar.fromJson(obj);
        if (avatar == null)
            return null;
        
        return avatar;
    }
    
}
