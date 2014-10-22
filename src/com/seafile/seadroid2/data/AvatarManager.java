package com.seafile.seadroid2.data;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONObject;

import android.util.Log;

import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

public class AvatarManager {
    private static final String DEBUG_TAG = "AvatarManager";
    
    private SeafConnection httpConnection;
    private static Map<String, Avatar> avatars;
    private List<Account> accounts;
    
    public AvatarManager(List<Account> accounts) {
        this.accounts = accounts;
        avatars = new HashMap<String, Avatar>();
    }
        
    public synchronized void getAvatars(int size) throws SeafException {
        // First decide if use cache
        if (!Utils.isNetworkOn()) {
            throw SeafException.networkException;
        }
        
        for (Account account : accounts) {
            httpConnection = new SeafConnection(account);
            String avatarRawData = httpConnection.getAvatar(account.getEmail(), size);
            avatars.put(account.getSignature(), parseAvatar(avatarRawData));
            Log.d(DEBUG_TAG, "add " + account.getEmail());
        }
        
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

    public static String getAvatarUrl(Account account) {
        Log.d(DEBUG_TAG, "getAvatar url");
        if (avatars == null) {
            return null;
        }
        if (!avatars.containsKey(account.getSignature())) {
            return null;
        }
        Log.d(DEBUG_TAG, "avatar url " + avatars.get(account.getSignature()).getUrl());
        return avatars.get(account.getSignature()).getUrl();
    }
    
}
