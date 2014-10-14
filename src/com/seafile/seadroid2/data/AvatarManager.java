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
    private static final String AVATARS_URL = "api2/avatars/";
    private SeafConnection sc;
    private Account account;
    private List<Avatar> avatars;
    
    public AvatarManager(Account account) {
        this.account = account;
        this.sc = new SeafConnection(account);
    }

    public Avatar getAvatar(int size) throws SeafException {
        // First decide if use cache
        if (!Utils.isNetworkOn()) {
            throw SeafException.networkException;
        }
        String avatarRawData = sc.getAvatar(account.email, size);
        Log.v(DEBUG_TAG, "Get Avatar : " + avatarRawData);
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
