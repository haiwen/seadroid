package com.seafile.seadroid2.avatar;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONObject;

import android.util.Log;

import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

public class AvatarManager {
    private static final String DEBUG_TAG = "AvatarManager";
    
    private SeafConnection httpConnection;
    private static List<Avatar> avatars;
    private List<Account> accounts;
    private final AvatarDBHelper dbHelper = AvatarDBHelper.getAvatarDbHelper();
    
    public AvatarManager(List<Account> accounts) {
        this.accounts = accounts;
        avatars = new ArrayList<Avatar>();
    }
        
    public synchronized void getAvatars(int size) throws SeafException {
        // First decide if use cache
        avatars = getAvatarList();
        
        List<Account> accountsWithoutAvatars = new ArrayList<Account>();
        
        for (Account account : accounts) {
            for (Avatar avatar : avatars) {
                if (!avatar.getSignature().equals(account.getSignature())) {
                    accountsWithoutAvatars.add(account);
                }
            }
        }

        if (!Utils.isNetworkOn()) {
            throw SeafException.networkException;
        }
        
        // already loaded avatars
        if (avatars.size() == accounts.size()) {
            return;
        } else if (avatars.isEmpty()) { // initialization
            for (Account account : accounts) {
                httpConnection = new SeafConnection(account);
                String avatarRawData = httpConnection.getAvatar(
                        account.getEmail(), size);
                Avatar avatar = parseAvatar(avatarRawData);
                avatar.setSignature(account.getSignature());
                avatars.add(avatar);
            }
        } else { // load avatars for new added account   
            for (Account account : accountsWithoutAvatars) {
                httpConnection = new SeafConnection(account);
                String avatarRawData = httpConnection.getAvatar(
                        account.getEmail(), size);
                Avatar avatar = parseAvatar(avatarRawData);
                avatar.setSignature(account.getSignature());
                avatars.add(avatar);
            }
        }
        
        saveAvatarList(avatars);
    }
    
    private List<Avatar> getAvatarList() {
        return dbHelper.getAvatarList();
    }

    private void saveAvatarList(List<Avatar> avatars) {
        dbHelper.saveAvatars(avatars);
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
        /*if (!avatars.contains(account)) {
            return null;
        }*/
        // Log.d(DEBUG_TAG, "avatar url " + avatars.get(avatars.indexOf(account)).getUrl());
        for (Avatar avatar : avatars) {
            if (avatar.getSignature().equals(account.getSignature())) {
                return avatar.getUrl();
            }
        }
        
        return null;
    }
    
}
