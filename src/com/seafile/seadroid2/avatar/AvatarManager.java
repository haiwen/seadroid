package com.seafile.seadroid2.avatar;

import java.util.*;

import org.json.JSONObject;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

/**
 * load, cache, update avatars
 *
 */
public class AvatarManager {
    private static final String DEBUG_TAG = "AvatarManager";



    private List<Account> accounts;
    private final AvatarDBHelper dbHelper = AvatarDBHelper.getAvatarDbHelper();
    private HashMap<String, Avatar> avatarMgr;
    private List<Avatar> avatars;

    public AvatarManager() {
        // this.accounts = accounts;
        this.avatars = Lists.newArrayList();
        this.avatarMgr = new HashMap<String, Avatar>();
    }

    public void setAccounts(List<Account> accounts) {
        this.accounts = accounts;
    }

    /**
     * get signature of which account doesn`t have avatar yet
     *
     * @return account signature
     */
    public ArrayList<String> getActSignatures() {

        if (accounts == null) return null;

        // get avatars from database, in order to use cache
        avatars = getAvatarList();

        // TODO check if avatar was changed by sending request to server, substitute local cache if changed.

        ArrayList<String> actSignature = Lists.newArrayList();
        for (Account account : accounts) {

            actSignature.add(account.getSignature());
            for (Avatar avatar : avatars) {
                if (account.getSignature().equals(avatar.getSignature())) {
                    actSignature.remove(account.getSignature());
                    break;
                }
            }
        }
        return actSignature;
    }

    /**
     * get account list who don`t have avatars yet
     *
     * @param signatures
     * @return
     */
    public List<Account> getActsBySignature(ArrayList<String> signatures) {

        if (signatures == null)
            return null;

        ArrayList<Account> actList = Lists.newArrayList();
        for (String signature : signatures) {

            for (Account account : accounts) {

                if (account.getSignature().equals(signature))
                    actList.add(account);
            }
        }
        return actList;
    }

    public List<Avatar> getAvatarList() {
        return dbHelper.getAvatarList();
    }

    public void saveAvatarList(List<Avatar> avatars) {
        dbHelper.saveAvatars(avatars);
    }

    public Avatar parseAvatar(String json) {
        if (json == null) return null;

        JSONObject obj = Utils.parseJsonObject(json);
        if (obj == null)
            return null;
        Avatar avatar = Avatar.fromJson(obj);
        if (avatar == null)
            return null;
        
        return avatar;
    }




}
