package com.seafile.seadroid2.avatar;

import java.util.*;

import android.os.Handler;
import android.os.Message;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import org.json.JSONObject;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

/**
 * load, cache, update avatars
 *
 */
public class AvatarManager {
    private static final String DEBUG_TAG = "AvatarManager";

    public static final int LOAD_AVATAR_FAILED = 0;
    public static final int LOAD_AVATAR_SUCCESSFULLY = 1;

    private SeafConnection httpConnection;
    private List<Avatar> avatars;
    private List<Account> accounts;
    private final AvatarDBHelper dbHelper = AvatarDBHelper.getAvatarDbHelper();
    private HashMap<String, Avatar> avatarMgr;

    public AvatarManager() {
        // this.accounts = accounts;
        this.avatars = Lists.newArrayList();
        avatarMgr = new HashMap<String, Avatar>();
    }

    public void setAccounts(List<Account> accounts) {
        this.accounts = accounts;
    }

    private ArrayList<String> getActSignatureWithoutAvatars() {
        // use account signature to mark accounts without avatars
        ArrayList<String> actSignature = Lists.newArrayList();

        if (accounts == null) return null;

        for (Account act : accounts) {
            avatarMgr.put(act.getSignature(), null);
        }

        avatars = getAvatarList();

        for (Avatar avatar : avatars) {
            if (avatarMgr.containsKey(avatar.getSignature())) {
                avatarMgr.put(avatar.getSignature(), avatar);
            }
        }

        Iterator<Map.Entry<String, Avatar>> iterator = avatarMgr.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, Avatar> pairs = iterator.next();
            if (pairs.getValue() == null) {
                String signature = pairs.getKey();
                actSignature.add(signature);
            }
            iterator.remove();
        }

        return actSignature;
    }

    private List<Account> getAccountsWithoutAvatars(ArrayList<String> signatures) {
        ArrayList<Account> actList = Lists.newArrayList();
        if (signatures == null)
            return null;
        for (String signature : signatures) {

            for (Account account : accounts) {

                if (account.getSignature().equals(signature))
                    actList.add(account);
            }
        }
        return actList;
    }


    public synchronized void loadAvatarsForAccounts(int size, Handler handler) throws SeafException {
        if (!Utils.isNetworkOn()) {
            throw SeafException.networkException;
        }

        ArrayList<String> signatures = getActSignatureWithoutAvatars();
        List<Account> acts = getAccountsWithoutAvatars(signatures);

        for (Account account : acts) {
            httpConnection = new SeafConnection(account);
            String avatarRawData = httpConnection.getAvatar(account.getEmail(), size);
            Avatar avatar = parseAvatar(avatarRawData);
            avatar.setSignature(account.getSignature());
            avatars.add(avatar);
        }

        Message msg = new Message();
        msg.what = LOAD_AVATAR_SUCCESSFULLY;
        msg.obj = avatars;
        handler.sendMessage(msg);

        // save avatars to database
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

    public void loadAvatars(Handler handler) {
        // set avatar size to 48*48
        LoadAvatarTask task = new LoadAvatarTask(48, handler);

        ConcurrentAsyncTask.execute(task);

    }

    /*
     * load avatars from server and use handler to notify UI
     */
    private class LoadAvatarTask implements Runnable {

        private int avatarSize;
        private Handler handler;

        /**
         *
         * @param avatarSize which size to download
         * @param handler
         */
        public LoadAvatarTask(int avatarSize, Handler handler) {
            this.avatarSize = avatarSize;
            this.handler = handler;
        }

        @Override
        public void run() {
            try {
                loadAvatarsForAccounts(avatarSize, handler);
            } catch (SeafException e) {
                e.printStackTrace();
            }
        }
    }

}
