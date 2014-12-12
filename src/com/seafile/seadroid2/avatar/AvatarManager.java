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
        this.avatarMgr = new HashMap<String, Avatar>();
    }

    public void setAccounts(List<Account> accounts) {
        this.accounts = accounts;
    }

    /**
     * get accounts signature who don`t have avatars yet
     *
     * @return account signature
     */
    private ArrayList<String> getActSignatures() {

        if (accounts == null) return null;

        // first put all keys (account signature) to avatarMgr, leave values to null
        for (Account act : accounts) {
            avatarMgr.put(act.getSignature(), null);
        }

        // second get avatars from database, in order to use cache
        avatars = getAvatarList();

        // third binding account signature with avatar (if has) in avatarMgr
        for (Avatar avatar : avatars) {
            if (avatarMgr.containsKey(avatar.getSignature())) {
                avatarMgr.put(avatar.getSignature(), avatar);
            }
        }

        // fourth get account signature who don`t have avatar yet
        ArrayList<String> actSignature = Lists.newArrayList();
        Iterator<Map.Entry<String, Avatar>> iterator = avatarMgr.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, Avatar> pairs = iterator.next();
            if (pairs.getValue() == null) {
                String signature = pairs.getKey();
                // use account signature to mark accounts who don`t have avatars
                actSignature.add(signature);
            }
            iterator.remove();
        }

        return actSignature;
    }

    /**
     * get account list who don`t have avatars yet
     *
     * @param signatures
     * @return
     */
    private List<Account> getActsBySignature(ArrayList<String> signatures) {

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


    private synchronized void loadAvatarsForAccounts(int size, Handler handler) throws SeafException {

        if (!Utils.isNetworkOn()) {
            throw SeafException.networkException;
        }

        ArrayList<String> signatures = getActSignatures();

        // contains accounts who don`t have avatars yet
        List<Account> acts = getActsBySignature(signatures);

        // contains new avatars in order to persist them to database
        List<Avatar> newAvatars = new ArrayList<Avatar>(acts.size());

        // load avatars from server
        for (Account account : acts) {
            httpConnection = new SeafConnection(account);
            String avatarRawData = httpConnection.getAvatar(account.getEmail(), size);
            Avatar avatar = parseAvatar(avatarRawData);
            avatar.setSignature(account.getSignature());

            // handler will send the latest data to ui
            avatars.add(avatar);

            // save new added avatars to database
            newAvatars.add(avatar);
        }

        Message msg = new Message();
        msg.what = LOAD_AVATAR_SUCCESSFULLY;
        msg.obj = avatars;
        if (handler != null)
            handler.sendMessage(msg);

        // save avatars to database
        saveAvatarList(newAvatars);
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

        //ConcurrentAsyncTask.execute(task);
        new Thread(task).start();

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
