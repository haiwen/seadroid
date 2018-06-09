//package com.seafile.seadroid2.util;
//
//import android.annotation.SuppressLint;
//import android.app.Dialog;
//import android.content.ContentUris;
//import android.content.ContentValues;
//import android.content.Context;
//import android.database.Cursor;
//import android.net.Uri;
//import android.os.Bundle;
//import android.provider.ContactsContract;
//import android.util.Log;
//import android.view.LayoutInflater;
//import android.view.View;
//import android.widget.TextView;
//import android.widget.Toast;
//
//import com.seafile.seadroid2.R;
//import com.seafile.seadroid2.SeadroidApplication;
//import com.seafile.seadroid2.SeafException;
//import com.seafile.seadroid2.SettingsManager;
//import com.seafile.seadroid2.account.Account;
//import com.seafile.seadroid2.account.AccountManager;
//import com.seafile.seadroid2.data.DataManager;
//import com.seafile.seadroid2.data.SeafDirent;
//import com.seafile.seadroid2.data.ContactsData;
//import com.seafile.seadroid2.ui.activity.SettingsActivity;
//import com.seafile.seadroid2.ui.dialog.TaskDialog;
//
//import java.io.BufferedReader;
//import java.io.File;
//import java.io.FileInputStream;
//import java.io.FileOutputStream;
//import java.io.InputStreamReader;
//import java.io.OutputStreamWriter;
//import java.util.ArrayList;
//import java.util.LinkedList;
//import java.util.List;
//
//import a_vcard.android.provider.Contacts;
//import a_vcard.android.syncml.pim.VDataBuilder;
//import a_vcard.android.syncml.pim.VNode;
//import a_vcard.android.syncml.pim.vcard.ContactStruct;
//import a_vcard.android.syncml.pim.vcard.VCardComposer;
//import a_vcard.android.syncml.pim.vcard.VCardException;
//import a_vcard.android.syncml.pim.vcard.VCardParser;
//
//
//
///**
// * Function:
// * Author:      Saud
// * Create:      2016/11/12
// * Modtime:     2016/11/12
// */
//@SuppressLint("ValidFragment")
//public class ContactsDialog extends TaskDialog {
//    private static final String STATE_TASK_CONTACTS_TYPE = "state_task_contacts_type";
//    public static final int CONTACTS_BACKUP = 1;
//    public static final int CONTACTS_RECOVERY = 2;
//    private int type;
//
//    private SettingsActivity mContext;
//
//    public ContactsDialog() {
//    }
//
//    public ContactsDialog(Context context, int type) {
//        if (context instanceof SettingsActivity) {
//            this.mContext = (SettingsActivity) context;
//            this.type = type;
//            isProgressHorizontal = true;
//        }
//    }
//
//    @Override
//    public void onTaskFailed(SeafException e) {
//        super.onTaskFailed(e);
//    }
//
//
//    @Override
//    public void onTaskSuccess() {
//        if (type == CONTACTS_BACKUP) {
//            Toast.makeText(mContext, getString(R.string.contacts_backup_success), Toast.LENGTH_LONG).show();
//        } else if (type == CONTACTS_RECOVERY) {
//            Toast.makeText(mContext, getString(R.string.contacts_recovery_success), Toast.LENGTH_LONG).show();
//        }
//        super.onTaskSuccess();
//    }
//
//
//    @Override
//    protected void onDialogCreated(Dialog dialog) {
//        if (type == CONTACTS_BACKUP) {
//            dialog.setTitle(R.string.contacts_backup_title);
//        } else if (type == CONTACTS_RECOVERY) {
//            dialog.setTitle(R.string.contacts_recovery_title);
//        }
//
//    }
//
//    @Override
//    protected void onSaveDialogContentState(Bundle outState) {
//        outState.putInt(STATE_TASK_CONTACTS_TYPE, type);
//    }
//
//    @Override
//    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
//        View view = inflater.inflate(R.layout.dialog_contacts, null);
//        TextView countText = (TextView) view.findViewById(R.id.setting_contacts_backup);
//        String fileName = "";
//        if (type == CONTACTS_BACKUP) {
//          fileName = getString(R.string.contacts_dialog_backup) + " " +
//                     ContactManager.getContactsBackupFileName();
//        } else if (type == CONTACTS_RECOVERY) {
//          fileName = getString(R.string.contacts_dialog_recovery);
//        }
//        countText.setText(fileName);
//        return view;
//    }
//
//    @Override
//    protected Task prepareTask() {
//        ContactManager contactManager = new ContactManager(mContext, type);
//        return contactManager;
//    }
//
//}
//
//
//class ContactManager extends TaskDialog.Task {
//    private static final String DEBUG_TAG = "ContactManager";
//    private final int type;
//    private SettingsActivity mContext;
//    private List<Account> mAccounts;
//    private String mContactsPath;
//    private long mMtime = 1;
//    private SeafDirent seafDirent;
//    private String mPath;
//
//    public ContactManager(Context context, int type) {
//        this.type = type;
//        if (context instanceof SettingsActivity) {
//            mContext = (SettingsActivity) context;
//        }
//    }
//
//
//    @Override
//    protected void runTask() {
//        if (type == ContactsDialog.CONTACTS_BACKUP) {
//            List<ContactsData> contactInfo = null;
//            try {
//                contactInfo = getContactInfo(mContext);
//                Log.d(DEBUG_TAG, "contacts  size  :" + contactInfo.size());
//                backupContacts(contactInfo);
//                //send  Broadcoast  to  upload
//                mContext.uploadContacts(mContactsPath);
//            } catch (SeafException e) {
//                setTaskException(e);
//                e.printStackTrace();
//            }
//        } else if (type == ContactsDialog.CONTACTS_RECOVERY) {
//            try {
//                List<ContactsData> infoList = restoreContacts();
//                for (int i = 0; i < infoList.size(); i++) {
//                    addContacts(mContext, infoList.get(i));
//                    progress(100 * i / infoList.size());
//                }
//            } catch (SeafException e) {
//                setTaskException(e);
//                e.printStackTrace();
//            }
//        }
//    }
//
//
//    /**
//     * read  contacts
//     *
//     * @param context
//     * @return
//     */
//    public List<ContactsData> getContactInfo(Context context) throws SeafException {
//
//        try {
//            List<ContactsData> infoList = new LinkedList<>();
//            Cursor cur = context.getContentResolver().query(ContactsContract.Contacts.CONTENT_URI, null, null, null, null);
//            if (cur != null && cur.moveToFirst()) {
//                do {
//                    ContactsData contactsData = new ContactsData();
//                    String id = cur.getString(cur.getColumnIndex(ContactsContract.Contacts._ID));
//                    String displayName = cur.getString(cur.getColumnIndex(ContactsContract.Contacts.DISPLAY_NAME));
//                    if (displayName == null || "".equals(displayName)) {
//                        //   vcard  libs  nonsupport  no name , so wo  used  '*'  replace   name
//                        displayName = "*";
//                    }
//                    contactsData.setName(displayName);
//                    contactsData.setUserid(id);
//                    //read contacts phone
//                    Cursor phonesCursor = context.getContentResolver().query(ContactsContract.CommonDataKinds.Phone.CONTENT_URI, null,
//                            ContactsContract.CommonDataKinds.Phone.CONTACT_ID + "=" + id, null, null);
//                    if (phonesCursor != null) {
//                        LinkedList<ContactsData.PhoneInfo> phoneInfos = new LinkedList<>();
//                        if (phonesCursor.moveToFirst()) {
//                            do {
//                                ContactsData.PhoneInfo phoneInfo = new ContactsData.PhoneInfo();
//                                String phoneNumber = phonesCursor.getString(phonesCursor.getColumnIndex(ContactsContract.CommonDataKinds
//                                        .Phone.NUMBER));
//                                int type = phonesCursor.getInt(phonesCursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone
//                                        .TYPE));
//                                phoneInfo.setNumber(phoneNumber);
//                                phoneInfo.setType(type);
//                                phoneInfos.add(phoneInfo);
//                            } while (phonesCursor.moveToNext());
//                        }
//                        phonesCursor.close();
//                        contactsData.setPhoneList(phoneInfos);
//                    }
//
//                    //read  contacts  email
//                    Cursor emailCur = context.getContentResolver().query(ContactsContract.CommonDataKinds.Email.CONTENT_URI, null,
//                            ContactsContract.CommonDataKinds.Email.CONTACT_ID + "=" + id, null, null);
//                    if (emailCur != null) {
//                        LinkedList<ContactsData.EmailInfo> emailInfos = new LinkedList<>();
//                        if (emailCur.moveToFirst()) {
//                            do {
//                                ContactsData.EmailInfo emailInfo = new ContactsData.EmailInfo();
//                                String email = emailCur.getString(emailCur.getColumnIndex(ContactsContract.CommonDataKinds.Email
//                                        .DATA1));
//                                int type = emailCur.getInt(emailCur.getColumnIndex(ContactsContract.CommonDataKinds.Email.TYPE));
//                                emailInfo.setEmail(email);
//                                emailInfo.setType(type);
//                                emailInfos.add(emailInfo);
//                            } while (emailCur.moveToNext());
//                        }
//                        emailCur.close();
//                        contactsData.setEmail(emailInfos);
//                    }
//                    infoList.add(contactsData);
//                    int progress = cur.getPosition() * 100 / cur.getCount();
//                    progress(progress);
//                } while (cur.moveToNext());
//                cur.close();
//            }
//            return infoList;
//        } catch (Exception e) {
//            throw new SeafException(0, mContext.getString(R.string.contacts_backup_fail));
//        }
//    }
//
//    private static final String CONTACTS_FILE_NAME = "contacts-%1s.vcf";
//
//    static String getContactsBackupFileName() {
//        return String.format(CONTACTS_FILE_NAME, Utils.translateTime());
//    }
//
//    /**
//     * write  contacts to  SD card
//     */
//    public void backupContacts(List<ContactsData> infos) throws SeafException {
//
//        if (infos == null || infos.size() == 0) {
//            throw new SeafException(0, mContext.getString(R.string.contacts_count_zero));
//        }
//
//        try {
//            mAccounts = new AccountManager(SeadroidApplication.getAppContext()).getAccountList();
//            String path = new DataManager(mAccounts.get(0)).getAccountDir() + "/temp/";
//            File fileDir = new File(path);
//            if (!fileDir.exists()) {
//                fileDir.mkdirs();
//            }
//            mContactsPath = fileDir.toString() + "/" + getContactsBackupFileName();
//            OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(mContactsPath), "UTF-8");
//            VCardComposer composer = new VCardComposer();
//            for (ContactsData contactsData : infos) {
//                ContactStruct contact = new ContactStruct();
//                contact.name = contactsData.getName();
//                List<ContactsData.PhoneInfo> phoneList = contactsData.getPhoneList();
//                if (phoneList != null && phoneList.size() > 0) {
//                    for (ContactsData.PhoneInfo phoneInfo : phoneList) {
//                        contact.addPhone(phoneInfo.getType(), phoneInfo.getNumber(), "", true);
//                    }
//                }
//
//                List<ContactsData.EmailInfo> emailList = contactsData.getEmail();
//                if (emailList != null && emailList.size() > 0) {
//                    for (ContactsData.EmailInfo emailInfo : emailList) {
//                        contact.addContactmethod(Contacts.KIND_EMAIL, emailInfo.getType(), emailInfo.getEmail(), null, true);
//                    }
//                }
//                String vcardString = composer.createVCard(contact, VCardComposer.VERSION_VCARD30_INT);
//                writer.write(vcardString);
//                writer.write("\n");
//                writer.flush();
//            }
//            writer.close();
//
//        } catch (Exception e) {
//            e.printStackTrace();
//            throw new SeafException(0, mContext.getString(R.string.contacts_backup_fail));
//        }
//    }
//
//
//    /**
//     * get contacts  from vCard
//     *
//     * @return
//     */
//    public List<ContactsData> restoreContacts() throws SeafException {
//        try {
//
//            List<Account> accounts = new AccountManager(SeadroidApplication.getAppContext()).getAccountList();
//            DataManager dataManager = new DataManager(accounts.get(0));
//            SettingsManager settingsManager = SettingsManager.instance();
//            String repoId = settingsManager.getContactsUploadRepoId();
//            String repoName = settingsManager.getContactsUploadRepoName();
//            List<SeafDirent> dirents = dataManager.getCachedDirents(repoId, "/");
//            if (dirents == null) {
//                dirents = dataManager.getDirentsFromServer(repoId, "/");
//            }
//            if (dirents != null) {
//                for (int i = 0; i < dirents.size(); i++) {
//                    SeafDirent seafDirent = dirents.get(i);
//                    if (seafDirent.isDir() && SettingsActivity.BASE_DIR.equals(seafDirent.getTitle())) {
//                        mPath = Utils.pathJoin("/", seafDirent.getTitle());
//                        List<SeafDirent> childDirents = dataManager.getCachedDirents(repoId, mPath);
//                        if (childDirents != null) {
//                            for (int j = 0; j < childDirents.size(); j++) {
//                                SeafDirent childDirent = childDirents.get(j);
//                                if (!childDirent.isDir()) {
//                                    String title = childDirent.getTitle();
//                                    if (title.indexOf("contacts") != -1) {
//                                        if (childDirent.mtime > mMtime) {
//                                            mMtime = childDirent.mtime;
//                                            this.seafDirent = childDirent;
//                                        }
//                                    }
//                                }
//                            }
//                        }
//                    }
//                }
//            }
//            final String filePath = Utils.pathJoin(mPath, seafDirent.name);
//            File localFile = dataManager.getLocalCachedFile(repoName, repoId, filePath, seafDirent.id);
//            if (localFile == null) {
//                mContext.txService.addDownloadTask(accounts.get(0), repoName, repoId, filePath, seafDirent.size);
//                localFile = dataManager.getLocalCachedFile(repoName, repoId, filePath, seafDirent.id);
//            }
//            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(localFile), "UTF-8"));
//            String vcardString = "";
//            String line;
//            while ((line = reader.readLine()) != null) {
//                vcardString += line + "\n";
//            }
//            reader.close();
//            VCardParser parse = new VCardParser();
//            VDataBuilder builder = new VDataBuilder();
//            boolean parsed = parse.parse(vcardString, "UTF-8", builder);
//            if (!parsed) {
//                throw new VCardException("Could not parse vCard file:" + R.drawable.file);
//            }
//            List<VNode> pimContacts = builder.vNodeList;
//            List<ContactsData> contactInfoList = new ArrayList<>();
//            for (VNode contact : pimContacts) {
//                ContactStruct contactStruct = ContactStruct.constructContactFromVNode(contact, 1);
//                ContactsData contactsData = new ContactsData();
//                contactsData.setName(contactStruct.name);
//
//                // get phone numb
//                List<ContactStruct.PhoneData> phoneDataList = contactStruct.phoneList;
//                if (phoneDataList != null && phoneDataList.size() > 0) {
//                    LinkedList<ContactsData.PhoneInfo> phoneInfos = new LinkedList<>();
//                    for (ContactStruct.PhoneData phoneDate : phoneDataList) {
//                        ContactsData.PhoneInfo phoneInfo = new ContactsData.PhoneInfo();
//                        phoneInfo.setType(phoneDate.type);
//                        phoneInfo.setNumber(phoneDate.data);
//                        phoneInfos.add(phoneInfo);
//                    }
//                    contactsData.setPhoneList(phoneInfos);
//                }
//                //get email
//                List<ContactStruct.ContactMethod> emailList = contactStruct.contactmethodList;
//                if (emailList != null && emailList.size() > 0) {
//                    LinkedList<ContactsData.EmailInfo> emailInfos = new LinkedList<>();
//                    for (ContactStruct.ContactMethod contactMethod : emailList) {
//                        if (Contacts.KIND_EMAIL == contactMethod.kind) {
//                            ContactsData.EmailInfo emailInfo = new ContactsData.EmailInfo();
//                            emailInfo.setType(contactMethod.type);
//                            emailInfo.setEmail(contactMethod.data);
//                            emailInfos.add(emailInfo);
//                        }
//                    }
//                    contactsData.setEmail(emailInfos);
//                }
//                contactInfoList.add(contactsData);
//            }
//            return contactInfoList;
//        } catch (Exception e) {
//            e.printStackTrace();
//            throw new SeafException(0, mContext.getString(R.string.contacts_recover_fail));
//        }
//    }
//
//    /**
//     * add contacts to  phone
//     *
//     * @param context
//     * @param info
//     * @throws SeafException
//     */
//    public void addContacts(Context context, ContactsData info) throws SeafException {
//
//        try {
//            ContentValues values = new ContentValues();
//            Uri rawContactUri = context.getContentResolver().insert(ContactsContract.RawContacts.CONTENT_URI, values);
//            long rawContactId = ContentUris.parseId(rawContactUri);
//            values.clear();
//            values.put(ContactsContract.RawContacts.Data.RAW_CONTACT_ID, rawContactId);
//            values.put(ContactsContract.RawContacts.Data.MIMETYPE, ContactsContract.CommonDataKinds.StructuredName.CONTENT_ITEM_TYPE);
//            values.put(ContactsContract.CommonDataKinds.StructuredName.GIVEN_NAME, info.getName());
//            context.getContentResolver().insert(ContactsContract.Data.CONTENT_URI, values);
//
//            List<ContactsData.PhoneInfo> phoneList = info.getPhoneList();
//            for (ContactsData.PhoneInfo phoneInfo : phoneList) {
//                values.clear();
//                values.put(ContactsContract.Contacts.Data.RAW_CONTACT_ID, rawContactId);
//                values.put(ContactsContract.RawContacts.Data.MIMETYPE, ContactsContract.CommonDataKinds.Phone.CONTENT_ITEM_TYPE);
//                values.put(ContactsContract.CommonDataKinds.Phone.NUMBER, phoneInfo.getNumber());
//                values.put(ContactsContract.CommonDataKinds.Phone.TYPE, phoneInfo.getType());
//                context.getContentResolver().insert(ContactsContract.Data.CONTENT_URI, values);
//            }
//
//            List<ContactsData.EmailInfo> emailList = info.getEmail();
//            for (ContactsData.EmailInfo email : emailList) {
//                values.clear();
//                values.put(ContactsContract.Contacts.Data.RAW_CONTACT_ID, rawContactId);
//                values.put(ContactsContract.RawContacts.Data.MIMETYPE, ContactsContract.CommonDataKinds.Email.CONTENT_ITEM_TYPE);
//                values.put(ContactsContract.CommonDataKinds.Email.DATA, email.getEmail());
//                values.put(ContactsContract.CommonDataKinds.Email.TYPE, email.getType());
//                context.getContentResolver().insert(ContactsContract.Data.CONTENT_URI, values);
//            }
//        } catch (Exception e) {
//            e.printStackTrace();
//            throw new SeafException(0, mContext.getString(R.string.contacts_recover_fail));
//        }
//    }
//}
