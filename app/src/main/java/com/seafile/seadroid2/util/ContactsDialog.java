package com.seafile.seadroid2.util;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.ContentUris;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.provider.ContactsContract;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.UserData;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.dialog.TaskDialog;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import a_vcard.android.provider.Contacts;
import a_vcard.android.syncml.pim.VDataBuilder;
import a_vcard.android.syncml.pim.VNode;
import a_vcard.android.syncml.pim.vcard.ContactStruct;
import a_vcard.android.syncml.pim.vcard.VCardComposer;
import a_vcard.android.syncml.pim.vcard.VCardException;
import a_vcard.android.syncml.pim.vcard.VCardParser;

import static android.R.attr.type;


/**
 * Function:
 * Author:      Saud
 * Create:      2016/11/12
 * Modtime:     2016/11/12
 */
@SuppressLint("ValidFragment")
public class ContactsDialog extends TaskDialog {
    private static final String STATE_TASK_CONTACTS_TYPE = "state_task_contacts_type";

    private BrowserActivity mContext;

    public ContactsDialog() {
    }

    public ContactsDialog(Context context) {
        if (context instanceof BrowserActivity) {
            this.mContext = (BrowserActivity) context;
        }
    }

    @Override
    public void onTaskFailed(SeafException e) {
        super.onTaskFailed(e);
    }

    @Override
    public void onTaskSuccess() {
        Toast.makeText(mContext, getString(R.string.contacts_backup_success), Toast.LENGTH_LONG).show();
        super.onTaskSuccess();
    }


    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(R.string.contacts_backup_title);

    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        outState.putInt(STATE_TASK_CONTACTS_TYPE, type);
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_contacts, null);
        TextView countText = (TextView) view.findViewById(R.id.setting_contacts_backup);
        final String fileName = String.format(getString(R.string.contacts_dialog_backup), Utils.translateTime());
        countText.setText(fileName);
        return view;
    }

    @Override
    protected Task prepareTask() {
        ContactManager contactManager = new ContactManager(mContext);
        return contactManager;
    }

}


class ContactManager extends TaskDialog.Task {
    private static final String DEBUG_TAG = "ContactManager";
    private BrowserActivity mContext;
    private List<Account> mAccounts;
    private String mContactsPath;

    public ContactManager(Context context) {
        if (context instanceof BrowserActivity) {
            mContext = (BrowserActivity) context;
        }
    }


    @Override
    protected void runTask() {
        List<UserData> contactInfo = null;
        try {
            contactInfo = getContactInfo(mContext);
            Log.d(DEBUG_TAG, "contacts  size  :" + contactInfo.size());
            backupContacts(contactInfo);
            mContext.uploadContacts(mContactsPath);
        } catch (SeafException e) {
            setTaskException(e);
            e.printStackTrace();
        }
    }


    /**
     * read  contacts
     *
     * @param context
     * @return
     */
    public List<UserData> getContactInfo(Context context) throws SeafException {

        try {
            List<UserData> infoList = new LinkedList<>();
            Cursor cur = context.getContentResolver().query(ContactsContract.Contacts.CONTENT_URI, null, null, null, null);
            if (cur != null && cur.moveToFirst()) {
                do {
                    int phoneCount = cur.getInt(cur.getColumnIndex(ContactsContract.Contacts.HAS_PHONE_NUMBER));
                    if (phoneCount > 0) {
                        UserData userData = new UserData();
                        String id = cur.getString(cur.getColumnIndex(ContactsContract.Contacts._ID));
                        String displayName = cur.getString(cur.getColumnIndex(ContactsContract.Contacts.DISPLAY_NAME));
                        userData.setName(displayName);
                        userData.setUserid(id);
                        //read contacts phone
                        Cursor phonesCursor = context.getContentResolver().query(ContactsContract.CommonDataKinds.Phone.CONTENT_URI, null,
                                ContactsContract.CommonDataKinds.Phone.CONTACT_ID + "=" + id, null, null);
                        if (phonesCursor != null) {
                            LinkedList<UserData.PhoneInfo> phoneInfos = new LinkedList<>();
                            if (phonesCursor.moveToFirst()) {
                                do {
                                    UserData.PhoneInfo phoneInfo = new UserData.PhoneInfo();
                                    String phoneNumber = phonesCursor.getString(phonesCursor.getColumnIndex(ContactsContract.CommonDataKinds
                                            .Phone.NUMBER));
                                    int type = phonesCursor.getInt(phonesCursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone
                                            .TYPE));
                                    phoneInfo.setNumber(phoneNumber);
                                    phoneInfo.setType(type);
                                    phoneInfos.add(phoneInfo);
                                } while (phonesCursor.moveToNext());
                            }
                            phonesCursor.close();
                            userData.setPhoneList(phoneInfos);
                        }

                        //read  contacts  email
                        Cursor emailCur = context.getContentResolver().query(ContactsContract.CommonDataKinds.Email.CONTENT_URI, null,
                                ContactsContract.CommonDataKinds.Email.CONTACT_ID + "=" + id, null, null);
                        if (emailCur != null) {
                            LinkedList<UserData.EmailInfo> emailInfos = new LinkedList<>();
                            if (emailCur.moveToFirst()) {
                                do {
                                    UserData.EmailInfo emailInfo = new UserData.EmailInfo();
                                    String email = emailCur.getString(emailCur.getColumnIndex(ContactsContract.CommonDataKinds.Email
                                            .DATA1));
                                    int type = emailCur.getInt(emailCur.getColumnIndex(ContactsContract.CommonDataKinds.Email.TYPE));
                                    emailInfo.setEmail(email);
                                    emailInfo.setType(type);
                                    emailInfos.add(emailInfo);
                                } while (emailCur.moveToNext());
                            }
                            emailCur.close();
                            userData.setEmail(emailInfos);
                        }
                        infoList.add(userData);
                    }

                } while (cur.moveToNext());
                cur.close();
            }
            return infoList;
        } catch (Exception e) {
            throw new SeafException(0, mContext.getString(R.string.contacts_backup_fail));
        }
    }


    /**
     * write  contacts to  SD card
     */
    public void backupContacts(List<UserData> infos) throws SeafException {

        if (infos == null || infos.size() == 0) {
            throw new SeafException(0, mContext.getString(R.string.contacts_count_zero));
        }

        try {
            mAccounts = new AccountManager(SeadroidApplication.getAppContext()).getAccountList();
            String path = Environment.getExternalStorageDirectory() + "/Seafile/temp/";
            if (mAccounts.size() > 0) {
                path = new DataManager(mAccounts.get(0)).getAccountDir() + "/temp/";
            }
            File fileDir = new File(path);
            if (!fileDir.exists()) {
                fileDir.mkdirs();
            }
            String fileName = String.format(mContext.getString(R.string.contacts_file_name), Utils.translateTime());
            mContactsPath = fileDir.toString() + "/" + fileName;
            OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(mContactsPath), "UTF-8");
            VCardComposer composer = new VCardComposer();
            for (UserData userData : infos) {
                ContactStruct contact = new ContactStruct();
                contact.name = userData.getName();
                List<UserData.PhoneInfo> phoneList = userData.getPhoneList();
                if (phoneList != null && phoneList.size() > 0) {
                    for (UserData.PhoneInfo phoneInfo : phoneList) {
                        contact.addPhone(phoneInfo.getType(), phoneInfo.getNumber(), "", true);
                    }
                }

                List<UserData.EmailInfo> emailList = userData.getEmail();
                if (emailList != null && emailList.size() > 0) {
                    for (UserData.EmailInfo emailInfo : emailList) {
                        contact.addContactmethod(Contacts.KIND_EMAIL, emailInfo.getType(), emailInfo.getEmail(), null, true);
                    }
                }
                String vcardString = composer.createVCard(contact, VCardComposer.VERSION_VCARD30_INT);
                writer.write(vcardString);
                writer.write("\n");
                writer.flush();
            }
            writer.close();

        } catch (Exception e) {
            e.printStackTrace();
            throw new SeafException(0, mContext.getString(R.string.contacts_backup_fail));
        }
    }


    /**
     * get contacts  from vCard
     *
     * @return
     */
    public List<UserData> restoreContacts() throws SeafException {
        try {
            List<Account> accounts = new AccountManager(SeadroidApplication.getAppContext()).getAccountList();
            String file = Environment.getExternalStorageDirectory() + "/Seafile/contacts/";
            if (accounts.size() > 0) {
                file = new DataManager(accounts.get(0)).getAccountDir() + "/contacts/contacts.vcf";
            }
            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));

            String vcardString = "";
            String line;
            while ((line = reader.readLine()) != null) {
                vcardString += line + "\n";
            }
            reader.close();
            VCardParser parse = new VCardParser();
            VDataBuilder builder = new VDataBuilder();
            boolean parsed = parse.parse(vcardString, "UTF-8", builder);
            if (!parsed) {
                throw new VCardException("Could not parse vCard file:" + file);
            }
            List<VNode> pimContacts = builder.vNodeList;
            List<UserData> contactInfoList = new ArrayList<>();
            for (VNode contact : pimContacts) {
                ContactStruct contactStruct = ContactStruct.constructContactFromVNode(contact, 1);
                UserData userData = new UserData();
                userData.setName(contactStruct.name);

                // get phone numb
                List<ContactStruct.PhoneData> phoneDataList = contactStruct.phoneList;
                if (phoneDataList != null && phoneDataList.size() > 0) {
                    LinkedList<UserData.PhoneInfo> phoneInfos = new LinkedList<>();
                    for (ContactStruct.PhoneData phoneDate : phoneDataList) {
                        UserData.PhoneInfo phoneInfo = new UserData.PhoneInfo();
                        phoneInfo.setType(phoneDate.type);
                        phoneInfo.setNumber(phoneDate.data);
                        phoneInfos.add(phoneInfo);
                    }
                    userData.setPhoneList(phoneInfos);
                }
                //get email
                List<ContactStruct.ContactMethod> emailList = contactStruct.contactmethodList;
                if (emailList != null && emailList.size() > 0) {
                    LinkedList<UserData.EmailInfo> emailInfos = new LinkedList<>();
                    for (ContactStruct.ContactMethod contactMethod : emailList) {
                        if (Contacts.KIND_EMAIL == contactMethod.kind) {
                            UserData.EmailInfo emailInfo = new UserData.EmailInfo();
                            emailInfo.setType(contactMethod.type);
                            emailInfo.setEmail(contactMethod.data);
                            emailInfos.add(emailInfo);
                        }
                    }
                    userData.setEmail(emailInfos);
                }
                contactInfoList.add(userData);
            }
            return contactInfoList;
        } catch (Exception e) {
            e.printStackTrace();
            throw new SeafException(0, mContext.getString(R.string.contacts_recover_fail));
        }
    }

    /**
     * add contacts to  phone
     *
     * @param context
     * @param info
     * @throws SeafException
     */
    public void addContacts(Context context, UserData info) throws SeafException {

        try {
            ContentValues values = new ContentValues();
            Uri rawContactUri = context.getContentResolver().insert(ContactsContract.RawContacts.CONTENT_URI, values);
            long rawContactId = ContentUris.parseId(rawContactUri);
            values.clear();
            values.put(ContactsContract.RawContacts.Data.RAW_CONTACT_ID, rawContactId);
            values.put(ContactsContract.RawContacts.Data.MIMETYPE, ContactsContract.CommonDataKinds.StructuredName.CONTENT_ITEM_TYPE);
            values.put(ContactsContract.CommonDataKinds.StructuredName.GIVEN_NAME, info.getName());
            context.getContentResolver().insert(ContactsContract.Data.CONTENT_URI, values);

            List<UserData.PhoneInfo> phoneList = info.getPhoneList();
            for (UserData.PhoneInfo phoneInfo : phoneList) {
                values.clear();
                values.put(ContactsContract.Contacts.Data.RAW_CONTACT_ID, rawContactId);
                values.put(ContactsContract.RawContacts.Data.MIMETYPE, ContactsContract.CommonDataKinds.Phone.CONTENT_ITEM_TYPE);
                values.put(ContactsContract.CommonDataKinds.Phone.NUMBER, phoneInfo.getNumber());
                values.put(ContactsContract.CommonDataKinds.Phone.TYPE, phoneInfo.getType());
                context.getContentResolver().insert(ContactsContract.Data.CONTENT_URI, values);
            }

            List<UserData.EmailInfo> emailList = info.getEmail();
            for (UserData.EmailInfo email : emailList) {
                values.clear();
                values.put(ContactsContract.Contacts.Data.RAW_CONTACT_ID, rawContactId);
                values.put(ContactsContract.RawContacts.Data.MIMETYPE, ContactsContract.CommonDataKinds.Email.CONTENT_ITEM_TYPE);
                values.put(ContactsContract.CommonDataKinds.Email.DATA, email.getEmail());
                values.put(ContactsContract.CommonDataKinds.Email.TYPE, email.getType());
                context.getContentResolver().insert(ContactsContract.Data.CONTENT_URI, values);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new SeafException(0, mContext.getString(R.string.contacts_recover_fail));
        }
    }
}


