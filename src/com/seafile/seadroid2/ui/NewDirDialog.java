package com.seafile.seadroid2.ui;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;

public class NewDirDialog extends DialogFragment {

    private EditText dirNameText;
    private TextView errorText; 
    private BrowserActivity mActivity;

    public NewDirDialog(BrowserActivity activity) {
        mActivity = activity;
    }
    
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        View view = inflater.inflate(R.layout.dialog_new_dir, null);
        dirNameText = (EditText) view.findViewById(R.id.new_dir_name);
        errorText = (TextView) view.findViewById(R.id.error_message);

        builder.setView(view)
            .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                }
            });
        builder.setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
            }
        });

        final AlertDialog dialog = builder.create();
        final View.OnClickListener onOKButtonClickedListener = new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String dirName = dirNameText.getText().toString();

                // TODO: show loading and disable  input
                NavContext nav = mActivity.getNavContext();
                NewDirTask task = new NewDirTask(nav.getRepoID(),
                                                 nav.getDirPath(),
                                                 dirName,
                                                 mActivity.getDataManager());
                task.setNewDirEveventListener(new NewDirEventListener() {
                    @Override
                    public void onSuccess(String dirName) {
                        dialog.dismiss();
                        mActivity.onNewDirCreated(dirName);
                    }

                    @Override
                    public void onFailure(String dirName, String err) {
                        errorText.setVisibility(View.VISIBLE);
                        errorText.setText(err);
                    }
                });

                ConcurrentAsyncTask.execute(task);
            }
        };
        
        dialog.setOnShowListener(new DialogInterface.OnShowListener() {
            @Override
            public void onShow(DialogInterface d) {
                Button btn = dialog.getButton(AlertDialog.BUTTON_POSITIVE);
                btn.setOnClickListener(onOKButtonClickedListener);
            }
        });

        return dialog;
    }

    private interface NewDirEventListener {
        public void onSuccess(String dirName);
        public void onFailure(String dirName, String err);
    }

    private class NewDirTask extends AsyncTask<Void, Void, Void> {
        String repoID;
        String parentDir;
        String dirName;
        DataManager dataManager;
        SeafException err;
        NewDirEventListener mListener;

        public NewDirTask(String repoID, String parentDir, String dirName, DataManager dataManager) {
            this.repoID = repoID;
            this.parentDir = parentDir;
            this.dirName = dirName;
            this.dataManager = dataManager;
        }

        public void setNewDirEveventListener(NewDirEventListener listener) {
            mListener = listener;
        }

        @Override
        public Void doInBackground(Void...params) {
            try {
                dataManager.createNewDir(repoID, parentDir, dirName);
            } catch (SeafException e) {
                err = e;
            }
            return null;
        }

        @Override
        public void onPostExecute(Void v) {
            if (err != null) {
                mListener.onFailure(dirName, err.getMessage());
            } else {
                mListener.onSuccess(dirName);
            }
        }
    }
}