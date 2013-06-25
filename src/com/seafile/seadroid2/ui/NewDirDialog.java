package com.seafile.seadroid2.ui;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.AnimationUtils;
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
    private View loading;
    private Button okButton, cancelButton;

    public NewDirDialog() {
    }

    private BrowserActivity getBrowserActivity() {
        return (BrowserActivity)getActivity();
    }
    
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        
        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        View view = inflater.inflate(R.layout.dialog_new_dir, null);
        dirNameText = (EditText) view.findViewById(R.id.new_dir_name);
        errorText = (TextView) view.findViewById(R.id.error_message);
        loading = view.findViewById(R.id.loading);

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
                String dirName = dirNameText.getText().toString().trim();

                if (dirName.length() == 0) {
                    String err = getBrowserActivity().getResources().getString(R.string.dir_name_empty);
                    showError(err);
                    return;
                }

                // TODO: disable input when requesting
                disableInput();
                hideError();
                showLoading();
                NavContext nav = getBrowserActivity().getNavContext();
                NewDirTask task = new NewDirTask(nav.getRepoID(),
                                                 nav.getDirPath(),
                                                 dirName,
                                                 getBrowserActivity().getDataManager());
                task.setNewDirEveventListener(new NewDirEventListener() {
                    @Override
                    public void onSuccess(String dirName) {
                        dialog.dismiss();
                        getBrowserActivity().onNewDirCreated(dirName);
                    }

                    @Override
                    public void onFailure(String dirName, String err) {
                        hideLoading();
                        showError(err);
                        enableInput();
                    }
                });

                ConcurrentAsyncTask.execute(task);
            }
        };
        
        dialog.setOnShowListener(new DialogInterface.OnShowListener() {
            @Override
            public void onShow(DialogInterface d) {
                okButton = dialog.getButton(AlertDialog.BUTTON_POSITIVE);
                cancelButton = dialog.getButton(AlertDialog.BUTTON_NEGATIVE);
                okButton.setOnClickListener(onOKButtonClickedListener);
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

    private void showLoading() {
        loading.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_in));
        loading.setVisibility(View.VISIBLE);
    }

    private void hideLoading() {
        loading.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_out));
        loading.setVisibility(View.INVISIBLE);
    }

    private void showError(String error) {
        errorText.setText(error);
        errorText.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_in));
        errorText.setVisibility(View.VISIBLE);
    }

    private void hideError() {
        errorText.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_out));
        errorText.setVisibility(View.GONE);
    }

    private void disableInput() {
        dirNameText.setEnabled(false);
        okButton.setEnabled(false);
        cancelButton.setEnabled(false);
    }

    private void enableInput() {
        dirNameText.setEnabled(true);
        okButton.setEnabled(true);
        cancelButton.setEnabled(true);
    }
}