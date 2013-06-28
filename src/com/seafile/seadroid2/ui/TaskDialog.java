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
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;


interface TaskStateListener {
    void onTaskSuccess();
    void onTaskFailed(SeafException e);
}

/**
 * Basic class for dialog which get input from user and carries out some
 * operation in the background.
 * @param K
 */
public abstract class TaskDialog<K extends TaskDialog.Task>
                        extends DialogFragment implements TaskStateListener {
    public static abstract class TaskDialogListener {
        public void onTaskSuccess() {
        }
        public void onTaskFailed(SeafException e) {
        }
        public void onTaskCancelled() {
        }
    }

    // The AsyncTask instance
    private K task;

    // The error message
    private TextView errorText;

    // The spinning wheel to show loading
    private ProgressBar loading;

    //
    private Button okButton, cancelButton;

    // The content area of the dialog
    private View contentView;

    private TaskDialogListener mListener;

    protected BrowserActivity getBrowserActivity() {
        return (BrowserActivity)getActivity();
    }

    /**
     * Save the content you are interested
     * @param outState
     */
    protected abstract void saveDialogState(Bundle outState);

    /**
     * Create the content area of the dialog
     * @param inflater
     * @return The created view
     */
    protected abstract View onCreateDialogContentView(LayoutInflater inflater,
                                                      Bundle savedInstanceState);

    /**
     * Create the AsyncTask
     */
    protected abstract K prepareTask();

    /**
     * Call when the "OK" button is clicked
     * @throws Exception with the error message if there is error in user input
     */
    protected void onValidateUserInput() throws Exception {
    }

    /**
     * Return the content area view of the dialog
     */
    protected View getContentView() {
        return contentView;
    }

    @Override
    public void onTaskSuccess() {
        getDialog().dismiss();
        if (mListener != null) {
            mListener.onTaskSuccess();
        }
    }

    @Override
    public void onTaskFailed(SeafException e) {
        hideLoading();
        showError(e.getMessage());
        enableInput();
        if (mListener != null) {
            mListener.onTaskFailed(e);
        }
    }

    public void setTaskDialogLisenter(TaskDialogListener listener) {
        mListener = listener;
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        // TODO: save state of error text
        saveDialogState(outState);

        super.onSaveInstanceState(outState);
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        LinearLayout view = (LinearLayout)inflater.inflate(R.layout.task_dialog, null);
        contentView = onCreateDialogContentView(inflater, savedInstanceState);

        view.addView(contentView, 0);

        errorText = (TextView)view.findViewById(R.id.error_message);
        loading = (ProgressBar)view.findViewById(R.id.loading);

        builder.setView(view);

        builder.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
            }
        });

        builder.setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (task != null) {
                    task.cancel(true);
                }
                if (mListener != null) {
                    mListener.onTaskCancelled();
                }
            }
        });

        final AlertDialog dialog = builder.create();
        final View.OnClickListener onOKButtonClickedListener = new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                try {
                    onValidateUserInput();
                } catch (Exception e) {
                    showError(e.getMessage());
                    return;
                }

                disableInput();
                hideError();
                showLoading();

                task = prepareTask();
                task.setTaskStateLisenter(TaskDialog.this);

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

    protected void showLoading() {
        loading.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_in));
        loading.setVisibility(View.VISIBLE);
    }

    protected void hideLoading() {
        loading.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_out));
        loading.setVisibility(View.INVISIBLE);
    }

    protected void showError(String error) {
        errorText.setText(error);
        errorText.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_in));
        errorText.setVisibility(View.VISIBLE);
    }

    protected void hideError() {
        errorText.startAnimation(AnimationUtils.loadAnimation(
                                   getBrowserActivity(), android.R.anim.fade_out));
        errorText.setVisibility(View.GONE);
    }

    protected void disableInput() {
        okButton.setEnabled(false);
    }

    protected void enableInput() {
        okButton.setEnabled(true);
    }

    public static abstract class Task extends AsyncTask<Void, Long, Void> {
        private SeafException err;
        private TaskStateListener mListener;

        public void setTaskStateLisenter(TaskStateListener listener) {
            mListener = listener;
        }

        protected void setTaskException(SeafException e) {
            err = e;
        }

        public SeafException getTaskException() {
            return err;
        }

        protected abstract void runTask();

        @Override
        public Void doInBackground(Void... params) {
            runTask();
            return null;
        }

        @Override
        public void onPostExecute(Void result) {
            if (err != null) {
                mListener.onTaskFailed(err);
            } else {
                mListener.onTaskSuccess();
            }
        }
    }
}