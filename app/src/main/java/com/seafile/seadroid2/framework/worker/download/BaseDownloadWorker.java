package com.seafile.seadroid2.framework.worker.download;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import org.json.JSONException;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.SocketTimeoutException;

import javax.net.ssl.SSLHandshakeException;

public abstract class BaseDownloadWorker extends TransferWorker {

    public abstract BaseNotification getNotification();

    public BaseDownloadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    private TransferResult parseTransferException(Exception e) {
        if (e instanceof JSONException) {
            return TransferResult.ENCODING_EXCEPTION;
        } else if (e instanceof SeafException) {
            if (e == SeafException.notFoundException) {
                return TransferResult.FILE_NOT_FOUND;
            } else if (e == SeafException.OUT_OF_QUOTA) {
                return TransferResult.OUT_OF_QUOTA;
            } else if (e == SeafException.networkException) {
                return TransferResult.NETWORK_CONNECTION;
            } else if (e == SeafException.sslException) {
                return TransferResult.SSL_EXCEPTION;
            } else if (e == SeafException.illFormatException) {
                return TransferResult.ENCODING_EXCEPTION;
            } else if (e == SeafException.notLoggedInException) {
                return TransferResult.ACCOUNT_NOT_LOGGED_IN;
            } else if (e == SeafException.notFoundUserException) {
                return TransferResult.ACCOUNT_NOT_FOUND;
            }
        } else if (e instanceof UnsupportedEncodingException) {
            return TransferResult.ENCODING_EXCEPTION;
        } else if (e instanceof SSLHandshakeException) {
            return TransferResult.SSL_EXCEPTION;
        } else if (e instanceof SocketTimeoutException) {
            return TransferResult.NETWORK_CONNECTION;
        } else if (e instanceof IOException) {
            return TransferResult.NETWORK_CONNECTION;
        }

        return TransferResult.UNKNOWN;
    }

    public TransferResult onException(FileTransferEntity transferEntity, Exception e) {
        transferEntity.transfer_status = TransferStatus.FAILED;
        transferEntity.action_end_at = System.currentTimeMillis();
        transferEntity.transfer_result = parseTransferException(e);

        //update db
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        return transferEntity.transfer_result;
    }


    public void notifyError(TransferResult result) {
        if (result == TransferResult.NETWORK_CONNECTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error, R.string.download);
        } else {
            getGeneralNotificationHelper().showErrorNotification(String.valueOf(result), R.string.download);
        }
    }

    public String isInterrupt(TransferResult result) {
        String finishFlagEvent = null;
        if (result == TransferResult.ENCODING_EXCEPTION) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.FILE_NOT_FOUND) {
//            finishFlagEvent = null;
        } else if (result == TransferResult.OUT_OF_QUOTA) {
            finishFlagEvent = TransferEvent.EVENT_CANCEL_WITH_OUT_OF_QUOTA;
        } else if (result == TransferResult.NETWORK_CONNECTION) {
            finishFlagEvent = TransferEvent.EVENT_CANCEL_WITH_NETWORK_ERR;
        } else if (result == TransferResult.SSL_EXCEPTION) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.ACCOUNT_NOT_LOGGED_IN) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.ACCOUNT_NOT_FOUND) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.UNKNOWN) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        }

        return finishFlagEvent;
    }
}
