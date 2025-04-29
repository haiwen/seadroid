package com.seafile.seadroid2.ui.share;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.util.ArrayList;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;
import retrofit2.Call;
import retrofit2.Response;

public class ShareToSeafileViewModel extends BaseViewModel {

    private final MutableLiveData<Boolean> mActionLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getActionLiveData() {
        return mActionLiveData;
    }

    public void checkRemoteExists(Context context, Account account, String repoId, String repoName, String parentDir, List<Uri> uris, Consumer<Boolean> consumer) {
        Single<Boolean> single = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                Call<DirentWrapperModel> call = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getDirentsSync(repoId, parentDir);
                Response<DirentWrapperModel> res = call.execute();
                if (!res.isSuccessful()) {
                    emitter.onError(SeafException.NETWORK_EXCEPTION);
                    return;
                }


                DirentWrapperModel wrapperModel = res.body();
                if (wrapperModel == null || CollectionUtils.isEmpty(wrapperModel.dirent_list)) {
                    emitter.onSuccess(false);
                    return;
                }

                List<DirentModel> existsList = new ArrayList<>();
                //todo simplify
                for (Uri uri : uris) {
                    String fileName = Utils.getFilenameFromUri(context, uri);
                    for (DirentModel direntModel : wrapperModel.dirent_list) {
                        if (TextUtils.equals(direntModel.name, fileName)) {
                            existsList.add(direntModel);
                            break;
                        }
                    }
                }

                emitter.onSuccess(!CollectionUtils.isEmpty(existsList));
            }
        });

        addSingleDisposable(single, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean b) throws Exception {
                consumer.accept(b);
            }
        });
    }

    public void upload(Context context, Account account, String repoId, String repoName,
                       String parentDir, List<Uri> uris, boolean isReplace) {

        if (CollectionUtils.isEmpty(uris)) {
            return;
        }

        for (Uri uri : uris) {
            String fileName = Utils.getFilenameFromUri(context, uri);
            TransferModel transferModel = gen(context, account, repoId, repoName, uri, fileName, parentDir, isReplace);
            GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(transferModel);
        }

        getActionLiveData().setValue(true);
    }

    private TransferModel gen(Context context, Account account, String repo_id, String repo_name, Uri sourceUri, String fileName, String parentDir, boolean isReplace) {
        //content://com.android.providers.media.documents/document/image:1000182224
        TransferModel transferModel = gen(account, repo_id, repo_name, fileName, parentDir, isReplace);
        transferModel.full_path = sourceUri.toString();
        transferModel.file_size = Utils.getFileSize(context, sourceUri);
        transferModel.setId(transferModel.genStableId());
        return transferModel;
    }

    private TransferModel gen(Account account, String repo_id, String repo_name, String fileName, String parentDir, boolean isReplace) {
        TransferModel transferModel = new TransferModel();
        transferModel.save_to = SaveTo.NO_SAVE;
        transferModel.created_at = System.currentTimeMillis();
        transferModel.repo_id = repo_id;
        transferModel.repo_name = repo_name;
        transferModel.related_account = account.getSignature();
        transferModel.target_path = Utils.pathJoin(parentDir, fileName);
        transferModel.setParentPath(parentDir);
        transferModel.file_name = fileName;
        transferModel.data_source = TransferDataSource.FILE_BACKUP;
        transferModel.transfer_status = TransferStatus.WAITING;
        transferModel.transfer_strategy = isReplace ? ExistingFileStrategy.REPLACE : ExistingFileStrategy.KEEP;
        return transferModel;
    }
}
