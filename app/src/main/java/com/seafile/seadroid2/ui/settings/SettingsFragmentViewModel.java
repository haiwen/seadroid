package com.seafile.seadroid2.ui.settings;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.data.model.server.ServerInfoModel;
import com.seafile.seadroid2.ui.main.MainService;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.io.http.IO;

import org.apache.commons.io.FileUtils;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;

public class SettingsFragmentViewModel extends BaseViewModel {
    private final MutableLiveData<AccountInfo> accountInfoLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> cacheSizeLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getCacheSizeLiveData() {
        return cacheSizeLiveData;
    }

    public MutableLiveData<AccountInfo> getAccountInfoLiveData() {
        return accountInfoLiveData;
    }

    public void getAccountInfo() {
        getRefreshLiveData().setValue(true);

        Single<ServerInfoModel> single1 = IO.getSingleton().execute(MainService.class).getServerInfo();
        Single<AccountInfo> single2 = IO.getSingleton().execute(AccountService.class).getAccountInfo();

        Single<AccountInfo> single = Single.zip(single1, single2, new BiFunction<ServerInfoModel, AccountInfo, AccountInfo>() {
            @Override
            public AccountInfo apply(ServerInfoModel serverInfoModel, AccountInfo accountInfo) throws Exception {

                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return accountInfo;
                }

                accountInfo.setServer(IO.getSingleton().getServerUrl());

                ServerInfo serverInfo1 = new ServerInfo(account.server, serverInfoModel.version, serverInfoModel.getFeaturesString());
                SupportAccountManager.getInstance().setServerInfo(account, serverInfo1);
                return accountInfo;
            }
        });

        addSingleDisposable(single, new Consumer<AccountInfo>() {
            @Override
            public void accept(AccountInfo accountInfo) throws Exception {
                accountInfo.setServer(IO.getSingleton().getServerUrl());

                getRefreshLiveData().setValue(false);

                getAccountInfoLiveData().setValue(accountInfo);
            }
        });
    }

    public void calculateCacheSize() {

        Single<String> single = Single.create(new SingleOnSubscribe<String>() {
            @Override
            public void subscribe(SingleEmitter<String> emitter) throws Exception {
                long l = StorageManager.getInstance().getUsedSpace();

                String total = FileUtils.byteCountToDisplaySize(l);
                emitter.onSuccess(total);
            }
        });
        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String s) throws Exception {
                getCacheSizeLiveData().setValue(s);
            }
        });

    }
}
