package com.seafile.seadroid2.ui.account.sso;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.model.sso.SSOLinkModel;
import com.seafile.seadroid2.framework.model.sso.SSOStatusModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.main.MainService;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class SingleSignOnViewModel extends BaseViewModel {
    private final MutableLiveData<ServerInfoModel> _serverInfoLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> _ssoLinkLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> _ssoStatusLiveData = new MutableLiveData<>();
    private final MutableLiveData<Account> _accountLiveData = new MutableLiveData<>();

    public MutableLiveData<Account> getAccountLiveData() {
        return _accountLiveData;
    }

    public MutableLiveData<String> getSsoStatusLiveData() {
        return _ssoStatusLiveData;
    }

    public MutableLiveData<String> getSsoLinkLiveData() {
        return _ssoLinkLiveData;
    }

    public MutableLiveData<ServerInfoModel> getServerInfoLiveData() {
        return _serverInfoLiveData;
    }

    public void loadServerInfo(String host) {
        getRefreshLiveData().setValue(true);
        Account tempAccount = new Account();
        tempAccount.is_shib = true;
        tempAccount.server = host;
        tempAccount.email = EncryptUtils.encryptMD5ToString(host);

        Single<ServerInfoModel> serverSingle = HttpIO.getInstanceByAccount(tempAccount).execute(MainService.class).getServerInfo();
        addSingleDisposable(serverSingle, new Consumer<ServerInfoModel>() {
            @Override
            public void accept(ServerInfoModel serverInfoModel) throws Exception {
                getServerInfoLiveData().setValue(serverInfoModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                String errMsg = getErrorMsgByThrowable(throwable);
                Toasts.show(errMsg);
            }
        });
    }

    public void getSsoLink(String host) {
        getRefreshLiveData().setValue(true);
        Account tempAccount = new Account();
        tempAccount.is_shib = true;
        tempAccount.server = host;
        tempAccount.email = EncryptUtils.encryptMD5ToString(host);

        Single<SSOLinkModel> serverSingle = HttpIO.getInstanceByAccount(tempAccount).execute(AccountService.class).getSsoLink();
        addSingleDisposable(serverSingle, new Consumer<SSOLinkModel>() {
            @Override
            public void accept(SSOLinkModel ssoLinkModel) throws Exception {
                getRefreshLiveData().setValue(false);
                getSsoLinkLiveData().setValue(ssoLinkModel.link);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void getSsoStatus(String host, String token) {
        getRefreshLiveData().setValue(true);
        Account tempAccount = new Account();
        tempAccount.is_shib = true;
        tempAccount.server = host;
        tempAccount.email = EncryptUtils.encryptMD5ToString(host);

        Single<SSOStatusModel> serverSingle = HttpIO.getInstanceByAccount(tempAccount).execute(AccountService.class).getSsoStatus(token);
        addSingleDisposable(serverSingle, new Consumer<SSOStatusModel>() {
            @Override
            public void accept(SSOStatusModel model) throws Exception {

                if (TextUtils.equals("success", model.status)) {
                    tempAccount.token = model.apiToken;
                    tempAccount.email = model.username;
                    loadAccountInfo(tempAccount);
                }else {
                    getSsoStatusLiveData().setValue(model.status);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                getSsoStatusLiveData().setValue(null);
            }
        });
    }

    public void loadAccountInfo(Account loginAccount) {

        Single<AccountInfo> single = HttpIO.getInstanceByAccount(loginAccount).execute(AccountService.class).getAccountInfo();
        addSingleDisposable(single, new Consumer<AccountInfo>() {
            @Override
            public void accept(AccountInfo accountInfo) throws Exception {

                loginAccount.login_time = System.currentTimeMillis();
                loginAccount.setEmail(accountInfo.getEmail());
                loginAccount.setName(accountInfo.getName());
                loginAccount.setAvatarUrl(accountInfo.getAvatarUrl());
                loginAccount.total = accountInfo.getTotal();
                loginAccount.usage = accountInfo.getUsage();
                loginAccount.login_time = System.currentTimeMillis();
                loginAccount.is_shib = true;

                getRefreshLiveData().setValue(false);
                getAccountLiveData().setValue(loginAccount);

            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }
}
