package com.seafile.seadroid2.io.http;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.io.http.converter.ConverterFactory;
import com.seafile.seadroid2.io.http.interceptor.HeaderInterceptor;
import com.seafile.seadroid2.util.SLogs;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

import okhttp3.Interceptor;
import okhttp3.logging.HttpLoggingInterceptor;
import retrofit2.Converter;


public class IO extends BaseIO {
    private static volatile IO ioInstance;
    private String mServerUrl;
    private String mToken;

    /**
     * Logged in
     */
    public static IO getSingleton() {
        if (ioInstance == null) {
            synchronized (IO.class) {
                if (ioInstance == null) {
                    ioInstance = new IO();

                    Account cur = SupportAccountManager.getInstance().getCurrentAccount();
                    if (cur != null) {
                        SLogs.d(cur.toString());
                        ioInstance.setToken(cur.token);
                        ioInstance.setServerUrl(cur.server);
                    }
                }
            }
        }
        return ioInstance;
    }

    /**
     * Not logged in/Log in to another server
     */
    public static IO getNewInstance(String hostUrl, String token) {
        IO io = new IO();
        io.setServerUrl(hostUrl);
        io.setToken(token);
        return io;
    }

    /**
     * When you log in again or switch account, you should reset this IO Singleton.
     * <p><b>because it's a SINGLETON, unless kill the APP!<b/></p>
     */
    public static void resetSingleton() {
        ioInstance = null;
    }

    private void setServerUrl(String mHostUrl) {
        this.mServerUrl = mHostUrl;
    }

    private void setToken(String mToken) {
        this.mToken = mToken;
    }

    /**
     * server url
     */
    @Override
    public String getServerUrl() {
        return mServerUrl;
    }

    /**
     * @return host.com
     */
    public String getHostDomain() {
        String host = mServerUrl;
        host = StringUtils.toRootLowerCase(host);
        host = StringUtils.removeStart(host, Constants.Protocol.HTTPS);
        host = StringUtils.removeStart(host, Constants.Protocol.HTTP);
        return StringUtils.removeEnd(host, "/");
    }


    @Override
    public List<Interceptor> getInterceptors() {

        List<Interceptor> interceptors = new ArrayList<>();

        //print log
        HttpLoggingInterceptor loggingInterceptor = new HttpLoggingInterceptor();
        loggingInterceptor.setLevel(BuildConfig.DEBUG ? HttpLoggingInterceptor.Level.BODY : HttpLoggingInterceptor.Level.BASIC);
        interceptors.add(loggingInterceptor);

        interceptors.add(new HeaderInterceptor(mToken));
//        interceptors.add(new AddCookiesInterceptor());
//        interceptors.add(new ReceivedCookiesInterceptor());

        return interceptors;
    }

    @Override
    public Converter.Factory getConverterFactory() {
        return ConverterFactory.create();
    }

}
