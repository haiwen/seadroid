package com.seafile.seadroid2.framework.http;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.http.callback.ProgressCallback;
import com.seafile.seadroid2.framework.http.download.BinaryFileDownloader;
import com.seafile.seadroid2.framework.http.download.BinaryFileWriter;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.http.converter.ConverterFactory;
import com.seafile.seadroid2.framework.http.interceptor.HeaderInterceptor;
import com.seafile.seadroid2.listener.ProgressListener;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import io.reactivex.BackpressureStrategy;
import io.reactivex.Flowable;
import io.reactivex.FlowableEmitter;
import io.reactivex.FlowableOnSubscribe;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import retrofit2.Converter;


public class IO extends BaseIO {
    private static volatile IO LOGGED_IN_INSTANCE;
    private String mServerUrl;
    private String mToken;

    private Account account;

    private static final ConcurrentMap<String, IO> IO_MAP = new ConcurrentHashMap<>();

    /**
     * Logged in
     */
    public static IO getInstanceWithLoggedIn() {
        if (LOGGED_IN_INSTANCE != null) {
            return LOGGED_IN_INSTANCE;
        }

        // singleton and map
        synchronized (IO.class) {
            if (LOGGED_IN_INSTANCE == null) {
                LOGGED_IN_INSTANCE = new IO();

                Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
                if (currentAccount == null) {
                    throw new IllegalStateException("No current account");
                }

                SLogs.d(currentAccount.toString());
                LOGGED_IN_INSTANCE.setAccount(currentAccount);
                LOGGED_IN_INSTANCE.setToken(currentAccount.token);
                LOGGED_IN_INSTANCE.setServerUrl(currentAccount.server);

                //
                IO_MAP.put(currentAccount.getSignature(), LOGGED_IN_INSTANCE);
            }
        }

        return LOGGED_IN_INSTANCE;
    }


    /**
     * Not logged in/Log in to another server
     */
    public static IO getInstanceByAccount(Account account) {
        if (IO_MAP.containsKey(account.getSignature())) {
            return IO_MAP.get(account.getSignature());
        }

        IO io = new IO();
        io.setServerUrl(account.server);
        io.setToken(account.token);
        io.account = account;

        IO_MAP.put(account.getSignature(), io);
        return io;
    }

    public static void updateInstanceByAccount(Account account) {
        IO_MAP.remove(account.getSignature());

        //reset okhttp client
        CLIENT_MAP.remove(account.getSignature());

        //
        IO io = new IO();
        io.setServerUrl(account.server);
        io.setToken(account.token);
        io.setAccount(account);

        IO_MAP.put(account.getSignature(), io);
    }

    @Override
    public Account getAccount() {
        return account;
    }

//    /**
//     * Not logged in/Log in to another server
//     */
//    public static IO getNewInstance(String hostUrl, String token) {
//        IO io = new IO();
//        io.setServerUrl(hostUrl);
//        io.setToken(token);
//
//        //
//        resetSingleton();
//
//        return io;
//    }

    /**
     * When you log in again or switch account, you should reset this IO Singleton.
     * <p><b>because it's a SINGLETON, unless kill the APP!<b/></p>
     */
    public static void resetLoggedInInstance() {
        LOGGED_IN_INSTANCE = null;
    }

    private void setServerUrl(String mHostUrl) {
        this.mServerUrl = mHostUrl;
    }

    private void setToken(String mToken) {
        this.mToken = mToken;
    }

    public String getToken() {
        return mToken;
    }

    public void setAccount(Account account) {
        this.account = account;
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
        loggingInterceptor.setLevel(HttpLoggingInterceptor.Level.BASIC);
//        loggingInterceptor.setLevel(BuildConfig.DEBUG ? HttpLoggingInterceptor.Level.BODY : HttpLoggingInterceptor.Level.BASIC);
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

    public void downloadBinarySync(String url, File destinationFile, ProgressListener callback) throws IOException {
        OkHttpClient client = getClient();

        try (OutputStream outputStream = Files.newOutputStream(destinationFile.toPath())) {
            BinaryFileWriter fileWriter = new BinaryFileWriter(outputStream, new ProgressCallback() {
                @Override
                public void onProgress(long transferSize, long totalSize) {
                    if (callback != null) {
                        callback.onProgress(destinationFile.getName(), transferSize, totalSize);
                    }
                }
            });

            try (BinaryFileDownloader fileDownloader = new BinaryFileDownloader(client, fileWriter)) {
                fileDownloader.download(url);

            } catch (Exception e) {
                if (callback != null) {
                    callback.isCancelled();
                }
                e.printStackTrace();
            }
        }
    }

    public Flowable<Long[]> downloadBinary(String url, File destinationFile) throws IOException {

        return Flowable.create(new FlowableOnSubscribe<Long[]>() {
            @Override
            public void subscribe(FlowableEmitter<Long[]> emitter) throws Exception {
                OkHttpClient client = getClient();

                try (OutputStream outputStream = Files.newOutputStream(destinationFile.toPath())) {
                    BinaryFileWriter fileWriter = new BinaryFileWriter(outputStream, new ProgressCallback() {
                        @Override
                        public void onProgress(long transferSize, long totalSize) {
                            emitter.onNext(new Long[]{transferSize, totalSize});
                        }
                    });

                    try (BinaryFileDownloader fileDownloader = new BinaryFileDownloader(client, fileWriter)) {
                        fileDownloader.download(url);

                        emitter.onComplete();
                    } catch (Exception e) {
                        emitter.onError(e);
                    }
                }
            }
        }, BackpressureStrategy.BUFFER);
    }
}
