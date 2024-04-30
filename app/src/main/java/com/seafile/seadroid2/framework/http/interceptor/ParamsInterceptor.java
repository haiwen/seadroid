package com.seafile.seadroid2.framework.http.interceptor;


import android.os.Build;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.AppUtils;

import java.io.IOException;

import okhttp3.FormBody;
import okhttp3.HttpUrl;
import okhttp3.Interceptor;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public class ParamsInterceptor implements Interceptor {

    private static final String TAG = "request params";

    @Override
    public Response intercept(@NonNull Chain chain) throws IOException {

        Request orgRequest = chain.request();
        RequestBody body = orgRequest.body();
        //收集请求参数，方便调试
        StringBuilder paramsBuilder = new StringBuilder();

        if (body != null) {

            RequestBody newBody;

            if (body instanceof FormBody) {
                newBody = addParamsToFormBody((FormBody) body, paramsBuilder);
            } else if (body instanceof MultipartBody) {
                newBody = addParamsToMultipartBody((MultipartBody) body, paramsBuilder);
            } else {
                Request original = chain.request();
                HttpUrl originalHttpUrl = original.url();

                newBody = new FormBody.Builder()
                        .add("client_version", AppUtils.getAppName())
                        .add("platform", "android")
                        .add("device_name", Build.MODEL)
                        .add("platform_version", Build.VERSION.RELEASE)
                        .build();

                Request.Builder requestBuilder = original.newBuilder()
                        .method(original.method(), newBody)
                        .url(originalHttpUrl);

                Request request = requestBuilder.build();
                return chain.proceed(request);
            }

            //打印参数
            Request newRequest = orgRequest.newBuilder()
                    .url(orgRequest.url())
                    .method(orgRequest.method(), newBody)
                    .build();

            return chain.proceed(newRequest);

        }

        return chain.proceed(orgRequest);
    }

    /**
     * 为MultipartBody类型请求体添加参数
     * <p>
     *
     * @param body          请求主体
     * @param paramsBuilder 参数builder
     * @return builder.build();
     */
    private MultipartBody addParamsToMultipartBody(MultipartBody body, StringBuilder paramsBuilder) {
        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        builder.addFormDataPart("client_version", AppUtils.getAppName());
        builder.addFormDataPart("platform", "android");
        builder.addFormDataPart("device_name", Build.MODEL);
        builder.addFormDataPart("platform_version", Build.VERSION.RELEASE);

        //添加原请求体
        for (int i = 0; i < body.size(); i++) {
            builder.addPart(body.part(i));
        }

        return builder.build();
    }


    /**
     * 为FormBody类型请求体添加参数
     * <p>
     *
     * @param body          请求主体
     * @param paramsBuilder 参数builder
     * @return builder.build();
     */
    private FormBody addParamsToFormBody(FormBody body, StringBuilder paramsBuilder) {
        FormBody.Builder builder = new FormBody.Builder();

        builder.add("client_version", AppUtils.getAppName());
        builder.add("platform", "android");
        builder.add("device_name", Build.MODEL);
        builder.add("platform_version", Build.VERSION.RELEASE);

        return builder.build();
    }


}

