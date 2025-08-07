package com.seafile.seadroid2.framework.http.converter;

import androidx.annotation.NonNull;

import com.google.gson.Gson;
import com.google.gson.TypeAdapter;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.util.SLogs;

import java.io.IOException;

import okhttp3.ResponseBody;
import retrofit2.Converter;

public class SupportResponseConverter<T> implements Converter<ResponseBody, T> {
    private final Gson gson;
    TypeAdapter<T> adapter;

    public SupportResponseConverter(Gson gson, TypeAdapter<T> adapter) {
        this.gson = gson;
        this.adapter = adapter;
    }

    @Override
    public T convert(@NonNull ResponseBody value) throws IOException {
        String body = value.string();
        try {
            return adapter.fromJson(body);
        } catch (Exception e) {
            SLogs.e("Response is not valid JSON");
            SLogs.e(body);
            throw new IOException("Response is not valid JSON");
        } finally {
            value.close();
        }
    }
}

