package com.seafile.seadroid2.io.http.converter;

import androidx.annotation.NonNull;

import com.google.gson.Gson;
import com.google.gson.TypeAdapter;

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

        try {
            String body = value.string();
            return adapter.fromJson(body);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
        } finally {
            value.close();
        }
    }
}

