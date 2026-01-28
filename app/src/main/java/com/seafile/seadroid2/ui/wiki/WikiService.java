package com.seafile.seadroid2.ui.wiki;

import com.seafile.seadroid2.framework.model.wiki.Wiki1Model;
import com.seafile.seadroid2.framework.model.wiki.Wiki2Model;

import io.reactivex.Single;
import retrofit2.http.GET;

public interface WikiService {
    @GET("api/v2.1/wikis/")
    Single<Wiki1Model> getWikis();

    @GET("api/v2.1/wikis2/")
    Single<Wiki2Model> getWikis2();
}
