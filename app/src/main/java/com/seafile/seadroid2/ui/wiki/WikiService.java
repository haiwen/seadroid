package com.seafile.seadroid2.ui.wiki;

import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.wiki.Wiki1Model;
import com.seafile.seadroid2.framework.model.wiki.Wiki2Model;
import com.seafile.seadroid2.framework.model.wiki.WikiInfoModel;

import java.util.Map;

import io.reactivex.Single;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.POST;
import retrofit2.http.PUT;
import retrofit2.http.Path;

public interface WikiService {
    @GET("api/v2.1/wikis/")
    Single<Wiki1Model> getWikis();

    @GET("api/v2.1/wikis2/")
    Single<Wiki2Model> getWikis2();

    // wiki_name
    @PUT("api/v2.1/wiki2/{wiki_id}/")
    Single<ResultModel> renameWiki(@Path("wiki_id") String wikiId, @Body Map<String, Object> map);

    // publish_url
    @POST("api/v2.1/wiki2/{wiki_id}/publish/")
    Single<WikiInfoModel> publishWiki(@Path("wiki_id") String wikiId, @Body Map<String, Object> map);

    @DELETE("api/v2.1/wiki2/{wiki_id}/publish/")
    Single<ResultModel> cancelPublishWiki(@Path("wiki_id") String wikiId);

    // publish_url
    @DELETE("api/v2.1/wiki2/{wiki_id}/publish/")
    Single<String> deleteWiki(@Path("wiki_id") String wikiId);

}
