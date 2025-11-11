package com.seafile.seadroid2.ui.search;

import com.seafile.seadroid2.framework.model.search.SearchWrapperModel;

import java.util.Map;

import io.reactivex.Single;
import retrofit2.http.Body;
import retrofit2.http.GET;
import retrofit2.http.POST;
import retrofit2.http.Query;

public interface SearchService {

    //search
    @GET("api2/search/")
    Single<SearchWrapperModel> search(@Query("search_repo") String repoId,
                                      @Query("q") String q,
                                      @Query("search_type") String searchType,
                                      @Query("page") int pageNo,
                                      @Query("per_page") int pageSize
    );

    //search-file
    @GET("api/v2.1/search-file")
    Single<SearchWrapperModel> searchFile(@Query("search_repo") String repoId,
                                      @Query("q") String q,
                                      @Query("search_type") String searchType,
                                      @Query("page") int pageNo,
                                      @Query("per_page") int pageSize
    );

    @POST("api/v2.1/ai/search/")
    Single<SearchWrapperModel> search(@Body Map<String, String> m);

}
