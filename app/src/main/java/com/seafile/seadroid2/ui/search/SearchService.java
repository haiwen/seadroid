package com.seafile.seadroid2.ui.search;

import com.seafile.seadroid2.data.model.search.SearchWrapperModel;

import io.reactivex.Single;
import retrofit2.http.GET;
import retrofit2.http.Query;

public interface SearchService {

    @GET("api2/search/")
    Single<SearchWrapperModel> search(@Query("search_repo") String repoId,
                                      @Query("q") String q,
                                      @Query("search_type") String searchType,
                                      @Query("page") int pageNo,
                                      @Query("per_page") int pageSize
    );

}
