package com.seafile.seadroid2.ui.activities;

import com.seafile.seadroid2.framework.data.model.activities.ActivityWrapperModel;

import io.reactivex.Single;
import retrofit2.http.GET;
import retrofit2.http.Query;

public interface ActivityService {
    @GET("api/v2.1/activities/?avatar_size=72")
    Single<ActivityWrapperModel> getActivities(@Query("page") int page);
}
