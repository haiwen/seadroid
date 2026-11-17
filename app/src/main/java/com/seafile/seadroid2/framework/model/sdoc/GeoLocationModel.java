package com.seafile.seadroid2.framework.model.sdoc;

import android.text.TextUtils;

import org.apache.commons.lang3.StringUtils;

import io.reactivex.annotations.Nullable;
import com.google.gson.annotations.SerializedName;

public class GeoLocationModel{
    @SerializedName("geo_format")
    public String geo_format;
    @SerializedName("country")
    public String country;
    @SerializedName("province")
    public String province;
    @SerializedName("city")
    public String city;
    @SerializedName("district")
    public String district;
    @SerializedName("street")
    public String street;

    @SerializedName("address")
    public String address;
    @SerializedName("lat")
    public double lat = -1;
    @SerializedName("lng")
    public double lng = -1;

    public GeoLocationModel() {
    }


    public String getLngLat() {
        return lng + "," + lat;
    }

    public boolean isEmptyAddress() {
        if (TextUtils.isEmpty(geo_format)) {
            return true;
        }

        return lat == 0
                && lng == 0
                && TextUtils.isEmpty(country)
                && TextUtils.isEmpty(province)
                && TextUtils.isEmpty(city)
                && TextUtils.isEmpty(district)
                && TextUtils.isEmpty(street)
                && TextUtils.isEmpty(address);
    }

    @Nullable
    public String getText() {
        if (TextUtils.isEmpty(geo_format)) {
            return null;
        }

        if (!StringUtils.isEmpty(address)) {
            return address;
        }

        String c = concatField();
        if (!StringUtils.isEmpty(c)) {
            return c;
        }

        if (lat > -1 && lng > -1) {
            return lng + "," + lat;
        }

        return null;
    }

    private String concatField() {

        String d = null;
        if (!StringUtils.isEmpty(country)) {
            d = country;
        }

        if (!StringUtils.isEmpty(province)) {
            d += province;
        }
        if (!StringUtils.isEmpty(city)) {
            d += province;
        }
        if (!StringUtils.isEmpty(district)) {
            d += province;
        }
        if (!StringUtils.isEmpty(street)) {
            d += province;
        }

        return d;
    }
}
