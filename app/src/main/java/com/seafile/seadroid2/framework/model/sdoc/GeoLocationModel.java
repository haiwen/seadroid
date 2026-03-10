package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import org.apache.commons.lang3.StringUtils;

import io.reactivex.annotations.Nullable;

public class GeoLocationModel implements Parcelable {
    public String geo_format;
    public String country;
    public String province;
    public String city;
    public String district;
    public String street;

    public String address;
    public double lat = -1;
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

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.geo_format);
        dest.writeString(this.country);
        dest.writeString(this.province);
        dest.writeString(this.city);
        dest.writeString(this.district);
        dest.writeString(this.street);
        dest.writeString(this.address);
        dest.writeDouble(this.lng);
        dest.writeDouble(this.lat);
    }


    protected GeoLocationModel(Parcel in) {
        this.geo_format = in.readString();
        this.country = in.readString();
        this.province = in.readString();
        this.city = in.readString();
        this.district = in.readString();
        this.street = in.readString();
        this.address = in.readString();
        this.lng = in.readDouble();
        this.lat = in.readDouble();
    }

    public static final Creator<GeoLocationModel> CREATOR = new Creator<GeoLocationModel>() {
        @Override
        public GeoLocationModel createFromParcel(Parcel source) {
            return new GeoLocationModel(source);
        }

        @Override
        public GeoLocationModel[] newArray(int size) {
            return new GeoLocationModel[size];
        }
    };
}
