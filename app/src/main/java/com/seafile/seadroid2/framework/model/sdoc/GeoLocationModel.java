package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import io.reactivex.annotations.Nullable;

public class GeoLocationModel implements Parcelable {
    public String geo_format;
    public String country_region;
    public String province;
    public String city;
    public String district;
    public String detail;

    public String address;
    public double lng;
    public double lat;
//    public AddressLngLatModel lngLat;

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
//                && lngLat == null
                && TextUtils.isEmpty(country_region)
                && TextUtils.isEmpty(province)
                && TextUtils.isEmpty(city)
                && TextUtils.isEmpty(district)
                && TextUtils.isEmpty(detail)
                && TextUtils.isEmpty(address);
    }

    @Nullable
    public String getText() {
        if (TextUtils.isEmpty(geo_format)) {
            return null;
        }

        switch (geo_format) {
            case "province":
                return province;
            case "province_city":
                return province + " " + city;
            case "province_city_district":
                return province + " " + city + " " + district;
            case "geolocation":
                return (province + " " + city + " " + district + " " + detail).trim();
            case "country_region":
                return country_region;
            case "lng_lat":
                return lng + "," + lat;
            case "map_selection":
                return address;
        }
        return null;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.geo_format);
        dest.writeString(this.country_region);
        dest.writeString(this.province);
        dest.writeString(this.city);
        dest.writeString(this.district);
        dest.writeString(this.detail);
        dest.writeString(this.address);
        dest.writeDouble(this.lng);
        dest.writeDouble(this.lat);
//        dest.writeParcelable(this.lngLat, flags);
    }

    public void readFromParcel(Parcel source) {
        this.geo_format = source.readString();
        this.country_region = source.readString();
        this.province = source.readString();
        this.city = source.readString();
        this.district = source.readString();
        this.detail = source.readString();
        this.address = source.readString();
        this.lng = source.readDouble();
        this.lat = source.readDouble();
//        this.lngLat = source.readParcelable(AddressLngLatModel.class.getClassLoader());
    }

    protected GeoLocationModel(Parcel in) {
        this.geo_format = in.readString();
        this.country_region = in.readString();
        this.province = in.readString();
        this.city = in.readString();
        this.district = in.readString();
        this.detail = in.readString();
        this.address = in.readString();
        this.lng = in.readDouble();
        this.lat = in.readDouble();
//        this.lngLat = in.readParcelable(AddressLngLatModel.class.getClassLoader());
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
