package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.media.image.PhotoFragment;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class FileProfileConfigModel implements Parcelable {
    private FileDetailModel detail;
    private MetadataConfigModel metadataConfig;

    private List<UserModel> relatedUserList = new ArrayList<>();
    private List<MetadataModel> recordMetaDataList = new ArrayList<>();// network data
    private final HashMap<String, MetadataModel> recordMetaDataMap = new HashMap<>();
    private List<Map<String, Object>> recordResultList = new ArrayList<>(); // network data
    private final HashMap<String, Object> recordResultMap = new HashMap<>();

    private Map<String, SDocTagModel> tagsMap = new HashMap<>();

    private LinkedHashMap<String, MetadataModel> linkedHashMap = new LinkedHashMap<>();


    private final List<String> _supportedField = List.of("_size", "_file_modifier", "_file_mtime", "_owner", "_description", "_collaborators", "_reviewer", "_status", "_location", "_tags", "_rate");

    public void build() {
        if (metadataConfig == null) {
            throw new RuntimeException("please first call setMetadataConfigModel()");
        }

        if (!isMetadataEnabled() || CollectionUtils.isEmpty(recordResultList)) {
            initDefaultIfMetaNotEnable();
            return;
        }

        List<MetadataModel> metadataList = new ArrayList<>(getRecordMetaDataList());
        for (MetadataModel metadata : metadataList) {
            if ("_file_modifier".equals(metadata.key)) {
                metadata.type = "collaborator";
                metadata.value = CollectionUtils.newArrayList(getResultValueByKey(metadata.name));
            } else {
                metadata.value = getResultValueByKey(metadata.name);
            }
        }

        swapSizePosition(metadataList);

        for (MetadataModel metadataModel : metadataList) {
            if (metadataModel.name.startsWith("_")) {
                if (_supportedField.contains(metadataModel.name)) {
                    linkedHashMap.put(metadataModel.name, metadataModel);
                }
            } else {
                linkedHashMap.put(metadataModel.name, metadataModel);
            }
        }
    }

    private Object getResultValueByKey(String key) {
        if (recordMetaDataMap.isEmpty()) {
            throw new RuntimeException("please first build recordMetaDataMap");
        }
        MetadataModel metadataModel = recordMetaDataMap.get(key);
        if (metadataModel == null) {
            return null;
        }

        Object obj = recordResultMap.get(metadataModel.name);
        return obj;
//        if (TextUtils.equals(ColumnType.COLLABORATOR, metadataModel.type)) {
//            List<UserModel> list = CollectionUtils.newArrayList();
//
//            if (obj instanceof ArrayList) {
//                ArrayList<String> arrayList = (ArrayList<String>) obj;
//                for (String userEmail : arrayList) {
//                    Optional<UserModel> op = getRelatedUserList().stream().filter(f -> f.getEmail().equals(userEmail)).findFirst();
//                    op.ifPresent(list::add);
//                }
//            }
//            return list;
//        } else if (TextUtils.equals(ColumnType.SINGLE_SELECT, metadataModel.type)) {
//            List<OptionsTagModel> list = CollectionUtils.newArrayList();
//
//            if (CollectionUtils.isEmpty(metadataModel.configData)) {
//                return list;
//            }
//
//            MetadataConfigDataModel configDataModel = metadataModel.configData.get(0);
//            if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
//                return list;
//            }
//
//            if (obj instanceof String value && !TextUtils.isEmpty(obj.toString())) {
//                Optional<OptionsTagModel> option = configDataModel.options
//                        .stream()
//                        .filter(f -> f.name.equals(value))
//                        .findFirst();
//                option.ifPresent(list::add);
//            }
//
//            return list;
//        } else if (TextUtils.equals(ColumnType.MULTIPLE_SELECT, metadataModel.type)) {
//            List<OptionsTagModel> list = CollectionUtils.newArrayList();
//
//            if (CollectionUtils.isEmpty(metadataModel.configData)) {
//                return list;
//            }
//
//            MetadataConfigDataModel configDataModel = metadataModel.configData.get(0);
//            if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
//                return list;
//            }
//
//            if (obj instanceof ArrayList) {
//                ArrayList<String> arrayList = (ArrayList<String>) obj;
//                for (String objKey : arrayList) {
//                    Optional<OptionsTagModel> option = configDataModel.options
//                            .stream()
//                            .filter(f -> f.name.equals(objKey))
//                            .findFirst();
//                    option.ifPresent(list::add);
//                }
//            }
//
//            return list;
//        } else if (TextUtils.equals(ColumnType.URL, metadataModel.type) && TextUtils.equals("_tags", metadataModel.key)) {
//            List<OptionsTagModel> list = CollectionUtils.newArrayList();
//
//            if (CollectionUtils.isEmpty(metadataModel.configData)) {
//                return list;
//            }
//
//            MetadataConfigDataModel configDataModel = metadataModel.configData.get(0);
//            if (configDataModel.options == null || CollectionUtils.isEmpty(configDataModel.options)) {
//                return list;
//            }
//
//            if (obj instanceof ArrayList) {
//                ArrayList<String> arrayList = (ArrayList<String>) obj;
//                for (String objKey : arrayList) {
//                    Optional<OptionsTagModel> option = configDataModel.options
//                            .stream()
//                            .filter(f -> f.name.equals(objKey))
//                            .findFirst();
//                    option.ifPresent(list::add);
//                }
//            }
//
//            return list;
//        } else if (TextUtils.equals(ColumnType.GEOLOCATION, metadataModel.type)) {
//            String content = "";
//
//            if (obj instanceof LinkedTreeMap) {
//                LinkedTreeMap<String, Object> treeMap = (LinkedTreeMap<String, Object>) obj;
//                String geo_format = metadataModel.getConfigData().geo_format;
//
//                if (TextUtils.equals("lng_lat", geo_format)) {
//                    String lat = treeMap.get("lat").toString();
//                    String lng = treeMap.get("lng").toString();
//                    String formatLat = Utils.convertLatitude(lat);
//                    String formatLng = Utils.convertLongitude(lng);
//                    content = formatLat + ", " + formatLng;
//                } else if (TextUtils.equals("geolocation", geo_format)) {
//                    String province = treeMap.get("province").toString();
//                    String city = treeMap.get("city").toString();
//                    String dis = treeMap.get("district").toString();
//                    String detail = treeMap.get("detail").toString();
//                    content = province + city + dis + detail;
//                } else if (TextUtils.equals("country_region", geo_format)) {
//                    content = treeMap.get("country_region").toString();
//                } else if (TextUtils.equals("province", geo_format)) {
//                    content = treeMap.get("province").toString();
//                } else if (TextUtils.equals("province_city", geo_format)) {
//                    String province = treeMap.get("province").toString();
//                    String city = treeMap.get("city").toString();
//                    content = province + city;
//                } else if (TextUtils.equals("province_city_district", geo_format)) {
//                    String province = treeMap.get("province").toString();
//                    String city = treeMap.get("city").toString();
//                    String dis = treeMap.get("district").toString();
//                    content = province + city + dis;
//                }
//
//                content = content.trim();
//            }
//            return content;
//        }
//
//
//        return obj;
    }


    public void setMetadataConfigModel(MetadataConfigModel metadataConfigModel) {
        this.metadataConfig = metadataConfigModel;
    }

    public boolean isMetadataEnabled() {
        if (metadataConfig == null) {
            throw new RuntimeException("please first call setMetadataConfigModel()");
        }
        return metadataConfig.enabled;
    }

    public boolean isTagsEnabled() {
        if (metadataConfig == null) {
            throw new RuntimeException("please first call setMetadataConfigModel()");
        }
        return metadataConfig.tags_enabled;
    }

    public void setFileDetail(FileDetailModel detail) {
        this.detail = detail;
    }

    public FileDetailModel getFileDetail() {
        return detail;
    }

    public void initDefaultIfMetaNotEnable() {
        if (detail == null) {
            return;
        }

        FileRecordWrapperModel recordWrapperModel = new FileRecordWrapperModel();
        //size
        MetadataModel sizeMetadataModel = new MetadataModel();
        sizeMetadataModel.key = "_size";
        sizeMetadataModel.name = "_size";
        sizeMetadataModel.type = ColumnType.NUMBER;
        recordWrapperModel.metadata.add(sizeMetadataModel);

        //modifier
        MetadataModel modifierMetadataModel = new MetadataModel();
        modifierMetadataModel.key = "_file_modifier";
        modifierMetadataModel.name = "_file_modifier";
        modifierMetadataModel.type = ColumnType.TEXT;
        recordWrapperModel.metadata.add(modifierMetadataModel);

        //mtime
        MetadataModel mTimeMetadataModel = new MetadataModel();
        mTimeMetadataModel.key = "_file_mtime";
        mTimeMetadataModel.name = "_file_mtime";
        mTimeMetadataModel.type = ColumnType.DATE;
        recordWrapperModel.metadata.add(mTimeMetadataModel);

        Map<String, Object> m = new HashMap<>();
        m.put("_size", getFileDetail().getSize());
        m.put("_file_modifier", getFileDetail().getLastModifierEmail());
        m.put("_file_mtime", getFileDetail().getLastModified());
        recordWrapperModel.results.add(m);

        addRecordWrapperModel(recordWrapperModel);

        //add a default user with last modifier user info
        UserWrapperModel wrapperModel = new UserWrapperModel();
        wrapperModel.user_list = new ArrayList<>();
        UserModel r = new UserModel();
        r.setName(getFileDetail().getLastModifierName());
        r.setAvatarUrl(getFileDetail().getLastModifierAvatar());
        r.setEmail(getFileDetail().getLastModifierEmail());
        r.setContactEmail(getFileDetail().getLastModifierContactEmail());
        wrapperModel.user_list.add(r);
        setRelatedUserList(wrapperModel);

        SLogs.d(PhotoFragment.TAG, "initDefaultIfMetaNotEnable()", "detail = " + detail.getName());
    }

    public void setRelatedUserWrapperModel(UserWrapperModel relatedUserWrapperModel) {
        this.relatedUserList.clear();
        this.relatedUserList.addAll(relatedUserWrapperModel.user_list);
    }

    private void setRelatedUserList(UserWrapperModel users) {
        this.relatedUserList.clear();
        this.relatedUserList.addAll(users.user_list);
    }

    public List<UserModel> getRelatedUserList() {
        return relatedUserList;
    }

    public void addRecordWrapperModel(FileRecordWrapperModel recordWrapperModel) {
        this.recordResultList.addAll(recordWrapperModel.results);
        if (CollectionUtils.isNotEmpty(recordWrapperModel.results)) {
            recordResultMap.putAll(recordWrapperModel.results.get(0));
        }

        List<MetadataModel> metadata = swapSizePosition(recordWrapperModel.metadata);
        this.recordMetaDataList.addAll(metadata);

        recordMetaDataMap.clear();
        for (MetadataModel metadataModel : this.recordMetaDataList) {
            recordMetaDataMap.put(metadataModel.key, metadataModel);
        }
    }

    private List<MetadataModel> swapSizePosition(List<MetadataModel> metadata) {
        if (CollectionUtils.isEmpty(metadata)) {
            return CollectionUtils.newArrayList();
        }

        int index = -1;
        for (int i = 0; i < metadata.size(); i++) {
            if (TextUtils.equals(metadata.get(i).key, "_size")) {
                index = i;
                break;
            }
        }

        if (index == -1) {
            return metadata;
        }

        if (index == 0) {
            return metadata;
        }

        MetadataModel sizeModel = metadata.get(index);
        metadata.remove(index);
        metadata.add(0, sizeModel);
        return metadata;
    }

    public void setTagWrapperModel(FileTagWrapperModel tagWrapperModel) {
        if (CollectionUtils.isEmpty(tagWrapperModel.results)) {
            return;
        }

        tagWrapperModel.results.forEach(new Consumer<FileTagResultModel>() {
            @Override
            public void accept(FileTagResultModel model) {
                SDocTagModel tagModel = new SDocTagModel();
                tagModel.id = model._id;
                tagModel.name = model._tag_name;
                tagModel.color = model._tag_color;
                tagsMap.put(tagModel.id, tagModel);
            }
        });
    }

    public List<Map<String, Object>> getRecordResultList() {
        return recordResultList;
    }

    public List<MetadataModel> getRecordMetaDataList() {
        if (CollectionUtils.isEmpty(getRecordResultList())) {
            throw new RuntimeException("please first call build()");
        }
        return recordMetaDataList;
    }

    public void setRecordMetaDataList(List<MetadataModel> list) {
        this.recordMetaDataList.clear();
        this.recordMetaDataList.addAll(list);
    }

    public Map<String, SDocTagModel> getTagMap() {
        return tagsMap;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeParcelable(this.detail, flags);
        dest.writeParcelable(this.metadataConfig, flags);
        dest.writeTypedList(this.relatedUserList);
        dest.writeTypedList(this.recordMetaDataList);
        dest.writeList(this.recordResultList);
        dest.writeInt(this.tagsMap.size());
        for (Map.Entry<String, SDocTagModel> entry : this.tagsMap.entrySet()) {
            dest.writeString(entry.getKey());
            dest.writeParcelable(entry.getValue(), flags);
        }
    }

    public FileProfileConfigModel() {
    }

    protected FileProfileConfigModel(Parcel in) {
        this.detail = in.readParcelable(FileDetailModel.class.getClassLoader());
        this.metadataConfig = in.readParcelable(MetadataConfigModel.class.getClassLoader());
        this.relatedUserList = in.createTypedArrayList(UserModel.CREATOR);
        this.recordMetaDataList = in.createTypedArrayList(MetadataModel.CREATOR);
        this.recordResultList = new ArrayList<Map<String, Object>>();
        in.readList(this.recordResultList, Map.class.getClassLoader());
        int tagsMapSize = in.readInt();
        this.tagsMap = new HashMap<String, SDocTagModel>(tagsMapSize);
        for (int i = 0; i < tagsMapSize; i++) {
            String key = in.readString();
            SDocTagModel value = in.readParcelable(SDocTagModel.class.getClassLoader());
            this.tagsMap.put(key, value);
        }
    }

    public static final Parcelable.Creator<FileProfileConfigModel> CREATOR = new Parcelable.Creator<FileProfileConfigModel>() {
        @Override
        public FileProfileConfigModel createFromParcel(Parcel source) {
            return new FileProfileConfigModel(source);
        }

        @Override
        public FileProfileConfigModel[] newArray(int size) {
            return new FileProfileConfigModel[size];
        }
    };
}

