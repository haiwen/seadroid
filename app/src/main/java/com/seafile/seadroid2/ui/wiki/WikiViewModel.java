package com.seafile.seadroid2.ui.wiki;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.config.WikiType;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.wiki.GroupWikiModel;
import com.seafile.seadroid2.framework.model.wiki.OldWikiInfoModel;
import com.seafile.seadroid2.framework.model.wiki.Wiki1Model;
import com.seafile.seadroid2.framework.model.wiki.Wiki2Model;
import com.seafile.seadroid2.framework.model.wiki.WikiGroupModel;
import com.seafile.seadroid2.framework.model.wiki.WikiInfoModel;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import io.reactivex.Single;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;

public class WikiViewModel extends BaseViewModel {
    private final MutableLiveData<List<BaseModel>> wikis = new MutableLiveData<>();

    public MutableLiveData<List<BaseModel>> getWikisLiveData() {
        return wikis;
    }

    public void loadWikis() {
        getRefreshLiveData().setValue(true);

        Single<Wiki1Model> w1Single = HttpIO.getCurrentInstance().execute(WikiService.class).getWikis();
        Single<Wiki2Model> w2Single = HttpIO.getCurrentInstance().execute(WikiService.class).getWikis2();

        Single<List<BaseModel>> single = Single.zip(w1Single, w2Single, new BiFunction<Wiki1Model, Wiki2Model, List<BaseModel>>() {
            @Override
            public List<BaseModel> apply(Wiki1Model wiki1Model, Wiki2Model wiki2Model) throws Exception {

                TreeMap<String, List<WikiInfoModel>> map = new TreeMap<String, List<WikiInfoModel>>();

                if (CollectionUtils.isNotEmpty(wiki2Model.group_wikis)) {
                    for (GroupWikiModel groupWiki : wiki2Model.group_wikis) {
                        if (CollectionUtils.isEmpty(groupWiki.wiki_info)) {
                            continue;
                        }

                        for (WikiInfoModel infoModel : groupWiki.wiki_info) {
                            infoModel.group_name = groupWiki.group_name;
                            infoModel.group_id = groupWiki.group_id;
                            infoModel.group_owner = groupWiki.owner;
                            String key = infoModel.group_id + "-" + infoModel.group_name;
                            checkAndInsert(map, key, infoModel);
                        }
                    }
                }

                String mineKey = "-1-Mine";
                String sharedKey = "-2-Shared";
                String oldKey = "-3-OldWiki";

                // mine shared
                if (CollectionUtils.isNotEmpty(wiki2Model.wikis)) {
                    for (WikiInfoModel wiki : wiki2Model.wikis) {
                        if (TextUtils.equals("mine", wiki.type)) {
                            wiki.group_name = "Mine";
                            wiki.group_id = -1;
                            wiki.group_owner = "mine";
                            checkAndInsert(map, mineKey, wiki);
                        } else if (TextUtils.equals("shared", wiki.type)) {
                            wiki.group_name = "Shared";
                            wiki.group_id = -2;
                            wiki.group_owner = "shared";

                            String key = wiki.group_id + "-" + wiki.group_name;
                            checkAndInsert(map, key, wiki);
                        }
                    }
                }

                // old wiki
                if (wiki1Model != null && CollectionUtils.isNotEmpty(wiki1Model.data)) {
                    for (OldWikiInfoModel m : wiki1Model.data) {
                        WikiInfoModel infoModel = new WikiInfoModel();
                        infoModel.group_id = -3;// old wiki
                        infoModel.group_name = "OldWiki";
                        infoModel.id = m.id + "";
                        infoModel.is_published = true;
                        infoModel.name = m.name;
                        infoModel.owner = m.owner;
                        infoModel.owner_nickname = m.owner_nickname;
                        infoModel.permission = m.permission;
                        infoModel.repo_id = m.repo_id;
                        infoModel.type = WikiType.TYPE_OLD;
                        infoModel.updated_at = m.updated_at;
                        infoModel.created_at = m.created_at;
                        infoModel.owner_avatar_url = m.owner_avatar_url;
                        infoModel.public_url = m.link;

                        String key = infoModel.group_id + "-" + infoModel.group_name;
                        checkAndInsert(map, key, infoModel);
                    }
                }


                List<BaseModel> list = CollectionUtils.newArrayList();

                // mine list
                List<WikiInfoModel> mineList = map.get(mineKey);
                if (CollectionUtils.isNotEmpty(mineList)) {
                    list.add(new WikiGroupModel(R.string.my_wiki, R.drawable.icon_my_libraries));
                    list.addAll(mineList);
                }

                // shared list
                List<WikiInfoModel> sharedList = map.get(sharedKey);
                if (CollectionUtils.isNotEmpty(sharedList)) {
                    list.add(new WikiGroupModel(R.string.my_wiki_of_shared, R.drawable.icon_shared_with_me));
                    list.addAll(sharedList);
                }

                for (String key : map.keySet()) {
                    if (TextUtils.equals(key, mineKey)) {
                        continue;
                    }

                    if (TextUtils.equals(key, sharedKey)) {
                        continue;
                    }

                    if (TextUtils.equals(key, oldKey)) {
                        continue;
                    }


                    List<WikiInfoModel> groupList = map.get(key);
                    if (CollectionUtils.isNotEmpty(groupList)) {
                        String[] ss = key.split("-");
                        if (ss.length == 2) {
                            list.add(new WikiGroupModel(ss[1], R.drawable.icon_shared_with_all));
                        } else {
                            list.add(new WikiGroupModel(key, R.drawable.icon_shared_with_all));
                        }
                        list.addAll(groupList);
                    }
                }

                // mine list
                List<WikiInfoModel> oldList = map.get(oldKey);
                if (CollectionUtils.isNotEmpty(oldList)) {
                    list.add(new WikiGroupModel(R.string.my_wiki_of_old, R.drawable.icon_shared_with_all));
                    list.addAll(oldList);
                }

                return list;
            }
        });

        addSingleDisposable(single, new Consumer<List<BaseModel>>() {
            @Override
            public void accept(List<BaseModel> models) throws Exception {
                getWikisLiveData().setValue(models);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    private void checkAndInsert(TreeMap<String, List<WikiInfoModel>> map, String groupName, WikiInfoModel infoModel) {
        if (TextUtils.isEmpty(groupName)) {
            return;
        }

        List<WikiInfoModel> list = null;
        if (map.containsKey(groupName)) {
            list = map.getOrDefault(groupName, CollectionUtils.newArrayList());
        } else {
            list = CollectionUtils.newArrayList();
        }
        list.add(infoModel);
        map.put(groupName, list);
    }

    public void publishWiki(String wikiId, String publishUrl) {
        getRefreshLiveData().setValue(true);

        Map<String, Object> map = new HashMap<>();
        map.put("publish_url", publishUrl);

        Single<WikiInfoModel> single = HttpIO.getCurrentInstance().execute(WikiService.class).publishWiki(wikiId, map);
        addSingleDisposable(single, new Consumer<WikiInfoModel>() {
            @Override
            public void accept(WikiInfoModel wikiInfoModel) throws Exception {
                getRefreshLiveData().setValue(false);
                getSeafExceptionLiveData().setValue(SeafException.SUCCESS);
            }
        });

    }


    public void cancelPublishWiki(String wikiId) {
        getRefreshLiveData().setValue(true);

        Single<ResultModel> single = HttpIO.getCurrentInstance().execute(WikiService.class).cancelPublishWiki(wikiId);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);
                if (resultModel.success) {
                    getSeafExceptionLiveData().setValue(SeafException.SUCCESS);
                }
            }
        });

    }

    public void renameWiki(String wikiId, String wiki_name) {
        getRefreshLiveData().setValue(true);

        Map<String, Object> map = new HashMap<>();
        map.put("wiki_name", wiki_name);

        Single<ResultModel> single = HttpIO.getCurrentInstance().execute(WikiService.class).renameWiki(wikiId, map);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);
                if (resultModel.success) {
                    getSeafExceptionLiveData().setValue(SeafException.SUCCESS);
                }
            }
        });

    }

    public void deleteWiki(String wikiId) {
        getRefreshLiveData().setValue(true);

        Single<String> single = HttpIO.getCurrentInstance().execute(WikiService.class).deleteWiki(wikiId);
        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String result) throws Exception {
                getRefreshLiveData().setValue(false);
                getSeafExceptionLiveData().setValue(SeafException.SUCCESS);
            }
        });
    }


}
