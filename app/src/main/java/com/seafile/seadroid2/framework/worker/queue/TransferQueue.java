package com.seafile.seadroid2.framework.worker.queue;

import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.enums.TransferStatus;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.function.Function;
import java.util.stream.Collectors;

public class TransferQueue {

    /**
     * if false, _uploadMultiMap
     */
    private final boolean isEnableCategory;

    public TransferQueue() {
        this.isEnableCategory = false;
    }

    public TransferQueue(boolean isEnableCategory) {
        this.isEnableCategory = isEnableCategory;
    }


    /**
     * enable category, like this:
     * <pre>
     * [
     *   {bucketName1, [transfer_model_id_1,transfer_model_id_2]},
     *   {bucketName2, [transfer_model_id_3,transfer_model_id_4]}
     * ]
     * </pre>
     */
    private final ConcurrentHashMap<String, Set<String>> _category_map = new ConcurrentHashMap<>();

    private ConcurrentHashMap<String, Set<String>> getCategoryMap() {
        return _category_map;
    }

    public Set<String> getCategoryKeys() {
        if (!isEnableCategory) {
            throw new IllegalArgumentException("isEnableCategory = true, can't use this method.");
        }
        return getCategoryMap().keySet();
    }

    public List<TransferModel> getCategoryTransferList(String key) {
        if (!isEnableCategory) {
            throw new IllegalArgumentException("isEnableCategory = false, can't use this method.");
        }

        List<TransferModel> list = new ArrayList<>();

        Set<String> sets = getCategoryMap().get(key);
        if (CollectionUtils.isEmpty(sets)) {
            return list;
        }

        return sets.stream()
                .map(getTransferMap()::get)
                .collect(Collectors.toList());
    }


    /**
     * cache UploadModel obj, like this:
     * <pre>
     * [
     *  {transfer_model_id_1, transfer_model1},
     *  {transfer_model_id_2, transfer_model2},
     *  {transfer_model_id_3, transfer_model3}
     *  {transfer_model_id_4, transfer_model4}
     * ]
     * </pre>
     */
    private final ConcurrentHashMap<String, TransferModel> _transfer_map = new ConcurrentHashMap<>();

    public ConcurrentHashMap<String, TransferModel> getTransferMap() {
        return _transfer_map;
    }

    public List<TransferModel> getSortedTransferMapList() {
        if (getTransferMap().isEmpty()) {
            return new ArrayList<>();
        }

        return getTransferMap()
                .values()
                .stream()
                .sorted()
                .collect(Collectors.toList());
    }

    public List<TransferModel> getSortedTransferQueueList() {
        if (getTransferQueue().isEmpty()) {
            return new ArrayList<>();
        }

        return getTransferQueue().stream().map(new Function<String, TransferModel>() {
                    @Override
                    public TransferModel apply(String s) {
                        return getTransferMap().get(s);
                    }
                })
                .sorted()
                .collect(Collectors.toList());
    }


    /**
     * transferModel's id queue, like this:
     * <pre>
     * [
     *   transfer_model_id_1,
     *   transfer_model_id_2,
     *   transfer_model_id_3,
     *   transfer_model_id_4
     * ]
     * </pre>
     */
    private final ConcurrentLinkedDeque<String> _transfer_queue = new ConcurrentLinkedDeque<>();

    private ConcurrentLinkedDeque<String> getTransferQueue() {
        return _transfer_queue;
    }
    public int getTotalCount() {
        return getTransferMap().size();
    }

    public int getPendingCount() {
        return getTransferQueue().size();
    }

    public synchronized void update(TransferModel model) {
        if (model == null) {
            return;
        }

        if (!getTransferMap().containsKey(model.getId())) {
            return;
        }

        getTransferMap().put(model.getId(), model);
    }

    public TransferModel getById(String id) {
        return getTransferMap().get(id);
    }

    public TransferModel pick() {
        return pick(false);
    }

    public synchronized TransferModel pick(boolean isRemoveFromMap) {
        String uploadId = getTransferQueue().pollFirst();
        if (TextUtils.isEmpty(uploadId)) {
            return null;
        }

        TransferModel transferModel = getTransferMap().get(uploadId);
        if (transferModel == null) {
            return null;
        }

        transferModel.transfer_status = TransferStatus.IN_PROGRESS;

        if (isRemoveFromMap) {
            getTransferMap().remove(transferModel.getId());
        } else {
            //update
            update(transferModel);
        }

        return transferModel;
    }


    /**
     * @param categoryKey bucketName
     */
    public void remove(String categoryKey, TransferModel model) {
        if (model == null) {
            return;
        }

        Set<String> uploadKeys = getCategoryMap().get(categoryKey);
        if (uploadKeys == null) {
            return;
        }

        //remove old
        uploadKeys.remove(model.getId());
        getCategoryMap().put(categoryKey, uploadKeys);

        getTransferMap().remove(model.getId());

        if (getTransferQueue().isEmpty()) {
            return;
        }

        boolean e = getTransferQueue().stream().anyMatch(a -> TextUtils.equals(model.getId(), a));
        if (!e) {
            return;
        }

        List<String> li = getTransferQueue().stream()
                .filter(f -> !TextUtils.equals(model.getId(), f))
                .collect(Collectors.toList());

        addAllIntoQueueClearOld(li);
    }

    public void remove(String modelId) {
        if (TextUtils.isEmpty(modelId)) {
            return;
        }

        TransferModel transferModel = getTransferMap().get(modelId);
        if (transferModel == null) {
            return;
        }

        remove(transferModel);
    }

    public void remove(TransferModel model) {
        if (model == null) {
            return;
        }

        if (getTransferMap().containsKey(model.getId())) {
            getTransferMap().remove(model.getId());
        }

        if (getTransferQueue().isEmpty()) {
            return;
        }

        boolean e = getTransferQueue().stream().anyMatch(a -> TextUtils.equals(model.getId(), a));
        if (!e) {
            return;
        }

        List<String> li = getTransferQueue().stream()
                .filter(f -> !TextUtils.equals(model.getId(), f))
                .collect(Collectors.toList());

        addAllIntoQueueClearOld(li);
    }

    /**
     * transfer model will overwrite the old one if it exists
     *
     * @param model transfer model
     */
    public void put(TransferModel model) {
        if (isEnableCategory) {
            throw new IllegalArgumentException("isEnableCategory = true, please use another put()");
        }

        if (model == null) {
            return;
        }

        if (TextUtils.isEmpty(model.getId())) {
            throw new IllegalArgumentException("model.id is empty");
        }

        //remove old if existed
        remove(model);

        getTransferMap().put(model.getId(), model);
        //add queue if not existed
        if (!getTransferQueue().contains(model.getId())) {
            getTransferQueue().addLast(model.getId());
        }
    }

    /**
     * transfer model will overwrite the old one if it exists
     *
     * @param categoryKey bucketName
     * @param model       transfer model
     */
    public void put(String categoryKey, TransferModel model) {
        if (!isEnableCategory) {
            throw new IllegalArgumentException("isEnableCategory = false, please use another put()");
        }

        if (model == null) {
            return;
        }

        if (TextUtils.isEmpty(categoryKey)) {
            return;
        }

        if (TextUtils.isEmpty(model.getId())) {
            throw new IllegalArgumentException("model.id is empty");
        }

        //remove old if existed
        remove(categoryKey, model);

        Set<String> uploadKeys = getCategoryMap().get(categoryKey);
        if (uploadKeys == null) {
            uploadKeys = new HashSet<>();
        }

        uploadKeys.add(model.getId());

        //put new
        getCategoryMap().put(categoryKey, uploadKeys);
        getTransferMap().put(model.getId(), model);
        //add queue if not existed
        if (!getTransferQueue().contains(model.getId())) {
            getTransferQueue().addLast(model.getId());
        }
    }

    private void addAllIntoQueueClearOld(List<String> list) {
        getTransferQueue().clear();
        addAllIntoQueue(list);
    }

    private void addAllIntoQueue(List<String> list) {
        for (String modelId : list) {
            getTransferQueue().addLast(modelId);
        }
    }

    public void clear() {
        getCategoryMap().clear();
        getTransferMap().clear();
        getTransferQueue().clear();
    }

    public boolean isQueueEmpty() {
        return getTransferQueue().isEmpty();
    }
}
