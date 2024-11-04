package com.seafile.seadroid2.ui.repo;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.DiffUtil;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.EncodeUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemAccountBinding;
import com.seafile.seadroid2.databinding.ItemDirentBinding;
import com.seafile.seadroid2.databinding.ItemDirentGalleryBinding;
import com.seafile.seadroid2.databinding.ItemDirentGridBinding;
import com.seafile.seadroid2.databinding.ItemGroupItemBinding;
import com.seafile.seadroid2.databinding.ItemRepoBinding;
import com.seafile.seadroid2.databinding.ItemUnsupportedBinding;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.RepoSelectType;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.GroupItemModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.framework.util.GlideRequests;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.viewholder.GroupItemViewHolder;
import com.seafile.seadroid2.widget.AnimatedStateListDrawableCompatUtils;

import org.checkerframework.checker.units.qual.A;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class RepoQuickAdapter extends BaseMultiAdapter<BaseModel> {
    private final String SERVER = HttpIO.getCurrentInstance().getServerUrl();

    private boolean actionModeOn;

    private boolean repoEncrypted = false;
    private FileViewType fileViewType = FileViewType.LIST;
    private RepoSelectType selectType = RepoSelectType.NOT_SELECTABLE;

    /**
     * <pre>
     *  -1 no limited
     *   0 do not write this value
     * >=1 max count
     * </pre>
     */
    private int selectedMaxCount = 1;

    public void setSelectType(RepoSelectType selectType) {
        this.selectType = selectType;
    }

    public void setSelectType(RepoSelectType selectType, int selectedMaxCount) {
        this.selectType = selectType;
        this.selectedMaxCount = selectedMaxCount;
    }

    private Drawable starDrawable;

    public Drawable getStarDrawable() {
        if (null == starDrawable) {
            int DP_16 = SizeUtils.dp2px(12);
            starDrawable = ContextCompat.getDrawable(getContext(), R.drawable.ic_star_32);
            starDrawable.setBounds(0, 0, DP_16, DP_16);
            starDrawable.setTint(ContextCompat.getColor(getContext(), R.color.light_grey));
        }
        return starDrawable;
    }

    public void setRepoEncrypted(boolean repoEncrypted) {
        this.repoEncrypted = repoEncrypted;
    }

    public void setFileViewType(FileViewType fileViewType) {
        this.fileViewType = fileViewType;
    }

    public RepoQuickAdapter() {
        addItemType(AbsLayoutItemType.ACCOUNT, new OnMultiItem<BaseModel, AccountViewHolder>() {
            @NonNull
            @Override
            public AccountViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemAccountBinding binding = ItemAccountBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new AccountViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull AccountViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
                onBindAccount(viewHolder, baseModel);
            }
        }).addItemType(AbsLayoutItemType.GROUP_ITEM, new OnMultiItemAdapterListener<BaseModel, GroupItemViewHolder>() {
            @NonNull
            @Override
            public GroupItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemGroupItemBinding binding = ItemGroupItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new GroupItemViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull GroupItemViewHolder holder, int i, @Nullable BaseModel groupTimeModel) {
                onBind(holder, i, groupTimeModel, null);
            }

            @Override
            public void onBind(@NonNull GroupItemViewHolder holder, int position, @Nullable BaseModel item, @NonNull List<?> payloads) {
                onBindGroup(position, holder, (GroupItemModel) item, payloads);
            }
        }).addItemType(AbsLayoutItemType.REPO, new OnMultiItem<BaseModel, RepoViewHolder>() {
            @NonNull
            @Override
            public RepoViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemRepoBinding binding = ItemRepoBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new RepoViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull RepoViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
                onBindRepos(viewHolder, (RepoModel) baseModel);
            }
        }).addItemType(AbsLayoutItemType.DIRENT_LIST, new OnMultiItem<BaseModel, DirentViewHolder>() {
            @NonNull
            @Override
            public DirentViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemDirentBinding binding = ItemDirentBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new DirentViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull DirentViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
                onBindDirents(viewHolder, (DirentModel) baseModel);
            }
        }).addItemType(AbsLayoutItemType.DIRENT_GRID, new OnMultiItem<BaseModel, DirentGridViewHolder>() {
            @NonNull
            @Override
            public DirentGridViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemDirentGridBinding binding = ItemDirentGridBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new DirentGridViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull DirentGridViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
                onBindDirentsGrid(viewHolder, (DirentModel) baseModel);
            }
        }).addItemType(AbsLayoutItemType.DIRENT_GALLERY, new OnMultiItem<BaseModel, DirentGalleryViewHolder>() {
            @NonNull
            @Override
            public DirentGalleryViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemDirentGalleryBinding binding = ItemDirentGalleryBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new DirentGalleryViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull DirentGalleryViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
                onBindDirentsGallery(viewHolder, (DirentModel) baseModel);
            }
        }).addItemType(AbsLayoutItemType.NOT_SUPPORTED, new OnMultiItem<BaseModel, UnsupportedViewHolder>() {
            @NonNull
            @Override
            public UnsupportedViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemUnsupportedBinding binding = ItemUnsupportedBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new UnsupportedViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull UnsupportedViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {
            }
        }).onItemViewType(new OnItemViewTypeListener<BaseModel>() {
            @Override
            public int onItemViewType(int i, @NonNull List<? extends BaseModel> list) {
                if (list.get(i) instanceof GroupItemModel) {
                    return AbsLayoutItemType.GROUP_ITEM;
                } else if (list.get(i) instanceof RepoModel) {
                    return AbsLayoutItemType.REPO;
                } else if (list.get(i) instanceof DirentModel) {
                    if (FileViewType.LIST == fileViewType) {
                        return AbsLayoutItemType.DIRENT_LIST;
                    } else if (FileViewType.GRID == fileViewType) {
                        return AbsLayoutItemType.DIRENT_GRID;
                    } else {
                        return AbsLayoutItemType.DIRENT_GALLERY;
                    }
                } else if (list.get(i) instanceof Account) {
                    return AbsLayoutItemType.ACCOUNT;
                }
                return AbsLayoutItemType.NOT_SUPPORTED;
            }
        });
    }

    private void onBindAccount(AccountViewHolder holder, BaseModel model) {
        holder.binding.getRoot().setBackground(null);

        Account account = (Account) model;

        holder.binding.listItemAccountTitle.setText(account.getServerHost());
        holder.binding.listItemAccountSubtitle.setText(account.getName());

        if (TextUtils.isEmpty(account.avatar_url)) {
            holder.binding.listItemAccountIcon.setImageResource(R.drawable.default_avatar);
        } else {
            GlideApp.with(getContext())
                    .load(account.avatar_url)
                    .apply(GlideLoadConfig.getAvatarOptions())
                    .into(holder.binding.listItemAccountIcon);
        }

        if (selectType.ordinal() >= RepoSelectType.ONLY_ACCOUNT.ordinal()) {
            holder.binding.itemSelectView.setVisibility(account.is_checked ? View.VISIBLE : View.INVISIBLE);
        } else {
            holder.binding.itemSelectView.setVisibility(View.INVISIBLE);
        }
    }

    private void onBindGroup(int position, GroupItemViewHolder holder, GroupItemModel model, List<?> payloads) {

//        if (!CollectionUtils.isEmpty(payloads)) {
//            Bundle bundle = (Bundle) payloads.get(0);
//            boolean isChecked = bundle.getBoolean("is_checked");
//
//            holder.binding.listSeparatorItemActionText.setRotation(isChecked ? 90 : 270);
//            return;
//        }

        if (!TextUtils.isEmpty(model.title)) {
            if ("Organization".equals(model.title)) {
                holder.binding.itemGroupTitle.setText(R.string.shared_with_all);
            } else {
                holder.binding.itemGroupTitle.setText(model.title);
            }
        }

//        holder.binding.listSeparatorItemActionText.setRotation(model.is_checked ? 90 : 270);
    }

    private void onBindRepos(RepoViewHolder holder, RepoModel model) {
        holder.binding.itemTitle.setText(model.repo_name);
        holder.binding.itemSubtitle.setText(model.getSubtitle());
        holder.binding.itemIcon.setImageResource(model.getIcon());

        if (selectType.ordinal() >= RepoSelectType.ONLY_REPO.ordinal()) {
            holder.binding.itemSelectView.setVisibility(model.is_checked ? View.VISIBLE : View.INVISIBLE);
            holder.binding.expandableToggleButton.setVisibility(View.INVISIBLE);
        } else {
            holder.binding.expandableToggleButton.setVisibility(View.VISIBLE);
            holder.binding.itemSelectView.setVisibility(View.GONE);
        }

        holder.binding.itemTitle.setCompoundDrawablePadding(Constants.DP.DP_4);
        if (model.starred) {
            holder.binding.itemTitle.setCompoundDrawables(null, null, getStarDrawable(), null);
        } else {
            holder.binding.itemTitle.setCompoundDrawables(null, null, null, null);
        }
    }

    private void onBindDirents(DirentViewHolder holder, DirentModel model) {
        holder.binding.itemTitle.setText(model.name);
        holder.binding.itemSubtitle.setText(model.getSubtitle());

        holder.binding.getRoot().setBackground(AnimatedStateListDrawableCompatUtils.createDrawableCompat(getContext()));

        if (repoEncrypted || !Utils.isViewableImage(model.name)) {
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
            loadImage(model, holder.binding.itemIcon);
        }

        //action mode
        if (actionModeOn) {
            holder.binding.itemMultiSelect.setVisibility(View.VISIBLE);

            holder.binding.getRoot().setChecked(model.is_checked);

            if (model.is_checked) {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_checked);
            } else {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
            }
        } else {
            holder.binding.itemMultiSelect.setVisibility(View.GONE);
            holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);

            holder.binding.getRoot().setChecked(false);

        }

        holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
        holder.binding.itemDownloadStatus.setVisibility(View.GONE);

        if (selectType.ordinal() < RepoSelectType.ONLY_ACCOUNT.ordinal()) {
            holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.item_title_color));
            holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.item_subtitle_color));

            holder.binding.expandableToggleButton.setVisibility(View.VISIBLE);
        } else {

            if (!model.isDir()) {
                holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.light_grey));
                holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.light_grey));
            } else {
                holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.item_title_color));
                holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.item_subtitle_color));
            }

            holder.binding.expandableToggleButton.setVisibility(View.INVISIBLE);
        }

        if (model.isDir()) {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
            holder.binding.itemDownloadStatus.setVisibility(View.GONE);
        } else if (TransferStatus.WAITING == model.transfer_status ||
                TransferStatus.IN_PROGRESS == model.transfer_status) {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.VISIBLE);
            holder.binding.itemDownloadStatus.setVisibility(View.VISIBLE);
        } else if (TransferStatus.CANCELLED == model.transfer_status ||
                TransferStatus.FAILED == model.transfer_status) {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
            holder.binding.itemDownloadStatus.setVisibility(View.GONE);
        } else if (TransferStatus.SUCCEEDED == model.transfer_status) {
            if (FileUtils.isFileExists(model.local_file_path)) {
                holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
                holder.binding.itemDownloadStatus.setVisibility(View.VISIBLE);
            } else {
                holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
                holder.binding.itemDownloadStatus.setVisibility(View.GONE);
            }
        } else {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
            holder.binding.itemDownloadStatus.setVisibility(View.GONE);
        }

        if (model.starred) {
            holder.binding.itemTitle.setCompoundDrawables(null, null, getStarDrawable(), null);
        } else {
            holder.binding.itemTitle.setCompoundDrawables(null, null, null, null);
        }
    }

    private void onBindDirentsGrid(DirentGridViewHolder holder, DirentModel model) {
        holder.binding.itemTitle.setText(model.name);

        holder.binding.getRoot().setBackground(AnimatedStateListDrawableCompatUtils.createDrawableCompat(getContext()));

        if (model.isDir()) {
            holder.binding.itemOutline.setVisibility(View.GONE);
        } else {
            holder.binding.itemOutline.setVisibility(View.VISIBLE);
        }

        if (repoEncrypted || !Utils.isViewableImage(model.name)) {
            holder.binding.itemIcon.setScaleType(ImageView.ScaleType.FIT_CENTER);
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
            holder.binding.itemIcon.setScaleType(ImageView.ScaleType.CENTER_CROP);
            loadImage(model, holder.binding.itemIcon);
        }

        //action mode
        if (actionModeOn) {
            holder.binding.itemMultiSelect.setVisibility(View.VISIBLE);
            holder.binding.getRoot().setChecked(model.is_checked);

            if (model.is_checked) {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_checked);
            } else {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
            }
        } else {
            holder.binding.itemMultiSelect.setVisibility(View.GONE);
            holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);

            holder.binding.getRoot().setChecked(false);
        }

        if (model.starred) {
            holder.binding.itemTitle.setCompoundDrawables(null, null, getStarDrawable(), null);
        } else {
            holder.binding.itemTitle.setCompoundDrawables(null, null, null, null);
        }
    }

    private void onBindDirentsGallery(DirentGalleryViewHolder holder, DirentModel model) {
//        holder.binding.itemTitle.setText(model.name);

        holder.binding.getRoot().setBackground(AnimatedStateListDrawableCompatUtils.createDrawableCompat(getContext()));

        if (repoEncrypted || !Utils.isViewableImage(model.name)) {
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
            loadImage(model, holder.binding.itemIcon);
        }

        //action mode
        if (actionModeOn) {
            holder.binding.itemMultiSelect.setVisibility(View.VISIBLE);
            holder.binding.getRoot().setChecked(model.is_checked);

            if (model.is_checked) {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_checked);
            } else {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
            }
        } else {
            holder.binding.getRoot().setChecked(false);

            holder.binding.itemMultiSelect.setVisibility(View.GONE);
            holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
        }
    }

    private void loadImage(DirentModel direntModel, ImageView imageView) {

        if (direntModel.name.toLowerCase().endsWith(".gif")) {
            imageView.setImageResource(direntModel.getIcon());
        } else {
            String url = convertThumbnailUrl(direntModel);
            GlideApp.with(getContext()).load(url)
                    .apply(GlideLoadConfig.getOptions())
                    .into(imageView);
        }


    }

    private String convertThumbnailUrl(DirentModel direntModel) {
        return convertThumbnailUrl(direntModel, 128);
    }

    private String convertMiddleUrl(DirentModel direntModel) {
        return convertThumbnailUrl(direntModel, 256);
    }

    private String convertThumbnailUrl(DirentModel direntModel, int size) {
        String newFilePath = EncodeUtils.urlEncode(direntModel.full_path);
        return String.format(Locale.ROOT, "%sapi2/repos/%s/thumbnail/?p=%s&size=%d", SERVER, direntModel.repo_id, newFilePath, size);
    }

    public void setActionModeOn(boolean actionModeOn) {
        this.actionModeOn = actionModeOn;

        if (!actionModeOn) {
            setItemSelected(false);
        }

        notifyItemRangeChanged(0, getItemCount());
    }

    public boolean getActionMode() {
        return actionModeOn;
    }

    public void setItemSelected(boolean itemSelected) {
        for (BaseModel item : getItems()) {
            if (item instanceof DirentModel) {
                DirentModel model = (DirentModel) item;
                model.is_checked = itemSelected;
            }
        }

        notifyItemRangeChanged(0, getItemCount());
    }

    public List<DirentModel> getSelectedList() {
        List<DirentModel> list = new ArrayList<>();
        for (BaseModel item : getItems()) {
            if (item instanceof DirentModel model) {
                if (model.is_checked) {
                    list.add(model);
                }
            }
        }
        return list;
    }

    /**
     * @return is selected?
     */
    public boolean selectItemByMode(int position) {
        BaseModel item = getItems().get(position);

        //single
        if (selectedMaxCount == 1) {
            int selectedPosition = getSelectedPositionByMode();
            if (selectedPosition == position) {
                item.is_checked = !item.is_checked;
                notifyItemChanged(selectedPosition);
                return item.is_checked;

            } else if (selectedPosition > -1) {
                //Deselect an item that has already been selected
                getItems().get(selectedPosition).is_checked = false;
                notifyItemChanged(selectedPosition);

                item.is_checked = true;
                notifyItemChanged(position);
            } else {
                item.is_checked = true;
                notifyItemChanged(position);
            }
        } else {
            long selectedCount = getSelectedCountBySelectType();
            if (selectedCount >= selectedMaxCount) {
                return false;
            }

            item.is_checked = !item.is_checked;
            notifyItemChanged(position);

            return item.is_checked;
        }

        return true;
    }

    private long getSelectedCountBySelectType() {
        if (RepoSelectType.ONLY_ACCOUNT == selectType) {
            return getItems().stream()
                    .filter(Account.class::isInstance)
                    .filter(f -> f.is_checked)
                    .count();
        } else if (RepoSelectType.ONLY_REPO == selectType) {
            return getItems().stream()
                    .filter(RepoModel.class::isInstance)
                    .filter(f -> f.is_checked)
                    .count();
        }
        return 0;
    }

    private int getSelectedPositionByMode() {
        for (int i = 0; i < getItems().size(); i++) {
            if (getItems().get(i).is_checked) {
                return i;
            }
        }
        return -1;
    }

    public void notifySearchChanged(String searchContent) {
        this.searchContent = searchContent;

        if (CollectionUtils.isEmpty(finalList)) {
            submitList(finalList);
            return;
        }

        List<BaseModel> filterList;
        Predicate<? super BaseModel> predicate = getSearchFilter();
        if (predicate != null) {
            filterList = finalList.stream().filter(predicate).collect(Collectors.toList());
        } else {
            filterList = finalList;
        }
        notify(filterList);
    }

    private List<BaseModel> finalList;

    public void notifyDataChanged(List<BaseModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            finalList = null;
            submitList(null);
            return;
        }

        finalList = new ArrayList<>(list);
        if (CollectionUtils.isEmpty(getItems())) {
            submitList(finalList);
            return;
        }

        notify(finalList);
    }

    private void notify(List<BaseModel> list) {
        final List<BaseModel> newList = list;
        final DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffUtil.Callback() {
            @Override
            public int getOldListSize() {
                return getItems().size();
            }

            @Override
            public int getNewListSize() {
                return newList.size();
            }

            @Override
            public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
                String oldClassName = getItems().get(oldItemPosition).getClass().getName();
                String newClassName = newList.get(newItemPosition).getClass().getName();
                if (!oldClassName.equals(newClassName)) {
                    return false;
                }

                if (getItems().get(oldItemPosition) instanceof Account) {
                    Account newT = (Account) getItems().get(oldItemPosition);
                    Account oldT = (Account) newList.get(newItemPosition);
                    if (!TextUtils.equals(newT.email, oldT.email)) {
                        return false;
                    }
                }

                if (getItems().get(oldItemPosition) instanceof RepoModel) {
                    RepoModel newT = (RepoModel) getItems().get(oldItemPosition);
                    RepoModel oldT = (RepoModel) newList.get(newItemPosition);
                    if (!TextUtils.equals(newT.repo_id, oldT.repo_id) || newT.group_id != oldT.group_id) {
                        return false;
                    }
                }

                if (getItems().get(oldItemPosition) instanceof DirentModel) {
                    DirentModel newT = (DirentModel) getItems().get(oldItemPosition);
                    DirentModel oldT = (DirentModel) newList.get(newItemPosition);
                    return TextUtils.equals(newT.full_path, oldT.full_path);
                }

                return true;
            }

            @Override
            public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
                String oldClassName = getItems().get(oldItemPosition).getClass().getName();
                String newClassName = newList.get(newItemPosition).getClass().getName();
                if (!oldClassName.equals(newClassName)) {
                    return false;
                }

                if (getItems().get(oldItemPosition) instanceof RepoModel) {
                    RepoModel newT = (RepoModel) getItems().get(oldItemPosition);
                    RepoModel oldT = (RepoModel) newList.get(newItemPosition);

                    return TextUtils.equals(newT.repo_id, oldT.repo_id)
                            && TextUtils.equals(newT.repo_name, oldT.repo_name)
                            && TextUtils.equals(newT.type, oldT.type)
                            && TextUtils.equals(newT.group_name, oldT.group_name)
                            && TextUtils.equals(newT.owner_name, oldT.owner_name)
                            && TextUtils.equals(newT.owner_email, oldT.owner_email)
                            && TextUtils.equals(newT.owner_contact_email, oldT.owner_contact_email)
                            && TextUtils.equals(newT.modifier_email, oldT.modifier_email)
                            && TextUtils.equals(newT.modifier_name, oldT.modifier_name)
                            && TextUtils.equals(newT.modifier_contact_email, oldT.modifier_contact_email)
                            && TextUtils.equals(newT.permission, oldT.permission)
                            && TextUtils.equals(newT.salt, oldT.salt)
                            && TextUtils.equals(newT.status, oldT.status)
                            && TextUtils.equals(newT.last_modified, oldT.last_modified)
                            && newT.group_id == oldT.group_id
                            && newT.encrypted == oldT.encrypted
                            && newT.size == oldT.size
                            && newT.starred == oldT.starred
                            && newT.monitored == oldT.monitored
                            && newT.is_admin == oldT.is_admin;
                }

                if (getItems().get(oldItemPosition) instanceof Account) {
                    Account newT = (Account) getItems().get(oldItemPosition);
                    Account oldT = (Account) newList.get(newItemPosition);
                    return TextUtils.equals(newT.email, oldT.email)
                            && TextUtils.equals(newT.name, oldT.name)
                            && TextUtils.equals(newT.avatar_url, oldT.avatar_url)
                            && TextUtils.equals(newT.server, oldT.server);
                }

                if (getItems().get(oldItemPosition) instanceof DirentModel) {
                    DirentModel newT = (DirentModel) getItems().get(oldItemPosition);
                    DirentModel oldT = (DirentModel) newList.get(newItemPosition);
                    return TextUtils.equals(newT.full_path, oldT.full_path)
                            && TextUtils.equals(newT.name, oldT.name)
                            && TextUtils.equals(newT.parent_dir, oldT.parent_dir)
                            && TextUtils.equals(newT.id, oldT.id)
                            && TextUtils.equals(newT.type, oldT.type)
                            && TextUtils.equals(newT.permission, oldT.permission)
                            && TextUtils.equals(newT.dir_id, oldT.dir_id)
                            && TextUtils.equals(newT.related_account, oldT.related_account)
                            && TextUtils.equals(newT.repo_id, oldT.repo_id)
                            && TextUtils.equals(newT.repo_name, oldT.repo_name)
//                            && TextUtils.equals(newT.lock_owner, oldT.lock_owner)
//                            && TextUtils.equals(newT.lock_owner_name, oldT.lock_owner_name)
//                            && TextUtils.equals(newT.lock_owner_contact_email, oldT.lock_owner_contact_email)
//                            && TextUtils.equals(newT.modifier_email, oldT.modifier_email)
//                            && TextUtils.equals(newT.modifier_name, oldT.modifier_name)
//                            && TextUtils.equals(newT.modifier_contact_email, oldT.modifier_contact_email)
//                            && TextUtils.equals(newT.encoded_thumbnail_src, oldT.encoded_thumbnail_src)
                            && newT.mtime == oldT.mtime
                            && newT.starred == oldT.starred
                            && newT.size == oldT.size
                            && newT.is_locked == oldT.is_locked
                            && newT.is_freezed == oldT.is_freezed
                            && newT.transfer_status == oldT.transfer_status
//                            && newT.locked_by_me == oldT.locked_by_me
//                            && newT.lock_time == oldT.lock_time
                            ;

                }

                return true;
            }
        });

        setItems(newList);
        diffResult.dispatchUpdatesTo(this);
    }

    public Predicate<? super BaseModel> getSearchFilter() {
        if (TextUtils.isEmpty(searchContent)) {
            return null;
        }
        return _searchFilter;
    }

    private String searchContent;
    private final Predicate<? super BaseModel> _searchFilter = new Predicate<>() {
        @Override
        public boolean test(BaseModel baseModel) {
            if (TextUtils.isEmpty(searchContent)) {
                return true;
            }

            if (baseModel instanceof Account) {
                return false;
            } else if (baseModel instanceof GroupItemModel) {
                return false;
            } else if (baseModel instanceof RepoModel m) {
                return m.repo_name.toLowerCase().contains(searchContent.toLowerCase());
            } else if (baseModel instanceof DirentModel m) {
                return m.name.toLowerCase().contains(searchContent.toLowerCase());
            } else {
                return false;
            }
        }
    };
}
