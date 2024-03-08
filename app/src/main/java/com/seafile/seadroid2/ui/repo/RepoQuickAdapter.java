package com.seafile.seadroid2.ui.repo;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.DiffUtil;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.model.GroupItemModel;
import com.seafile.seadroid2.databinding.ItemAccountBinding;
import com.seafile.seadroid2.databinding.ItemDirentBinding;
import com.seafile.seadroid2.databinding.ItemGroupItemBinding;
import com.seafile.seadroid2.databinding.ItemRepoBinding;
import com.seafile.seadroid2.databinding.ItemUnsupportedBinding;
import com.seafile.seadroid2.ui.viewholder.GroupItemViewHolder;
import com.seafile.seadroid2.util.GlideApp;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

public class RepoQuickAdapter extends BaseMultiAdapter<BaseModel> {
    private final String SERVER = IO.getSingleton().getServerUrl();

    private boolean actionModeOn;

    private Drawable starDrawable;

    /**
     * -1 no select
     * 0 only select account
     * 1 only select repo
     */
    private int selectorMode = -1;

    /**
     * -1 no limited
     * 0 no this value
     * >=1 max count
     */
    private int selectedMaxCount = 1;

    public void setSelectorMode(int selectorMode) {
        this.selectorMode = selectorMode;
    }

    public void setSelectData(int selectorMode, int selectedMaxCount) {
        this.selectorMode = selectorMode;
        this.selectedMaxCount = selectedMaxCount;
    }

    public RepoQuickAdapter() {
        addItemType(AbsLayoutItemType.GROUP_ITEM, new OnMultiItemAdapterListener<BaseModel, GroupItemViewHolder>() {
            @NonNull
            @Override
            public GroupItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemGroupItemBinding binding = ItemGroupItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new GroupItemViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull GroupItemViewHolder holder, int i, @Nullable BaseModel groupTimeModel) {
                onBindGroup(holder, (GroupItemModel) groupTimeModel);
            }
        }).addItemType(AbsLayoutItemType.ACCOUNT, new OnMultiItem<BaseModel, AccountViewHolder>() {
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
        }).addItemType(AbsLayoutItemType.DIRENT, new OnMultiItem<BaseModel, DirentViewHolder>() {
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
        }).addItemType(AbsLayoutItemType.UNSUPPORTED, new OnMultiItem<BaseModel, UnsupportedViewHolder>() {
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
                    return AbsLayoutItemType.DIRENT;
                } else if (list.get(i) instanceof Account) {
                    return AbsLayoutItemType.ACCOUNT;
                }
                return AbsLayoutItemType.UNSUPPORTED;
            }
        });
    }

    public Drawable getStarDrawable() {
        if (null == starDrawable) {
            int DP_16 = SizeUtils.dp2px(16);
            starDrawable = ContextCompat.getDrawable(getContext(), R.drawable.baseline_star_24);
            starDrawable.setBounds(0, 0, DP_16, DP_16);
            starDrawable.setTint(ContextCompat.getColor(getContext(), R.color.light_grey));
        }
        return starDrawable;
    }


    private void onBindAccount(AccountViewHolder holder, BaseModel model) {
        Account account = (Account) model;

        holder.binding.listItemAccountTitle.setText(account.getServerHost());
        holder.binding.listItemAccountSubtitle.setText(account.getName());

        if (TextUtils.isEmpty(account.avatar_url)) {
            holder.binding.listItemAccountIcon.setImageResource(R.drawable.default_avatar);
        } else {
            GlideApp.with(getContext())
                    .load(GlideLoadConfig.getGlideUrl(account.avatar_url))
                    .apply(GlideLoadConfig.getAvatarOptions())
                    .into(holder.binding.listItemAccountIcon);
        }


        if (selectorMode >= 0) {
            holder.binding.itemSelectView.setVisibility(account.is_selected ? View.VISIBLE : View.INVISIBLE);
        } else {
            holder.binding.itemSelectView.setVisibility(View.INVISIBLE);
        }

    }

    private void onBindGroup(GroupItemViewHolder holder, GroupItemModel model) {
        if (model.name != 0) {
            holder.binding.itemGroupTitle.setText(model.name);
        } else if (!TextUtils.isEmpty(model.title)) {
            if ("Organization".equals(model.title)) {
                holder.binding.itemGroupTitle.setText(R.string.shared_with_all);
            } else {
                holder.binding.itemGroupTitle.setText(model.title);
            }
        }
    }

    private void onBindRepos(RepoViewHolder holder, RepoModel model) {
        holder.binding.itemTitle.setText(model.repo_name);
        holder.binding.itemSubtitle.setText(model.getSubtitle());
        holder.binding.itemIcon.setImageResource(model.getIcon());

        if (selectorMode >= 1) {
            holder.binding.itemSelectView.setVisibility(model.is_selected ? View.VISIBLE : View.INVISIBLE);
            holder.binding.expandableToggleButton.setVisibility(View.INVISIBLE);
        } else if (!model.hasWritePermission()) {
            holder.binding.expandableToggleButton.setVisibility(View.INVISIBLE);
            holder.binding.itemSelectView.setVisibility(View.GONE);
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

        if (!Utils.isViewableImage(model.name)) {
            holder.binding.itemIcon.setImageResource(model.getIcon());
        } else {
            String url = convertThumbnailUrl(model.repo_id, model.full_path);
            GlideApp.with(getContext())
                    .load(GlideLoadConfig.getGlideUrl(url))
                    .apply(GlideLoadConfig.getOptions())
                    .into(holder.binding.itemIcon);
        }

        //action mode
        if (actionModeOn) {
            holder.binding.itemMultiSelect.setVisibility(View.VISIBLE);
            if (model.is_selected) {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_checked);
            } else {
                holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
            }
        } else {
            holder.binding.itemMultiSelect.setVisibility(View.GONE);
            holder.binding.itemMultiSelect.setImageResource(R.drawable.multi_select_item_unchecked);
        }

        holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
        holder.binding.itemDownloadStatus.setVisibility(View.GONE);

        if (selectorMode < 0) {
            holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.list_item_title_color));
            holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.list_item_subtitle_color));

            holder.binding.expandableToggleButton.setVisibility(View.VISIBLE);
        } else {

            if (!model.isDir()) {
                holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.light_grey));
                holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.light_grey));
            } else {
                holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.list_item_title_color));
                holder.binding.itemSubtitle.setTextColor(ContextCompat.getColor(getContext(), R.color.list_item_subtitle_color));
            }

            holder.binding.expandableToggleButton.setVisibility(View.INVISIBLE);
        }

        if (model.isDir()) {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
            holder.binding.itemDownloadStatus.setVisibility(View.GONE);
        } else if (TransferStatus.TRANSFER_WAITING == model.transfer_status ||
                TransferStatus.TRANSFER_IN_PROGRESS == model.transfer_status) {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.VISIBLE);
            holder.binding.itemDownloadStatus.setVisibility(View.VISIBLE);
        } else if (TransferStatus.TRANSFER_CANCELLED == model.transfer_status ||
                TransferStatus.TRANSFER_FAILED == model.transfer_status) {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
            holder.binding.itemDownloadStatus.setVisibility(View.GONE);
        } else if (TransferStatus.TRANSFER_SUCCEEDED == model.transfer_status) {
            holder.binding.itemDownloadStatusProgressbar.setVisibility(View.GONE);
            holder.binding.itemDownloadStatus.setVisibility(View.VISIBLE);
        }

        holder.binding.itemTitle.setCompoundDrawablePadding(Constants.DP.DP_4);
        if (model.starred) {
            holder.binding.itemTitle.setCompoundDrawables(null, null, getStarDrawable(), null);
        } else {
            holder.binding.itemTitle.setCompoundDrawables(null, null, null, null);
        }
    }

    private String convertThumbnailUrl(String repoId, String filePath) {
        return String.format(Locale.getDefault(), "%sapi2/repos/%s/thumbnail/?p=%s&size=%d", SERVER, repoId, filePath, 48);
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
                model.is_selected = itemSelected;
            }
        }

        notifyItemRangeChanged(0, getItemCount());
    }

    public List<DirentModel> getSelectedList() {
        List<DirentModel> list = new ArrayList<>();
        for (BaseModel item : getItems()) {
            if (item instanceof DirentModel) {
                DirentModel model = (DirentModel) item;
                if (model.is_selected) {
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
                item.is_selected = !item.is_selected;
                notifyItemChanged(selectedPosition);
                return item.is_selected;

            } else if (selectedPosition > -1) {
                //Deselect an item that has already been selected
                getItems().get(selectedPosition).is_selected = false;
                notifyItemChanged(selectedPosition);

                item.is_selected = true;
                notifyItemChanged(position);
            } else {
                item.is_selected = true;
                notifyItemChanged(position);
            }
        } else {
            long selectedCount = getSelectedCountByMode();
            if (selectedCount >= selectedMaxCount) {
                return false;
            }

            item.is_selected = !item.is_selected;
            notifyItemChanged(position);

            return item.is_selected;
        }

        return true;
    }

    private long getSelectedCountByMode() {
        if (selectorMode == 0) {
            return getItems().stream()
                    .filter(Account.class::isInstance)
                    .filter(f -> f.is_selected)
                    .count();
        } else if (selectorMode == 1) {
            return getItems().stream()
                    .filter(RepoModel.class::isInstance)
                    .filter(f -> f.is_selected)
                    .count();
        }
        return 0;
    }

    private int getSelectedPositionByMode() {
        for (int i = 0; i < getItems().size(); i++) {
            if (getItems().get(i).is_selected) {
                return i;
            }
        }
        return -1;
    }

    public void notifyDataChanged(List<BaseModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            submitList(list);
            return;
        }

        if (CollectionUtils.isEmpty(getItems())) {
            submitList(list);
            return;
        }

        DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffUtil.Callback() {
            @Override
            public int getOldListSize() {
                return getItems().size();
            }

            @Override
            public int getNewListSize() {
                return list.size();
            }

            @Override
            public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
                String oldClassName = getItems().get(oldItemPosition).getClass().getName();
                String newClassName = list.get(newItemPosition).getClass().getName();
                if (!oldClassName.equals(newClassName)) {
                    return false;
                }
                if (getItems().get(oldItemPosition) instanceof Account) {
                    Account newT = (Account) getItems().get(oldItemPosition);
                    Account oldT = (Account) list.get(newItemPosition);
                    if (!TextUtils.equals(newT.email, oldT.email)) {
                        return false;
                    }
                }

                if (getItems().get(oldItemPosition) instanceof RepoModel) {
                    RepoModel newT = (RepoModel) getItems().get(oldItemPosition);
                    RepoModel oldT = (RepoModel) list.get(newItemPosition);
                    if (!TextUtils.equals(newT.repo_id, oldT.repo_id) || newT.group_id != oldT.group_id) {
                        return false;
                    }
                }

                if (getItems().get(oldItemPosition) instanceof DirentModel) {
                    DirentModel newT = (DirentModel) getItems().get(oldItemPosition);
                    DirentModel oldT = (DirentModel) list.get(newItemPosition);
                    return TextUtils.equals(newT.full_path, oldT.full_path);
                }

                return true;
            }

            @Override
            public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
                String oldClassName = getItems().get(oldItemPosition).getClass().getName();
                String newClassName = list.get(newItemPosition).getClass().getName();
                if (!oldClassName.equals(newClassName)) {
                    return false;
                }

                if (getItems().get(oldItemPosition) instanceof RepoModel) {
                    RepoModel newT = (RepoModel) getItems().get(oldItemPosition);
                    RepoModel oldT = (RepoModel) list.get(newItemPosition);

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
                    Account oldT = (Account) list.get(newItemPosition);
                    return TextUtils.equals(newT.email, oldT.email)
                            && TextUtils.equals(newT.name, oldT.name)
                            && TextUtils.equals(newT.avatar_url, oldT.avatar_url)
                            && TextUtils.equals(newT.server, oldT.server);
                }

                if (getItems().get(oldItemPosition) instanceof DirentModel) {
                    DirentModel newT = (DirentModel) getItems().get(oldItemPosition);
                    DirentModel oldT = (DirentModel) list.get(newItemPosition);
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

        setItems(list);
        diffResult.dispatchUpdatesTo(this);
    }
}
