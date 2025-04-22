package com.seafile.seadroid2.view;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.PopupWindow;

import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.SortBy;
import com.seafile.seadroid2.preferences.Settings;

public class ViewSortPopupWindow extends PopupWindow {
    private final int w;
    public ViewSortPopupWindow(Context context) {
        super(context);

        w = SizeUtils.dp2px(144);

        View popView = LayoutInflater.from(context).inflate(R.layout.layout_fragment_pop_menu, null);
        ViewGroup.LayoutParams params = new ViewGroup.LayoutParams(Constants.DP.DP_160, ViewGroup.LayoutParams.WRAP_CONTENT);
        popView.setLayoutParams(params);

        setContentView(popView);

        this.setWidth(Constants.DP.DP_160);
        this.setHeight(ViewGroup.LayoutParams.WRAP_CONTENT);

        this.setFocusable(true);
        this.setElevation(8);
        this.setOutsideTouchable(true);
        this.setFocusable(true);
        this.setBackgroundDrawable(ContextCompat.getDrawable(getContentView().getContext(), R.color.white));

        initView(popView);
        initViewData(context);
    }

    public int getW() {
        return w;
    }

    private ImageView menuListIcon;
    private ImageView menuGridIcon;
    private ImageView menuGalleryIcon;
    private ImageView menuSortNameIcon;
    private ImageView menuSortSizeIcon;
    private ImageView menuSortLastModifiedIcon;
    private ImageView menuSortAscendingIcon;

    private void initView(View view) {
        //icon
        menuListIcon = getContentView().findViewById(R.id.menu_view_list_icon);
        menuGridIcon = getContentView().findViewById(R.id.menu_view_grid_icon);
        menuGalleryIcon = getContentView().findViewById(R.id.menu_view_gallery_icon);
        menuSortNameIcon = getContentView().findViewById(R.id.menu_sort_name_icon);
        menuSortSizeIcon = getContentView().findViewById(R.id.menu_sort_size_icon);
        menuSortLastModifiedIcon = getContentView().findViewById(R.id.menu_sort_last_modified_icon);
        menuSortAscendingIcon = getContentView().findViewById(R.id.menu_sort_ascending_icon);

        view.findViewById(R.id.menu_view_list).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_view_list);
            dismiss();
        });
        view.findViewById(R.id.menu_view_grid).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_view_grid);
            dismiss();
        });
        view.findViewById(R.id.menu_view_gallery).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_view_gallery);
            dismiss();
        });
        view.findViewById(R.id.menu_sort_name).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_name);
            dismiss();
        });
        view.findViewById(R.id.menu_sort_size).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_size);
            dismiss();
        });
        view.findViewById(R.id.menu_sort_last_modified).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_last_modified);
            dismiss();
        });
        view.findViewById(R.id.menu_sort_ascending).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_ascending);
            dismiss();
        });
    }

    private void goneAllIcon() {
        menuListIcon.setVisibility(View.INVISIBLE);
        menuGridIcon.setVisibility(View.INVISIBLE);
        menuGalleryIcon.setVisibility(View.INVISIBLE);
        menuSortNameIcon.setVisibility(View.INVISIBLE);
        menuSortSizeIcon.setVisibility(View.INVISIBLE);
        menuSortLastModifiedIcon.setVisibility(View.INVISIBLE);
        menuSortAscendingIcon.setVisibility(View.INVISIBLE);
    }

    private void onContainerClick(long clickedId) {
        if (clickedId == R.id.menu_view_list) {
            menuListIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_VIEW_TYPE.putValue(FileViewType.LIST);
        } else if (clickedId == R.id.menu_view_grid) {
            menuGridIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_VIEW_TYPE.putValue(FileViewType.GRID);
        } else if (clickedId == R.id.menu_view_gallery) {
            menuGalleryIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_VIEW_TYPE.putValue(FileViewType.GALLERY);
        } else if (clickedId == R.id.menu_sort_name) {
            menuSortNameIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_SORT_BY.putValue(SortBy.NAME);
        } else if (clickedId == R.id.menu_sort_size) {
            menuSortSizeIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_SORT_BY.putValue(SortBy.TYPE);
        } else if (clickedId == R.id.menu_sort_last_modified) {
            menuSortLastModifiedIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_SORT_BY.putValue(SortBy.SIZE);
        } else if (clickedId == R.id.menu_sort_ascending) {
            Boolean b = Settings.FILE_LIST_SORT_ASCENDING.queryValue();
            if (b) {
                menuSortAscendingIcon.setVisibility(View.INVISIBLE);
                Settings.FILE_LIST_SORT_ASCENDING.putValue(false);
            } else {
                menuSortAscendingIcon.setVisibility(View.VISIBLE);
                Settings.FILE_LIST_SORT_ASCENDING.putValue(true);
            }
        }
    }

    private void initViewData(Context context) {

        goneAllIcon();

        FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
        if (fileViewType == FileViewType.LIST) {
            menuListIcon.setVisibility(View.VISIBLE);
        } else if (fileViewType == FileViewType.GRID) {
            menuGridIcon.setVisibility(View.VISIBLE);
        } else if (fileViewType == FileViewType.GALLERY) {
            menuGalleryIcon.setVisibility(View.VISIBLE);
        } else {
            menuListIcon.setVisibility(View.VISIBLE);
        }

        SortBy sortBy = Settings.FILE_LIST_SORT_BY.queryValue();
        if (sortBy == SortBy.NAME) {
            menuSortNameIcon.setVisibility(View.VISIBLE);
        } else if (sortBy == SortBy.SIZE) {
            menuSortSizeIcon.setVisibility(View.VISIBLE);
        } else if (sortBy == SortBy.LAST_MODIFIED) {
            menuSortLastModifiedIcon.setVisibility(View.VISIBLE);
        } else {
            menuSortNameIcon.setVisibility(View.VISIBLE);
        }

        Boolean ascendingValue = Settings.FILE_LIST_SORT_ASCENDING.queryValue();
        if (Boolean.TRUE.equals(ascendingValue)) {
            menuSortAscendingIcon.setVisibility(View.VISIBLE);
        }
    }
}
