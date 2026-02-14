package com.seafile.seadroid2.view;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.PopupWindow;
import android.widget.TextView;

import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.SizeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.SortBy;
import com.seafile.seadroid2.preferences.Settings;

public class ViewSortPopupWindow extends PopupWindow {
    private final int w;

    public ViewSortPopupWindow(Context context, NavContext navContext) {
        super(context);

        w = SizeUtils.dp2px(160);

        View popView = LayoutInflater.from(context).inflate(R.layout.layout_fragment_pop_menu, null);
        ViewGroup.LayoutParams params = new ViewGroup.LayoutParams(w, ViewGroup.LayoutParams.WRAP_CONTENT);
        popView.setLayoutParams(params);

        setContentView(popView);

        this.setWidth(w);
        this.setHeight(ViewGroup.LayoutParams.WRAP_CONTENT);

        this.setFocusable(true);
        this.setElevation(8);
        this.setOutsideTouchable(true);
        this.setFocusable(true);
        this.setBackgroundDrawable(ContextCompat.getDrawable(getContentView().getContext(), R.color.white));

        //
        initView(popView);

        //
        initViewData();

        ableMenu(navContext);
    }

    public int getW() {
        return w;
    }

    private LinearLayout menuViewList;
    private LinearLayout menuViewGrid;
    private LinearLayout menuViewGallery;
    private LinearLayout menuSortName;
    private LinearLayout menuSortSize;
    private LinearLayout menuSortType;

    private ImageView menuListIcon;
    private ImageView menuGridIcon;
    private ImageView menuGalleryIcon;
    private ImageView menuSortNameIcon;
    private ImageView menuSortSizeIcon;
    private ImageView menuSortLastModifiedIcon;
    private ImageView menuSortTypeIcon;
    private ImageView menuSortAscendingIcon;

    private TextView menuViewListTitle;
    private TextView menuViewGridTitle;
    private TextView menuViewGalleryTitle;

    private void initView(View view) {
        menuViewList = view.findViewById(R.id.menu_view_list);
        menuViewGrid = view.findViewById(R.id.menu_view_grid);
        menuViewGallery = view.findViewById(R.id.menu_view_gallery);
        menuSortName = view.findViewById(R.id.menu_sort_name);
        menuSortSize = view.findViewById(R.id.menu_sort_size);
        menuSortType = view.findViewById(R.id.menu_sort_type);


        //icon
        menuListIcon = getContentView().findViewById(R.id.menu_view_list_icon);
        menuGridIcon = getContentView().findViewById(R.id.menu_view_grid_icon);
        menuGalleryIcon = getContentView().findViewById(R.id.menu_view_gallery_icon);
        menuSortNameIcon = getContentView().findViewById(R.id.menu_sort_name_icon);
        menuSortSizeIcon = getContentView().findViewById(R.id.menu_sort_size_icon);
        menuSortLastModifiedIcon = getContentView().findViewById(R.id.menu_sort_last_modified_icon);
        menuSortTypeIcon = getContentView().findViewById(R.id.menu_sort_type_icon);
        menuSortAscendingIcon = getContentView().findViewById(R.id.menu_sort_ascending_icon);

        //text
        menuViewListTitle = getContentView().findViewById(R.id.menu_view_list_title);
        menuViewGridTitle = getContentView().findViewById(R.id.menu_view_grid_title);
        menuViewGalleryTitle = getContentView().findViewById(R.id.menu_view_gallery_title);


        //
        menuViewList.setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_view_list);
            dismiss();
        });

        menuViewGrid.setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_view_grid);
            dismiss();
        });

        menuViewGallery.setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_view_gallery);
            dismiss();
        });

        menuSortName.setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_name);
            dismiss();
        });

        menuSortSize.setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_size);
            dismiss();
        });

        view.findViewById(R.id.menu_sort_last_modified).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_last_modified);
            dismiss();
        });

        view.findViewById(R.id.menu_sort_type).setOnClickListener(v -> {
            goneAllIcon();
            onContainerClick(R.id.menu_sort_type);
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
        menuSortTypeIcon.setVisibility(View.INVISIBLE);
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
            Settings.FILE_LIST_SORT_BY.putValue(SortBy.SIZE);
        } else if (clickedId == R.id.menu_sort_last_modified) {
            menuSortLastModifiedIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_SORT_BY.putValue(SortBy.LAST_MODIFIED);
        } else if (clickedId == R.id.menu_sort_type) {
            menuSortTypeIcon.setVisibility(View.VISIBLE);
            Settings.FILE_LIST_SORT_BY.putValue(SortBy.TYPE);
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

    private void initViewData() {

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
        } else if (sortBy == SortBy.TYPE) {
            menuSortTypeIcon.setVisibility(View.VISIBLE);
        } else {
            menuSortNameIcon.setVisibility(View.VISIBLE);
        }

        Boolean ascendingValue = Settings.FILE_LIST_SORT_ASCENDING.queryValue();
        if (Boolean.TRUE.equals(ascendingValue)) {
            menuSortAscendingIcon.setVisibility(View.VISIBLE);
        }
    }

    private void ableMenu(NavContext navContext) {
        if (navContext.inRepo()) {
            menuViewList.setVisibility(View.VISIBLE);
            menuViewGallery.setVisibility(View.VISIBLE);
            menuViewGrid.setVisibility(View.VISIBLE);
        } else {
            menuViewList.setVisibility(View.GONE);
            menuViewGallery.setVisibility(View.GONE);
            menuViewGrid.setVisibility(View.GONE);
        }
    }
}
