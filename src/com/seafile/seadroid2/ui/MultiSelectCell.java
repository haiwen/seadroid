package com.seafile.seadroid2.ui;

import android.widget.ImageView;

/**
 *
 */
public class MultiSelectCell {
    private ImageView multiSelectBtn;
    private int position;
    private boolean isSelected;

    public MultiSelectCell(ImageView imageView, int position, boolean isSelected) {
        this.multiSelectBtn = imageView;
        this.position = position;
        this.isSelected = isSelected;
    }

    public boolean isSelected() {
        return isSelected;
    }

    public void setSelected(boolean isSelected) {
        this.isSelected = isSelected;
    }

    public void toggleSelection() {
        this.isSelected = !isSelected;
    }

    public void setImageResource(int id) {
        this.multiSelectBtn.setImageResource(id);
    }

    public int getPosition() {
        return position;
    }

    public void setPosition(int position) {
        this.position = position;
    }

    public ImageView getMultiSelectBtn() {
        return multiSelectBtn;
    }

    public void setMultiSelectBtn(ImageView multiSelectBtn) {
        this.multiSelectBtn = multiSelectBtn;
    }

}
