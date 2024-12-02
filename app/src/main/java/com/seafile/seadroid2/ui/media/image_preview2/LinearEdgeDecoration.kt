package com.seafile.seadroid2.ui.media.image_preview2

import android.graphics.Rect
import android.view.View
import androidx.annotation.Px
import androidx.recyclerview.widget.RecyclerView

class LinearEdgeDecoration(
    @Px private val startPadding: Int,
    @Px private val endPadding: Int = startPadding,
    private val orientation: Int = RecyclerView.VERTICAL,
    private val inverted: Boolean = false
) : RecyclerView.ItemDecoration() {

    override fun getItemOffsets(
        outRect: Rect, view: View, parent: RecyclerView,
        state: RecyclerView.State
    ) {
        super.getItemOffsets(outRect, view, parent, state)

        val layoutManager: RecyclerView.LayoutManager = parent.layoutManager!!
        val layoutParams = view.layoutParams as RecyclerView.LayoutParams
        val position = layoutParams.viewAdapterPosition
        val itemCount = layoutManager.itemCount

        if (position == RecyclerView.NO_POSITION || itemCount == 0
            || (position > 0 && position < itemCount - 1)
        ) {
            return
        }

        if (orientation == RecyclerView.HORIZONTAL) {
            if (position == 0) {
                if (!inverted) {
                    outRect.left = startPadding
                } else {
                    outRect.right = startPadding
                }
            } else if (position == itemCount - 1) {
                if (!inverted) {
                    outRect.right = endPadding
                } else {
                    outRect.left = endPadding
                }
            }
        } else {
            if (position == 0) {
                if (!inverted) {
                    outRect.top = startPadding
                } else {
                    outRect.bottom = startPadding
                }
            } else if (position == itemCount - 1) {
                if (!inverted) {
                    outRect.bottom = endPadding
                } else {
                    outRect.top = endPadding
                }
            }
        }
    }
}