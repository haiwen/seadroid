package com.seafile.seadroid2.gallery;

import android.net.Uri;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.PriorityQueue;

import com.google.common.collect.Maps;

/**
 * A union of different <code>IImageList</code>. This class can merge several
 * <code>IImageList</code> into one list and sort them according to the
 * timestamp (The sorting must be same as all the given lists).
 */
public class ImageListUber implements IImageList {
    @SuppressWarnings("unused")
    private static final String TAG = "ImageListUber";

    private final IImageList [] mSubList;
    private final PriorityQueue<MergeSlot> mQueue;

    // This is an array of Longs wherein each Long consists of two components:
    // "a number" and "an index of sublist".
    //   * The lower 32bit indicates the number of consecutive entries that
    //     belong to a given sublist.
    //
    //   * The higher 32bit component indicates which sublist we're referring
    //     to.
    private long[] mSkipList;
    private int mSkipListSize;
    private int [] mSkipCounts;
    private int mLastListIndex;

    public ImageListUber(IImageList [] sublist, int sort) {
        mSubList = sublist.clone();
        mQueue = new PriorityQueue<MergeSlot>(4,
                sort == ImageManager.SORT_ASCENDING
                ? new AscendingComparator()
                : new DescendingComparator());
        mSkipList = new long[16];
        mSkipListSize = 0;
        mSkipCounts = new int[mSubList.length];
        mLastListIndex = -1;
        mQueue.clear();
        for (int i = 0, n = mSubList.length; i < n; ++i) {
            IImageList list = mSubList[i];
            MergeSlot slot = new MergeSlot(list, i);
            if (slot.next()) mQueue.add(slot);
        }
    }

    public HashMap<String, String> getBucketIds() {
        HashMap<String, String> hashMap = Maps.newHashMap();
        for (IImageList list : mSubList) {
            hashMap.putAll(list.getBucketIds());
        }
        return hashMap;
    }

    public int getCount() {
        int count = 0;
        for (IImageList subList : mSubList) {
            count += subList.getCount();
        }
        return count;
    }

    public boolean isEmpty() {
        for (IImageList subList : mSubList) {
            if (!subList.isEmpty()) return false;
        }
        return true;
    }

    // mSkipCounts is used to tally the counts as we traverse
    // the mSkipList.  It's a member variable only so that
    // we don't have to allocate each time through.  Otherwise
    // it could just as easily be a local.

    public IImage getImageAt(int index) {
        if (index < 0 || index > getCount()) {
            throw new IndexOutOfBoundsException(
                    "index " + index + " out of range max is " + getCount());
        }

        int skipCounts[] = mSkipCounts;
        // zero out the mSkipCounts since that's only used for the
        // duration of the function call.
        Arrays.fill(skipCounts, 0);

        // a counter of how many images we've skipped in
        // trying to get to index.  alternatively we could
        // have decremented index but, alas, I liked this
        // way more.
        int skipCount = 0;

        // scan the existing mSkipList to see if we've computed
        // enough to just return the answer
        for (int i = 0, n = mSkipListSize; i < n; ++i) {
            long v = mSkipList[i];

            int offset = (int) (v & 0xFFFFFFFF);
            int which  = (int) (v >> 32);
            if (skipCount + offset > index) {
                int subindex = mSkipCounts[which] + (index - skipCount);
                return mSubList[which].getImageAt(subindex);
            }
            skipCount += offset;
            mSkipCounts[which] += offset;
        }

        for (; true; ++skipCount) {
            MergeSlot slot = nextMergeSlot();
            if (slot == null) return null;
            if (skipCount == index) {
                IImage result = slot.mImage;
                if (slot.next()) mQueue.add(slot);
                return result;
            }
            if (slot.next()) mQueue.add(slot);
        }
    }

    private MergeSlot nextMergeSlot() {
        MergeSlot slot = mQueue.poll();
        if (slot == null) return null;
        if (slot.mListIndex == mLastListIndex) {
            int lastIndex = mSkipListSize - 1;
            ++mSkipList[lastIndex];
        } else {
            mLastListIndex = slot.mListIndex;
            if (mSkipList.length == mSkipListSize) {
                long [] temp = new long[mSkipListSize * 2];
                System.arraycopy(mSkipList, 0, temp, 0, mSkipListSize);
                mSkipList = temp;
            }
            mSkipList[mSkipListSize++] = (((long) mLastListIndex) << 32) | 1;
        }
        return slot;
    }

    public IImage getImageForUri(Uri uri) {
        for (IImageList sublist : mSubList) {
            IImage image = sublist.getImageForUri(uri);
            if (image != null) return image;
        }
        return null;
    }

    /**
     * Modify the skip list when an image is deleted by finding
     * the relevant entry in mSkipList and decrementing the
     * counter.  This is simple because deletion can never
     * cause change the order of images.
     */
    private void modifySkipCountForDeletedImage(int index) {
        int skipCount = 0;
        for (int i = 0, n = mSkipListSize; i < n; i++) {
            long v = mSkipList[i];
            int offset = (int) (v & 0xFFFFFFFF);
            if (skipCount + offset > index) {
                mSkipList[i] = v - 1;
                break;
            }
            skipCount += offset;
        }
    }

    private boolean removeImage(IImage image, int index) {
        IImageList list = image.getContainer();
        if (list != null && list.removeImage(image)) {
            modifySkipCountForDeletedImage(index);
            return true;
        }
        return false;
    }

    public boolean removeImage(IImage image) {
        return removeImage(image, getImageIndex(image));
    }

    public boolean removeImageAt(int index) {
        IImage image = getImageAt(index);
        if (image == null) return false;
        return removeImage(image, index);
    }

    public synchronized int getImageIndex(IImage image) {
        IImageList list = image.getContainer();
        int listIndex = Util.indexOf(mSubList, list);
        if (listIndex == -1) {
            throw new IllegalArgumentException();
        }
        int listOffset = list.getImageIndex(image);

        // Similar algorithm as getImageAt(int index)
        int skipCount = 0;
        for (int i = 0, n = mSkipListSize; i < n; ++i) {
            long value = mSkipList[i];
            int offset = (int) (value & 0xFFFFFFFF);
            int which  = (int) (value >> 32);
            if (which == listIndex) {
                if (listOffset < offset) {
                    return skipCount + listOffset;
                }
                listOffset -= offset;
            }
            skipCount += offset;
        }

        for (; true; ++skipCount) {
            MergeSlot slot = nextMergeSlot();
            if (slot == null) return -1;
            if (slot.mImage == image) {
                if (slot.next()) mQueue.add(slot);
                return skipCount;
            }
            if (slot.next()) mQueue.add(slot);
        }
    }

    private static class DescendingComparator implements Comparator<MergeSlot>, Serializable {
        private static final long serialVersionUID = 3767807277478952774L;

        public int compare(MergeSlot m1, MergeSlot m2) {
            if (m1.mDateTaken != m2.mDateTaken) {
                return m1.mDateTaken < m2.mDateTaken ? 1 : -1;
            }
            return m1.mListIndex - m2.mListIndex;
        }
    }

    private static class AscendingComparator implements Comparator<MergeSlot>, Serializable {
        private static final long serialVersionUID = -1156972649541001600L;

        public int compare(MergeSlot m1, MergeSlot m2) {
            if (m1.mDateTaken != m2.mDateTaken) {
                return m1.mDateTaken < m2.mDateTaken ? -1 : 1;
            }
            return m1.mListIndex - m2.mListIndex;
        }
    }

    /**
     * A merging slot is used to trace the current position of a sublist. For
     * each given sub list, there will be one corresponding merge slot. We
     * use merge-sort-like algorithm to build the merged list. At begining,
     * we put all the slots in a sorted heap (by timestamp). Each time, we
     * pop the slot with earliest timestamp out, get the image, and then move
     * the index forward, and put it back to the heap.
     */
    private static class MergeSlot {
        private int mOffset = -1;
        private final IImageList mList;

        int mListIndex;
        long mDateTaken;
        IImage mImage;

        public MergeSlot(IImageList list, int index) {
            mList = list;
            mListIndex = index;
        }

        public boolean next() {
            if (mOffset >= mList.getCount() - 1) return false;
            mImage = mList.getImageAt(++mOffset);
            mDateTaken = mImage.getDateTaken();
            return true;
        }
    }

    public void close() {
        for (int i = 0, n = mSubList.length; i < n; ++i) {
            mSubList[i].close();
        }
    }
}