package com.seafile.seadroid2.gallery;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import android.content.ContentResolver;
import android.database.Cursor;
import android.net.Uri;
import android.os.Environment;
import android.os.Parcel;
import android.os.Parcelable;
import android.provider.MediaStore;
import android.provider.MediaStore.Images;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.util.Utils;

/**
 * ImageManager is used to retrieve and store images
 * in the media content provider.
 */
public class ImageManager {
    private static final String TAG = "ImageManager";

    private static final Uri STORAGE_URI = Images.Media.EXTERNAL_CONTENT_URI;
    private static final Uri VIDEO_STORAGE_URI = Uri.parse("content://media/external/video/media");

    // ImageListParam specifies all the parameters we need to create an image
    // list (we also need a ContentResolver).
    public static class ImageListParam implements Parcelable {
        public DataLocation mLocation;
        public int mInclusion;
        public int mSort;
        public String mBucketId;

        // This is only used if we are creating a single image list.
        public Uri mSingleImageUri;

        // This is only used if we are creating an empty image list.
        public boolean mIsEmptyImageList;

        public ImageListParam() {}

        public void writeToParcel(Parcel out, int flags) {
            out.writeInt(mLocation.ordinal());
            out.writeInt(mInclusion);
            out.writeInt(mSort);
            out.writeString(mBucketId);
            out.writeParcelable(mSingleImageUri, flags);
            out.writeInt(mIsEmptyImageList ? 1 : 0);
        }

        private ImageListParam(Parcel in) {
            mLocation = DataLocation.values()[in.readInt()];
            mInclusion = in.readInt();
            mSort = in.readInt();
            mBucketId = in.readString();
            mSingleImageUri = in.readParcelable(null);
            mIsEmptyImageList = (in.readInt() != 0);
        }

        public String toString() {
            return String.format("ImageListParam{loc=%s,inc=%d,sort=%d," +
                "bucket=%s,empty=%b,single=%s}", mLocation, mInclusion,
                mSort, mBucketId, mIsEmptyImageList, mSingleImageUri);
        }

        public static final Parcelable.Creator CREATOR
                = new Parcelable.Creator() {
            public ImageListParam createFromParcel(Parcel in) {
                return new ImageListParam(in);
            }

            public ImageListParam[] newArray(int size) {
                return new ImageListParam[size];
            }
        };

        public int describeContents() {
            return 0;
        }
    }

    // Location
    public static enum DataLocation { NONE, INTERNAL, EXTERNAL, ALL }

    // Inclusion
    public static final int INCLUDE_IMAGES = (1 << 0);
    public static final int INCLUDE_DRM_IMAGES = (1 << 1);
    public static final int INCLUDE_VIDEOS = (1 << 2);

    // Sort
    public static final int SORT_ASCENDING = 1;
    public static final int SORT_DESCENDING = 2;

    private static final String CAMERA_IMAGE_BUCKET_NAME =
            Environment.getExternalStorageDirectory().toString()
            + "/DCIM";

    public static final String CAMERA_IMAGE_BUCKET_ID =
            getBucketId(CAMERA_IMAGE_BUCKET_NAME);

    /**
     * Matches code in MediaProvider.computeBucketValues. Should be a common
     * function.
     */
    public static String getBucketId(String path) {
        return String.valueOf(path.toLowerCase().hashCode());
    }

    private static List<String> allBucketIds;

    public static List<String> getAutoScannedPathList() {
        String[] paths = {
                "/DCIM",
                "/DCIM/Camera",
                "/DCIM/100MEDIA",
                // Many Samsung phones mount the external sd card to /sdcard/external_sd
                "/external_sd/DCIM",
                "/external_sd/DCIM/Camera",
                "/external_sd/DCIM/100MEDIA"
        };

        List<String> pathList = Lists.newArrayList();
        for (String path : paths) {
            String fullPath = Utils.pathJoin(Environment.getExternalStorageDirectory().getAbsolutePath(), path);
            pathList.add(fullPath);
        }
        return pathList;
    }
    
    public static List<String> getAllBucketIds() {
        if (allBucketIds == null) {
            String[] paths = {
                "/DCIM",
                "/DCIM/Camera",
                "/DCIM/100MEDIA",
                // Many Samsung phones mount the external sd card to /sdcard/external_sd
                "/external_sd/DCIM",
                "/external_sd/DCIM/Camera",
                "/external_sd/DCIM/100MEDIA"
            };

            List<String> ids = Lists.newArrayList();
            for (String path : paths) {
                String fullPath = Environment.getExternalStorageDirectory().toString() + path;
                ids.add(getBucketId(fullPath));
            }

            allBucketIds = ImmutableList.copyOf(ids);
        }

        return allBucketIds;
    }

    /**
     * @return true if the mimetype is an image mimetype.
     */
    public static boolean isImageMimeType(String mimeType) {
        return mimeType.startsWith("image/");
    }

    /**
     * @return true if the mimetype is a video mimetype.
     */
    /* This is commented out because isVideo is not calling this now.
    public static boolean isVideoMimeType(String mimeType) {
        return mimeType.startsWith("video/");
    }
    */

    /**
     * @return true if the image is an image.
     */
    public static boolean isImage(IImage image) {
        return isImageMimeType(image.getMimeType());
    }

    /**
     * @return true if the image is a video.
     */
    public static boolean isVideo(IImage image) {
        // This is the right implementation, but we use instanceof for speed.
        //return isVideoMimeType(image.getMimeType());
        return (image instanceof VideoObject);
    }

    // This is the factory function to create an image list.
    public static IImageList makeImageList(ContentResolver cr,
            ImageListParam param) {
        DataLocation location = param.mLocation;
        int inclusion = param.mInclusion;
        int sort = param.mSort;
        String bucketId = param.mBucketId;
        Uri singleImageUri = param.mSingleImageUri;
        boolean isEmptyImageList = param.mIsEmptyImageList;

        if (isEmptyImageList || cr == null) {
            return new EmptyImageList();
        }

        if (singleImageUri != null) {
            return new SingleImageList(cr, singleImageUri);
        }

        // false ==> don't require write access
        boolean haveSdCard = hasStorage(false);

        // use this code to merge videos and stills into the same list
        ArrayList<BaseImageList> l = Lists.newArrayList();

        if (haveSdCard && location != DataLocation.INTERNAL) {
            if ((inclusion & INCLUDE_IMAGES) != 0) {
                l.add(new ImageList(cr, STORAGE_URI, sort, bucketId));
            }
            if ((inclusion & INCLUDE_VIDEOS) != 0) {
                l.add(new VideoList(cr, VIDEO_STORAGE_URI, sort, bucketId));
            }
        }
        if (location == DataLocation.INTERNAL || location == DataLocation.ALL) {
            if ((inclusion & INCLUDE_IMAGES) != 0) {
                l.add(new ImageList(cr,
                        Images.Media.INTERNAL_CONTENT_URI, sort, bucketId));
            }
        }

        // Optimization: If some of the lists are empty, remove them.
        // If there is only one remaining list, return it directly.
        Iterator<BaseImageList> iter = l.iterator();
        while (iter.hasNext()) {
            BaseImageList sublist = iter.next();
            if (sublist.isEmpty()) {
                sublist.close();
                iter.remove();
            }
        }

        if (l.size() == 1) {
            BaseImageList list = l.get(0);
            return list;
        }

        ImageListUber uber = new ImageListUber(
                l.toArray(new IImageList[l.size()]), sort);
        return uber;
    }

    private static class EmptyImageList implements IImageList {
        public void close() {
        }

        public HashMap<String, String> getBucketIds() {
            return Maps.newHashMap();
        }

        public int getCount() {
            return 0;
        }

        public boolean isEmpty() {
            return true;
        }

        public IImage getImageAt(int i) {
            return null;
        }

        public IImage getImageForUri(Uri uri) {
            return null;
        }

        public boolean removeImage(IImage image) {
            return false;
        }

        public boolean removeImageAt(int i) {
            return false;
        }

        public int getImageIndex(IImage image) {
            throw new UnsupportedOperationException();
        }
    }

    public static ImageListParam getImageListParam(DataLocation location,
         int inclusion, int sort, String bucketId) {
         ImageListParam param = new ImageListParam();
         param.mLocation = location;
         param.mInclusion = inclusion;
         param.mSort = sort;
         param.mBucketId = bucketId;
         return param;
    }

    public static ImageListParam getSingleImageListParam(Uri uri) {
        ImageListParam param = new ImageListParam();
        param.mSingleImageUri = uri;
        return param;
    }

    public static ImageListParam getEmptyImageListParam() {
        ImageListParam param = new ImageListParam();
        param.mIsEmptyImageList = true;
        return param;
    }

    public static IImageList makeEmptyImageList() {
        return makeImageList(null, getEmptyImageListParam());
    }

    private static boolean checkFsWritable() {
        // Create a temporary file to see whether a volume is really writeable.
        // It's important not to put it in the root directory which may have a
        // limit on the number of files.
        String directoryName =
                Environment.getExternalStorageDirectory().toString() + "/DCIM";
        File directory = new File(directoryName);
        if (!directory.isDirectory()) {
            if (!directory.mkdirs()) {
                return false;
            }
        }
        File f = new File(directoryName, ".probe");
        try {
            // Remove stale file if any
            if (f.exists()) {
                f.delete();
            }
            if (!f.createNewFile()) {
                return false;
            }
            f.delete();
            return true;
        } catch (IOException ex) {
            return false;
        }
    }

    public static boolean hasStorage(boolean requireWriteAccess) {
        String state = Environment.getExternalStorageState();

        if (Environment.MEDIA_MOUNTED.equals(state)) {
            if (requireWriteAccess) {
                boolean writable = checkFsWritable();
                return writable;
            } else {
                return true;
            }
        } else if (!requireWriteAccess
                && Environment.MEDIA_MOUNTED_READ_ONLY.equals(state)) {
            return true;
        }
        return false;
    }

    private static Cursor query(ContentResolver resolver, Uri uri,
            String[] projection, String selection, String[] selectionArgs,
            String sortOrder) {
        try {
            if (resolver == null) {
                return null;
            }
            return resolver.query(
                    uri, projection, selection, selectionArgs, sortOrder);
         } catch (UnsupportedOperationException ex) {
            return null;
        }

    }

    public static boolean isMediaScannerScanning(ContentResolver cr) {
        boolean result = false;
        Cursor cursor = query(cr, MediaStore.getMediaScannerUri(),
                new String [] {MediaStore.MEDIA_SCANNER_VOLUME},
                null, null, null);
        if (cursor != null) {
            if (cursor.getCount() == 1) {
                cursor.moveToFirst();
                result = "external".equals(cursor.getString(0));
            }
            cursor.close();
        }

        return result;
    }
}
