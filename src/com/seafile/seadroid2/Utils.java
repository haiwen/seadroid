package com.seafile.seadroid2;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TreeMap;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.NetworkInfo.DetailedState;
import android.net.Uri;
import android.os.Environment;
import android.provider.MediaStore;
import android.util.Log;
import android.webkit.MimeTypeMap;

import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.fileschooser.SelectableFile;

public class Utils {
    public static final String MIME_APPLICATION_OCTET_STREAM = "application/octet-stream";

    public static JSONObject parseJsonObject(String json) {
        if (json == null) {
            // the caller should not give null
            Log.w("Utils", "null in parseJsonObject");
            return null;
        }

        try {
            return (JSONObject) new JSONTokener(json).nextValue();
        } catch (Exception e) {
            return null;
        }
    }

    public static JSONArray parseJsonArray(String json) {
        if (json == null) {
         // the caller should not give null
            Log.w("Utils", "null in parseJsonObject");
            return null;
        }

        try {
            return (JSONArray) new JSONTokener(json).nextValue();
        } catch (Exception e) {
            return null;
        }
    }

    /** Read input stream and convert the content to string.
     */
    public static String readIt(InputStream stream) throws IOException,
            UnsupportedEncodingException {
        Reader reader = new InputStreamReader(stream, "UTF-8");
        char[] buffer = new char[1024];
        StringBuilder responseStrBuilder = new StringBuilder();

        while (true) {
            int len = reader.read(buffer, 0, 1024);
            if (len == -1)
                break;
            responseStrBuilder.append(buffer, 0, len);
        }
        return responseStrBuilder.toString();
    }

    public static String readFile(File file) {
        Reader reader = null;
        try {
            try {
                // TODO: detect a file's encoding
                reader = new InputStreamReader(new FileInputStream(file), "UTF-8");
            } catch (UnsupportedEncodingException e) {
                return null;
            }

            char[] buffer = new char[1024];
            StringBuilder responseStrBuilder = new StringBuilder();

            while (true) {
                int len = reader.read(buffer, 0, 1024);
                if (len == -1)
                    break;
                responseStrBuilder.append(buffer, 0, len);
            }
            return responseStrBuilder.toString();
        } catch (IOException e) {
            return null;
        } finally {
            try {
                if (reader != null)
                    reader.close();
            } catch (Exception e) {

            }
        }
    }

    public static String getParentPath(String path) {
        if (path == null) {
            // the caller should not give null
            Log.w("Utils", "null in getParentPath");
            return null;
        }

        String parent = path.substring(0, path.lastIndexOf("/"));
        if (parent.equals("")) {
            return "/";
        } else
            return parent;
    }

    public static String fileNameFromPath(String path) {
        if (path == null) {
            // the caller should not give null
            Log.w("Utils", "null in getParentPath");
            return null;
        }

        return path.substring(path.lastIndexOf("/") + 1);
    }

    public static String readableFileSize(long size) {
        if(size <= 0) return "0 KB";
        final String[] units = new String[] { "B", "KB", "MB", "GB", "TB" };
        int digitGroups = (int) (Math.log10(size)/Math.log10(1024));
        return new DecimalFormat("#,##0.#").format(size/Math.pow(1024, digitGroups)) + " " + units[digitGroups];
    }

    public static void writeFile(File file, String content) throws IOException {
        OutputStream os = null;
        try {
            os = new FileOutputStream(file);
            os.write(content.getBytes());
        } finally {
            try {
                if (os != null)
                    os.close();
            } catch (Exception e) {
                // ignore
            }
        }
    }

    public static String NOGROUP = "$nogroup";

    public static TreeMap<String, List<SeafRepo>> groupRepos(List<SeafRepo> repos) {
        TreeMap<String, List<SeafRepo>> map = new TreeMap<String, List<SeafRepo>>();
        for (SeafRepo repo : repos) {
            List<SeafRepo> l;
            String groupName = repo.isGroupRepo ? repo.owner : NOGROUP;
            l = map.get(groupName);
            if (l == null) {
                l = new ArrayList<SeafRepo>();
                map.put(groupName, l);
            }
            l.add(repo);
        }
        return map;
    }

    public static int getResIdforMimetype(String mimetype) {
        if (mimetype == null)
            return R.drawable.file;

        if (mimetype.contains("pdf")) {
            return R.drawable.file_pdf;
        } else if (mimetype.contains("image")) {
            return R.drawable.file_image;
        } else if (mimetype.contains("text")) {
            return R.drawable.file_text;
        } else if (mimetype.contains("audio")) {
            return R.drawable.file_audio;
        } else if (mimetype.contains("video")) {
            return R.drawable.file_video;
        } if (mimetype.contains("pdf")) {
            return R.drawable.file_pdf;
        } else if (mimetype.contains("msword") || mimetype.contains("ms-word")) {
            return R.drawable.file_ms_word;
        } else if (mimetype.contains("mspowerpoint") || mimetype.contains("ms-powerpoint")) {
            return R.drawable.file_ms_ppt;
        } else if (mimetype.contains("msexcel") || mimetype.contains("ms-excel")) {
            return R.drawable.file_ms_excel;
        } else if (mimetype.contains("openxmlformats-officedocument")) {
            // see http://stackoverflow.com/questions/4212861/what-is-a-correct-mime-type-for-docx-pptx-etc
            if (mimetype.contains("wordprocessingml")) {
                return R.drawable.file_ms_word;
            } else if (mimetype.contains("spreadsheetml")) {
                return R.drawable.file_ms_excel;
            } else if (mimetype.contains("presentationml")) {
                return R.drawable.file_ms_ppt;
            }
        // } else if (mimetype.contains("application")) {
        //     return R.drawable.file_binary;
        }

        return R.drawable.file;
    }

    static HashMap<String, Integer> suffixIconMap = null;

    static private HashMap<String, Integer> getSuffixIconMap() {
        if (suffixIconMap != null)
            return suffixIconMap;

        suffixIconMap = new HashMap<String, Integer>();
        suffixIconMap.put("pdf", R.drawable.file_pdf);
        suffixIconMap.put("doc", R.drawable.file_ms_word);
        suffixIconMap.put("docx", R.drawable.file_ms_word);
        suffixIconMap.put("md", R.drawable.file_text);
        suffixIconMap.put("markdown", R.drawable.file_text);
        return suffixIconMap;
    }

    public static int getFileIcon(String name) {
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.length() == 0) {
            return R.drawable.file;
        }

        HashMap<String, Integer> map = getSuffixIconMap();
        Integer i = map.get(suffix);
        if (i != null)
            return i;

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return getResIdforMimetype(mime);
    }

    public static boolean isViewableImage(String name) {
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.length() == 0)
            return false;
        if (suffix.equals("svg"))
            // don't support svg preview
            return false;

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        if (mime == null)
            return false;
        return mime.contains("image");
    }

    public static boolean isNetworkOn() {
        ConnectivityManager connMgr = (ConnectivityManager)
                SeadroidApplication.getAppContext().getSystemService(
                        Context.CONNECTIVITY_SERVICE);

        NetworkInfo wifi = connMgr.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
        if(wifi != null && wifi.isAvailable()
           && wifi.getDetailedState() == DetailedState.CONNECTED) {
            return true;
        }

        NetworkInfo mobile = connMgr.getNetworkInfo(ConnectivityManager.TYPE_MOBILE);
        if(mobile != null && mobile.isAvailable()
           && mobile.getDetailedState() == DetailedState.CONNECTED) {
            return true;
        }

        return false;
    }

    public static String pathJoin (String first, String... rest) {
        String path = first;
        for (String b: rest) {
            if (path.endsWith("/") && b.startsWith("/")) {
                path = path + b.substring(1);
            } else if (path.endsWith("/") || b.startsWith("/")) {
                path += b;
            } else {
                path += "/" + b;
            }
        }

        return path;
    }

    /**
     * Strip leading and trailing slashes
     */
    public static String stripSlashes(String a) {
        return a.replaceAll("^[/]*|[/]*$", "");
    }

    /**
     * Translate commit time to human readable time description
     */
    public static String translateCommitTime(long timestampInMillis) {
        long now = Calendar.getInstance().getTimeInMillis();
        if (now <= timestampInMillis) {
            return SeadroidApplication.getAppContext().getString(R.string.just_now);
        }

        long delta = (now - timestampInMillis) / 1000;

        long secondsPerDay = 24 * 60 * 60;

        long days = delta / secondsPerDay;
        long seconds = delta % secondsPerDay;

        if (days >= 14) {
            Date d = new Date(timestampInMillis);
            SimpleDateFormat fmt = new SimpleDateFormat("yyyy-MM-dd");
            return fmt.format(d);
        } else if (days > 0) {
            return days + SeadroidApplication.getAppContext().getString(R.string.days_ago);
        } else if (seconds >= 60 * 60) {
            long hours = seconds / 3600;
            return hours + SeadroidApplication.getAppContext().getString(R.string.hours_ago);
        } else if (seconds >= 60) {
            long minutes = seconds / 60;
            return minutes + SeadroidApplication.getAppContext().getString(R.string.minutes_ago);
        } else if (seconds > 0) {
            return seconds + SeadroidApplication.getAppContext().getString(R.string.seconds_ago);
        } else {
            return SeadroidApplication.getAppContext().getString(R.string.just_now);
        }
    }

    public static long now() {
        return Calendar.getInstance().getTimeInMillis();
    }

    public static String getFileMimeType(String path) {
        String name = fileNameFromPath(path);
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.length() == 0) {
            return MIME_APPLICATION_OCTET_STREAM;
        } else {
            String mime =  MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
            if (mime != null) {
                return mime;
            } else {
                return MIME_APPLICATION_OCTET_STREAM;
            }
        }
    }

    public static String getFileMimeType(File file) {
        return getFileMimeType(file.getPath());
    }

    public static void copyFile(File src, File dst) throws IOException {
        InputStream in = new BufferedInputStream(new FileInputStream(src));
        OutputStream out = new BufferedOutputStream(new FileOutputStream(dst));

        // Transfer bytes from in to out
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
    }
    
    
    /************ MutiFileChooser ************/
    
    private static final String HIDDEN_PREFIX = ".";

    private static Comparator<SelectableFile> mComparator = new Comparator<SelectableFile>() {
        public int compare(SelectableFile f1, SelectableFile f2) {
            // Sort alphabetically by lower case, which is much cleaner
            return f1.getName().toLowerCase().compareTo(
                    f2.getName().toLowerCase());
        }
    };
    
    private static FileFilter mImageFilter = new FileFilter() {
		public boolean accept(File file) {
			final String fileName = file.getName();
			// Return files only (not directories) and skip hidden files
			if (file.isFile() && !fileName.startsWith(HIDDEN_PREFIX)) {
				for (String ext : imageExtensions) {
					if (fileName.endsWith("." + ext)) {
						return true;
					}
				}
			}
			return false;
		}
    };
    private static FileFilter mFileFilter = new FileFilter() {
        public boolean accept(File file) {
            final String fileName = file.getName();
            // Return files only (not directories) and skip hidden files
            return file.isFile() && !fileName.startsWith(HIDDEN_PREFIX);
        }
    };
    

    private static FileFilter mDirFilter = new FileFilter() {
        public boolean accept(File file) {
            final String fileName = file.getName();
            // Return directories only and skip hidden directories
            return file.isDirectory() && !fileName.startsWith(HIDDEN_PREFIX);
        }
    };
    
    private static String imageExtensions[] ={"png","PNG","jpg","JPG","jpeg","JPEG","bmp","BMP","gif","GIF"};
    /**
     * @author Logan Guo
     * {@link http://stackoverflow.com/questions/3873496/how-to-get-image-path-from-images-stored-on-sd-card}
     */
    private static FileFilter filterForImageFolders = new FileFilter() 
    {            
        public boolean accept(File folder) 
        { 
            try 
            { 
                //Checking only directories, since we are checking for files within 
                //a directory 
                if(folder.isDirectory() && !folder.getName().startsWith(HIDDEN_PREFIX)) 
                { 
                    File[] listOfFiles = folder.listFiles(); 

                    if (listOfFiles == null) return false; 

                    //For each file in the directory... 
                    for (File file : listOfFiles) 
                    {                            
                        //Check if the extension is one of the supported filetypes                           
                        //imageExtensions is a String[] containing image filetypes (e.g. "png")
                        for (String ext : imageExtensions) 
                        { 
                            if (file.getName().endsWith("." + ext)) return true; 
                        } 
                    }                        
                } 
                return false; 
            } 
            catch (SecurityException e) 
            { 
                Log.v("debug", "Access Denied"); 
                return false; 
            } 
        } 
    };
    
    public static List<SelectableFile> getFileList(String path, List<File> selectedFile) {
        ArrayList<SelectableFile> list = new ArrayList<SelectableFile>();

        // Current directory File instance
        final SelectableFile pathDir = new SelectableFile(path);

        // List file in this directory with the directory filter
        final SelectableFile[] dirs = pathDir.listFiles(mDirFilter);
        if (dirs != null) {
            // Sort the folders alphabetically
            Arrays.sort(dirs, mComparator);
            // Add each folder to the File list for the list adapter
            for (SelectableFile dir : dirs) list.add(dir);
        }

        // List file in this directory with the file filter
        final SelectableFile[] files = pathDir.listFiles(mFileFilter);
        if (files != null) {
            // Sort the files alphabetically
            Arrays.sort(files, mComparator);
            // Add each file to the File list for the list adapter
            for (SelectableFile file : files) {
                if (selectedFile != null) {
                    if (selectedFile.contains(file.getFile())) {
                        file.setSelected(true);
                    }
                }
                list.add(file);
            }
        }

        return list;
    }

	public static List<SelectableFile> getImageFoldersList(String path, List<File> selectedFile) {
		ArrayList<SelectableFile> list = new ArrayList<SelectableFile>();

		// Current directory File instance
		final SelectableFile pathDir = new SelectableFile(Environment.getExternalStorageDirectory().toString() + "/DCIM/");

		// List file in this directory with the directory filter
		final SelectableFile[] dirs = pathDir.listFiles(filterForImageFolders);
		if (dirs != null) {
			// Sort the folders alphabetically
			Arrays.sort(dirs, mComparator);
			// Add each folder to the File list for the list adapter
			for (SelectableFile dir : dirs)
				list.add(dir);
		}

		/*
		// List file in this directory with the file filter
		final SelectableFile[] files = pathDir.listFiles(mFileFilter);
		if (files != null) {
			// Sort the files alphabetically
			Arrays.sort(files, mComparator);
			// Add each file to the File list for the list adapter
			for (SelectableFile file : files) {
				if (selectedFile != null) {
					if (selectedFile.contains(file.getFile())) {
						file.setSelected(true);
					}
				}
				list.add(file);
			}
		}
		 */
		return list;
	}
    /**
	 * 获取SD卡的根目录，末尾带\
	 * @author Logan Guo
	 * @return
	 */
	public static String getSDRoot() {
		return Environment.getExternalStorageDirectory().getAbsolutePath()
				+ File.separator;
	}
    /**
	 * list all sub directories under root
	 * @author Logan Guo
	 * @param path
	 * @return 绝对路径
	 */
	public static List<String> listPath(String root) {
		List<String> allDir = new ArrayList<String>();
		SecurityManager checker = new SecurityManager();
		File path = new File(root);
		checker.checkRead(root);
		if (path.isDirectory()) {
			for (File f : path.listFiles()) {
				if (f.isDirectory()) {
					allDir.add(f.getAbsolutePath());
				}
			}
		}
		return allDir;
	}
	/**
	 * 截取路径名
	 * 
	 * @return
	 */
	public static String getPathName(String absolutePath) {
		int start = absolutePath.lastIndexOf(File.separator) + 1;
		int end = absolutePath.length();
		return absolutePath.substring(start, end);
	}
    /**
     * modified by Logan Guo 
     **/
    public static List<SelectableFile> getImagesAutoBackupFolders(ContentResolver mContentResolver, String path, List<File> selectedFile) {
        ArrayList<SelectableFile> list = new ArrayList<SelectableFile>();

        // Current directory File instance
        final SelectableFile pathDir = new SelectableFile(path);
        
        // List file in this directory with the directory filter
        final SelectableFile[] dirs = pathDir.listFiles(mDirFilter);
        System.out.println("Utils: "+ dirs.toString());
        String[] projection = {
                MediaStore.Images.Media._ID, MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Images.Media.DISPLAY_NAME
                };
                String selection = MediaStore.Images.Media.BUCKET_DISPLAY_NAME + " = ?";
                String[] selectionArgs = new String[] {
                    "Camera"
                };

                Cursor mImageCursor = mContentResolver.query(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, projection, selection, selectionArgs, null ); 

                if (mImageCursor != null)
                {

                    mImageCursor.moveToFirst();

                    for (int i = 0; i < mImageCursor.getCount(); i++)
                    {
//                        Images im=new Images();
//                        eachImageView=new ImageView(this);
                        int imageId = mImageCursor.getInt((mImageCursor.getColumnIndex( MediaStore.Images.Media._ID)));
                        Bitmap bm = MediaStore.Images.Thumbnails.getThumbnail(mContentResolver, imageId, MediaStore.Images.Thumbnails.MINI_KIND, null);
//                        im.setBitmap(bm);
//                        eachImageView.setImageBitmap(bm);
//                        im.setImageView(eachImageView);
//
//                        arrayOfImages.add(im);
                        System.out.println("Utils-->imageId: "+ imageId);
                        mImageCursor.moveToNext();
                    }
                }      
        
        return list;
    }
    
    /**
     * modified by Logan Guo 
     **/
    public static List<SelectableFile> getImagesAutoBackupFolders2(String path, List<File> selectedFile) {
        ArrayList<SelectableFile> list = new ArrayList<SelectableFile>();

        // Current directory File instance
        final SelectableFile pathDir = new SelectableFile(path);
        
        // List file in this directory with the directory filter
        final SelectableFile[] dirs = pathDir.listFiles(mDirFilter);
        System.out.println("Utils: "+ dirs.toString());
        if (dirs != null) {
            // Sort the folders alphabetically
            Arrays.sort(dirs, mComparator);
            // Add each folder to the File list for the list adapter
			for (SelectableFile dir : dirs) {
				// Check if a image folder
				System.out.println("Utils: "+ dir.listFiles(mImageFilter).length);
				if (dir != null && dir.listFiles(mDirFilter).length == 0 && dir.listFiles(mImageFilter).length >= 1) {
					list.add(dir);
				}

			} 
        }

        // List file in this directory with the file filter
        final SelectableFile[] files = pathDir.listFiles(mImageFilter);
        if (files != null) {
            // Sort the files alphabetically
            Arrays.sort(files, mComparator);
            // Add each file to the File list for the list adapter
            for (SelectableFile file : files) {
                if (selectedFile != null) {
                    if (selectedFile.contains(file.getFile())) {
                        file.setSelected(true);
                    }
                }
                list.add(file);
            }
        }       
        
        return list;
    }

    public static Intent createGetContentIntent() {
        // Implicitly allow the user to select a particular kind of data
        final Intent intent = new Intent(Intent.ACTION_GET_CONTENT); 
        // The MIME data type filter
        intent.setType("*/*"); 
        // Only return URIs that can be opened with ContentResolver
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        return intent;
    }
    
    public static String getPath(Context context, Uri uri) throws URISyntaxException {

        if ("content".equalsIgnoreCase(uri.getScheme())) {
            String[] projection = { "_data" };
            Cursor cursor = null;

            try {
                cursor = context.getContentResolver().query(uri, projection, null, null, null);
                int column_index = cursor
                .getColumnIndexOrThrow("_data");
                if (cursor.moveToFirst()) {
                    return cursor.getString(column_index);
                }
            } catch (Exception e) {
                // Eat it
            }
        }

        else if ("file".equalsIgnoreCase(uri.getScheme())) {
            return uri.getPath();
        }

        return null;
    }

    public static String getStackTrace(Exception e) {
        StringWriter buffer = new StringWriter();
        PrintWriter writer = new PrintWriter(buffer);
        e.printStackTrace(writer);
        return buffer.toString();
    }
}
