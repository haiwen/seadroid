package com.seafile.seadroid2.data;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.StandardCharsets;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

public abstract class CacheItem<T> {

    private static final String FILE_SUFFIX = ".dat";
    private static final StorageManager storageManager = StorageManager.getInstance();
    private static final byte[] split = ",".getBytes(StandardCharsets.UTF_8);
    private static final byte[] left = "[".getBytes(StandardCharsets.UTF_8);
    private static final byte[] right = "]".getBytes(StandardCharsets.UTF_8);

    private String name;
    private List<Integer> positions;
    private List<Integer> itemSizes;
    private RandomAccessFile file;

    public static String getConfigFilePath(String name){
        return Utils.pathJoin(storageManager.getJsonCacheDir().getAbsolutePath(), name+"-config"+FILE_SUFFIX);
    }

    public static String getDataFilePath(String name){
        return Utils.pathJoin(storageManager.getJsonCacheDir().getAbsolutePath(), name+FILE_SUFFIX);
    }

    public static boolean cacheFileExists(String name){
        File configFile = new File(getConfigFilePath(name));
        if(!configFile.exists()){
            return false;
        }
        File dataFile = new File(getDataFilePath(name));
        if(!dataFile.exists()){
            return false;
        }
        return true;
    }

    public CacheItem(String name) throws IOException{
        this.name = name;
        readContents(name);
    }

    public CacheItem(String name, List<T> caches, Comparator<T> comp) throws IOException{
        this.name = name;
        saveContents(caches, comp);
    }

    protected abstract byte[] toBytes(T item);

    protected abstract T fromBytes(byte[] bytes);

    public void saveContents(List<T> caches, Comparator<T> comp) throws IOException {
        if(caches == null){
            return;
        }
        caches.sort(comp);
        FileOutputStream os = null;
        FileOutputStream configOs = null;
        positions = Lists.newArrayList();
        itemSizes = Lists.newArrayList();
        try {
            os = new FileOutputStream(getDataFilePath(name));
            configOs = new FileOutputStream(getConfigFilePath(name));
            os.write(left);
            int position = left.length;
            for(T item: caches){
                if(position != left.length){
                    os.write(split);
                    position += split.length;
                }
                byte[] content = toBytes(item);
                os.write(content);
                configOs.write(intToBytesLittleEndian(content.length));
                itemSizes.add(content.length);
                positions.add(position);
                position += content.length;
            }
            os.write(right);
            os.close();
            configOs.close();
            file = new RandomAccessFile(new File(getDataFilePath(name)), "r");
        } finally {
            try {
                if (os != null) {
                    os.close();
                }
                if(configOs != null){
                    configOs.close();
                }
            } catch (Exception e) {
                // ignore
            }
        }
    }

    public void readContents(String name) throws IOException{
        FileInputStream is = null;
        try {
            is = new FileInputStream(getConfigFilePath(name));
            byte[] buffer = new byte[Integer.BYTES];
            positions = Lists.newArrayList();
            itemSizes = Lists.newArrayList();
            int position = left.length;
            while (true) {
                if(position != left.length){
                    position += split.length;
                }
                int len = is.read(buffer);
                if (len == -1)
                    break;
                int size = bytesToIntLittleEndian(buffer);
                itemSizes.add(size);
                positions.add(position);
                position += size;
            }
            is.close();
            file = new RandomAccessFile(new File(getDataFilePath(name)), "r");
        }finally {
            try {
                if (is != null) {
                    is.close();
                }
            } catch (Exception e) {
                // ignore
            }
        }
    }

    public int getCount(){
        if(positions == null){
            return 0;
        }
        return positions.size();
    }

    public T get(int i){
        if(i >= getCount() || i < 0){
            return null;
        }
        try {
            file.seek(positions.get(i));
            byte[] buffer = new byte[itemSizes.get(i)];
            int len = file.read(buffer);
            if(len != itemSizes.get(i)){
                return null;
            }
            return fromBytes(buffer);
        }catch (IOException e){
            return null;
        }
    }

    public List<T> getAll() throws IOException{
        List<T> res = Lists.newArrayList();
        int n = getCount();
        for(int i=0;i<n;++i){
            res.add(get(i));
        }
        return res;
    }

    public boolean delete() throws IOException{
        if(file != null){
            file.close();
            file = null;
        }
        File config = new File(getConfigFilePath(name));
        File data = new File(getDataFilePath(name));
        positions = null;
        itemSizes = null;
        return config.delete() && data.delete();
    }

    public boolean exists(){
        return positions != null && itemSizes != null && file != null;
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        if(file != null){
            file.close();
        }
    }

    public static int bytesToIntLittleEndian(byte[] bytes) {
        return bytes[0] & 0xFF | //
                (bytes[1] & 0xFF) << 8 | //
                (bytes[2] & 0xFF) << 16 | //
                (bytes[3] & 0xFF) << 24; //
    }

    public static int bytesToIntBigEndian(byte[] bytes) {
        return bytes[3] & 0xFF | //
                (bytes[2] & 0xFF) << 8 | //
                (bytes[1] & 0xFF) << 16 | //
                (bytes[0] & 0xFF) << 24; //
    }

    public static byte[] intToBytesLittleEndian(int value) {
        byte[] bytes = new byte[Integer.BYTES];
        bytes[0] = (byte) (value & 0xff);
        bytes[1] = (byte) ((value >> Byte.SIZE) & 0xff);
        bytes[2] = (byte) ((value >> Byte.SIZE * 2) & 0xff);
        bytes[3] = (byte) ((value >> Byte.SIZE * 3) & 0xff);
        return bytes;
    }

    public static byte[] intToBytesBigEndian(int value) {
        byte[] bytes = new byte[Integer.BYTES];
        bytes[3] = (byte) (value & 0xff);
        bytes[2] = (byte) ((value >> Byte.SIZE) & 0xff);
        bytes[1] = (byte) ((value >> Byte.SIZE * 2) & 0xff);
        bytes[0] = (byte) ((value >> Byte.SIZE * 3) & 0xff);
        return bytes;
    }


}
