package com.seafile.seadroid2.data;

import android.util.Log;

import com.seafile.seadroid2.R;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Seafile event entity
 */
public class SeafEvent implements SeafItem {
    public static final String DEBUG_TAG = SeafItem.class.getSimpleName();

    public static final String EVENT_TYPE_REPO_CREATE = "repo-create";
    public static final String EVENT_TYPE_REPO_DELETE = "repo-delete";

    // true for events like a file upload by unregistered user from a
    // uploadable link
    private boolean anonymous;
    private String repo_id;
    private String author;
    private String nick;
    private long time;
    private String etype;
    private String repo_name;
    private String desc;
    private String commit_id;
    private String date;
    private String name;
    private String time_relative;
    private String converted_cmmt_desc;
    private String avatar;
    private boolean repo_encrypted;
    private boolean more_files;

    public static SeafEvent fromJson(JSONObject obj) {
        SeafEvent event = new SeafEvent();
        try {
            event.author = obj.optString("author");
            if (event.author.isEmpty()) {
                event.author = "anonymous";
                event.anonymous = true;
            } else {
                event.anonymous = false;
            }

            event.repo_id = obj.optString("repo_id");
            event.nick = obj.optString("nick");
            if (event.nick.isEmpty()) {
                event.nick = "anonymous";
            }

            event.etype = obj.optString("etype");
            event.repo_name = obj.optString("repo_name");
            event.time = obj.getLong("time");
            event.avatar = obj.optString("avatar");
            event.commit_id = obj.optString("commit_id");
            event.date = obj.optString("date");
            event.name = obj.optString("name");
            event.time_relative = obj.optString("time_relative");
            event.converted_cmmt_desc = obj.optString("converted_cmmt_desc");
            event.repo_encrypted = obj.getBoolean("repo_encrypted");
            event.more_files = obj.getBoolean("more_files");

            event.desc = obj.optString("desc");
            if (event.etype.equals(EVENT_TYPE_REPO_CREATE)) {
                event.desc = String.format("Created library \"%s\"", event.repo_name);
            } else if (event.etype.equals(EVENT_TYPE_REPO_DELETE)) {
                event.desc = String.format("Deleted library \"%s\"", event.repo_name);
            }

            event.desc = translateCommitDesc(event.desc);
            return event;
        } catch (JSONException e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        }
    }

    private static Matcher fullMatch(Pattern pattern, String str) {
        Matcher matcher = pattern.matcher(str);
        return matcher.matches() ? matcher : null;
    }

    public static String translateCommitDesc(String value) {
        if (value.startsWith("Reverted repo")) {
            value.replace("repo", "library");
        }

        if (value.startsWith("Reverted library")) {
            return value.replace("Reverted library to status at", "Reverted library to status at");
        } else if (value.startsWith("Reverted file")) {
            String regex = "Reverted file \"(.*)\" to status at (.*)";
            Pattern pattern = Pattern.compile(regex);
            Matcher matcher;
            if ((matcher = fullMatch(pattern, value)) != null) {
                String name = matcher.group(1);
                String time = matcher.group(2);
                return String.format("Reverted file \"%s\" to status at %s.", name, time);
            }

        } else if (value.startsWith("Recovered deleted directory")) {
            return value.replace("Recovered deleted directory", "Recovered deleted directory");
        } else if (value.startsWith("Changed library")) {
            return value.replace("Changed library name or description", "Changed library name or description");
        } else if (value.startsWith("Merged") || value.startsWith("Auto merge")) {
            return "Auto merge by seafile system";
        }

        final String[] lines = value.split("\n");
        StringBuilder out = new StringBuilder();

        for (int i = 0; i < lines.length; i++) {
            final String translateLine = translateLine(lines[i]);
            out.append(translateLine);
            // should avoid append for the last item
            if (i < lines.length - 1) out.append("\n");
        }

        return out.toString();
    }

    private static String translateLine(String line) {
        // String regex = String.format("(%s) \"(.*)\"\\s?(and ([0-9]+) more (files|directories))?", getOperations());
        // String regex = String.format("(%s).* ".*\..*"\s+(and ([0-9]+) more (files|directories))?", getOperations());
        String regex = String.format("(%s).* \"\\S+\\.\\S+\"\\s+(and ([0-9]+) more (files|directories))?", getOperations());
        Pattern pattern = Pattern.compile(regex);

        Matcher matcher;
        if ((matcher = fullMatch(pattern, line)) == null) {
            return line;
        }

        String op = matcher.group(1);
        String file_name = matcher.group(2);
        String has_more = matcher.group(3);
        String n_more = matcher.group(4);
        String more_type = matcher.group(5);

        String op_trans = (getVerbsMap().get(op) == null ? op : getVerbsMap().get(op));

        String type, ret;
        // has more may be null caused a crash
        if (has_more.length() > 0) {
            if (more_type.equals("files")) {
                type = "files";
            } else {
                type = "directories";
            }

            String more = String.format("and %s more", n_more);
            ret = String.format("%s \"%s\" %s %s.", op_trans, file_name, more, type);
        } else {
            ret = String.format("%s \"%s\".", op_trans, file_name);
        }

        return ret;
    }

    private static HashMap<String, String> verbsMap = null;
    private static HashMap<String, String> getVerbsMap() {
        if (verbsMap == null) {
            verbsMap = new HashMap<>();
            verbsMap.put("Added", "Added");
            verbsMap.put("Deleted", "Deleted");
            verbsMap.put("Removed", "Removed");
            verbsMap.put("Modified", "Modified");
            verbsMap.put("Renamed", "Renamed");
            verbsMap.put("Moved", "Moved");
            verbsMap.put("Added directory", "Added directory");
            verbsMap.put("Removed directory", "Removed directory");
            verbsMap.put("Renamed directory", "Renamed directory");
            verbsMap.put("Moved directory", "Moved directory");
        }

        return verbsMap;
    }

    private static String getOperations() {
        return "Added|Deleted|Removed|Modified|Renamed|Moved|Added directory|Removed directory|Renamed directory|Moved directory";
    }

    public boolean isAnonymous() {
        return anonymous;
    }

    public void setAnonymous(boolean anonymous) {
        this.anonymous = anonymous;
    }

    public void setRepo_id(String repo_id) {
        this.repo_id = repo_id;
    }

    public void setAuthor(String author) {
        this.author = author;
    }

    public void setNick(String nick) {
        this.nick = nick;
    }

    public void setTime(int time) {
        this.time = time;
    }

    public void setEtype(String etype) {
        this.etype = etype;
    }

    public void setRepo_name(String repo_name) {
        this.repo_name = repo_name;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public String getRepo_id() {
        return repo_id;
    }

    public String getAuthor() {
        return author;
    }

    public String getNick() {
        return nick;
    }

    public long getTime() {
        return time;
    }

    public String getEtype() {
        return etype;
    }

    public String getRepo_name() {
        return repo_name;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String getTitle() {
        return desc;
    }

    @Override
    public String getSubtitle() {
        return nick;
    }

    @Override
    public int getIcon() {
        return R.drawable.repo;
    }

    public void setTime(long time) {
        this.time = time;
    }

    public String getCommit_id() {
        return commit_id;
    }

    public void setCommit_id(String commit_id) {
        this.commit_id = commit_id;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTime_relative() {
        return time_relative;
    }

    public void setTime_relative(String time_relative) {
        this.time_relative = time_relative;
    }

    public String getConverted_cmmt_desc() {
        return converted_cmmt_desc;
    }

    public void setConverted_cmmt_desc(String converted_cmmt_desc) {
        this.converted_cmmt_desc = converted_cmmt_desc;
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public boolean isRepo_encrypted() {
        return repo_encrypted;
    }

    public void setRepo_encrypted(boolean repo_encrypted) {
        this.repo_encrypted = repo_encrypted;
    }

    public boolean isMore_files() {
        return more_files;
    }

    public void setMore_files(boolean more_files) {
        this.more_files = more_files;
    }

}
