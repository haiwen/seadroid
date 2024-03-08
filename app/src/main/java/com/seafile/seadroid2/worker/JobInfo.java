package com.seafile.seadroid2.worker;

import java.util.UUID;

public class JobInfo {
    public UUID id = UUID.fromString("00000000-0000-0000-0000-000000000000");
    public String name;
    public String state;
    public String user;
    public String workClass;
    public long startTime;//mills
    public int progress;
}
