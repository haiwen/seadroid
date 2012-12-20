# Seafile Android Client

## Setup

Download and setup ActionBarSherlock-4.2.0

If your seadroid project is in /home/user/dev/seadroid, download ActionBarSherlock, and untar it to /home/usr/dev/ActionBarSherlock-4.2.0. Then build the library

    cd /home/user/dev/ActionBarSherlock-4.2.0/library
    android update project -p . --target android-17

Then build seadroid

    cd /home/user/dev/seadroid
    ant debug
