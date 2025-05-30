package com.seafile.seadroid2.enums;

/**
 * <pre>
 * NOT_SELECTABLE: no select
 * ONLY_ACCOUNT: only select account
 * ONLY_REPO: only select repo
 * </pre>
 */
public enum ObjSelectType {
    NOT_SELECTABLE(-1),
    ACCOUNT(0),
    REPO(1),
    DIR(2);

    ObjSelectType(int i) {

    }
}
