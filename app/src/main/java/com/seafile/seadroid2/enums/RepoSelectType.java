package com.seafile.seadroid2.enums;

/**
 * <pre>
 * NOT_SELECTABLE: no select
 * ONLY_ACCOUNT: only select account
 * ONLY_REPO: only select repo
 * </pre>
 */
public enum RepoSelectType {
    NOT_SELECTABLE(-1),
    ONLY_ACCOUNT(0),
    ONLY_REPO(1),
    FOLDER(2);

    RepoSelectType(int i) {

    }
}
