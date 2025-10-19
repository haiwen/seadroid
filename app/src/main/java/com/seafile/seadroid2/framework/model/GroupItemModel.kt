package com.seafile.seadroid2.framework.model

import androidx.annotation.StringRes
import com.seafile.seadroid2.SeadroidApplication
import com.seafile.seadroid2.framework.db.entities.RepoModel

class GroupItemModel : BaseModel {
    @JvmField
    var title: String? = null

    @JvmField
    var is_expanded: Boolean = true

    @JvmField
    val repo_list: MutableList<RepoModel> = mutableListOf()

    constructor(@StringRes nameRes: Int) : super() {
        checkable = false
        title = SeadroidApplication.getAppString(nameRes)
    }

    constructor(@StringRes nameRes: Int, repoList: List<RepoModel>) : super() {
        repo_list.addAll(repoList)
        checkable = false
        title = SeadroidApplication.getAppString(nameRes)
    }

    constructor(title: String, repoList: List<RepoModel>) : super() {
        this.title = title
        repo_list.addAll(repoList)
        checkable = false
    }

    fun addAllRepoList(repoList: List<RepoModel>) {
        repo_list.clear()
        repo_list.addAll(repoList)
    }

    fun getRepoList(): List<RepoModel> = repo_list

    fun getTitle(): String? = title

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is GroupItemModel) return false
        return title == other.title
    }

    override fun hashCode(): Int = title?.hashCode() ?: 0
}
