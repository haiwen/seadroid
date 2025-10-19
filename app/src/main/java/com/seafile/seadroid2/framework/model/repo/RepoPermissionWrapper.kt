package com.seafile.seadroid2.framework.model.repo

import com.seafile.seadroid2.framework.db.entities.PermissionEntity
import com.seafile.seadroid2.framework.db.entities.RepoModel

class RepoPermissionWrapper(
    var repoModel: RepoModel?,
    var permission: PermissionEntity?
)
