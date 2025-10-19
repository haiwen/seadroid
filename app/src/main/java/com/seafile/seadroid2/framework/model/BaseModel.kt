package com.seafile.seadroid2.framework.model

import androidx.room.Ignore
import com.seafile.seadroid2.config.Constants
import com.seafile.seadroid2.enums.ItemPositionEnum
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity
import com.seafile.seadroid2.ui.data_migrate.DataMigrationActivity

/**
 * Controls data versioning for models that participate in migrations.
 *
 * When [v] == 0 legacy data may be incomplete, see [FileBackupStatusEntity] and [DataMigrationActivity].
 * When [v] == 1 the record has been migrated (app version >= 3.0.0). Default is 1.
 */
open class BaseModel {
    /**
     * Controls data versioning for models that participate in migrations.
     *
     * When [v] == 0 legacy data may be incomplete, see [FileBackupStatusEntity] and [DataMigrationActivity].
     * When [v] == 1 the record has been migrated (app version >= 3.0.0). Default is 1.
     */
    @JvmField
    var v: Int = 1

    /**
     * @see com.seafile.seadroid2.config.Constants.DataStatus
     */
    @JvmField
    var data_status: Int = Constants.DataStatus.NORMAL

    @Ignore
    @JvmField
    var is_checked: Boolean = false

    @Ignore
    @JvmField
    var checkable: Boolean = true

    @Ignore
    @JvmField
    var item_position: ItemPositionEnum? = null
}
