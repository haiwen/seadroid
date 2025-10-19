package com.seafile.seadroid2.framework.model.livephoto

import android.net.Uri

data class LivePhotoModel(
    val imageUri: Uri,
    val videoUri: Uri,
    val displayName: String,
    val takenTime: Long
)
