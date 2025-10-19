package com.seafile.seadroid2.framework.model.sdoc

import com.google.gson.annotations.JsonAdapter
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter
import java.time.OffsetDateTime

class SDocCommentReplyModel {
    @JvmField
    var id: Int = 0

    @JvmField
    var comment_id: Int = 0

    @JvmField
    var doc_uuid: String? = null

    @JvmField
    var reply: String? = null

    @JvmField
    var author: String? = null

    @JvmField
    var avatar_url: String? = null

    @JvmField
    var type: String? = null

    @JvmField
    var resolved: String? = null

    @JvmField
    var user_contact_email: String? = null

    @JvmField
    var user_email: String? = null

    @JvmField
    var user_name: String? = null

    @JsonAdapter(OffsetDateTimeAdapter::class)
    @JvmField
    var created_at: OffsetDateTime? = null

    @JsonAdapter(OffsetDateTimeAdapter::class)
    @JvmField
    var updated_at: OffsetDateTime? = null
}
