package com.seafile.seadroid2.framework.model.docs_comment

import com.google.gson.annotations.JsonAdapter
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter
import com.seafile.seadroid2.framework.model.sdoc.SDocCommentReplyModel
import com.seafile.seadroid2.view.rich_edittext.RichEditText
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

class DocsCommentModel {
    @JvmField
    var id: Int = 0

    @JvmField
    var item_name: String? = null

    @JvmField
    var parent_path: String? = null

    @JvmField
    var avatar_url: String? = null

    @JvmField
    var replies: List<SDocCommentReplyModel>? = null

    @JvmField
    var comment: String? = null

    @JvmField
    var repo_id: String? = null

    @JvmField
    var resolved: Boolean = false

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

    @JvmField
    var detail: String? = null

    @JvmField
    var isContainImage: Boolean = false

    @JvmField
    var commentList: MutableList<RichEditText.RichContentModel>? = null

    fun getCreatedAtFriendlyText(): String {
        val created = created_at ?: return ""
        return created.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace("T", " ")
    }
}
