package com.seafile.seadroid2.framework.model

import android.util.Log
import com.seafile.seadroid2.R
import org.json.JSONObject
import java.util.regex.Matcher
import java.util.regex.Pattern

/**
 * Seafile event entity.
 */
class SeafEvent : SeafItem {

    var anonymous: Boolean = false
    var repo_id: String? = null
    var author: String? = null
    var nick: String? = null
    var time: Long = 0
    var v_time: String? = null
    var etype: String? = null
    var repo_name: String? = null
    var desc: String? = null
    var commit_id: String? = null
    var date: String? = null
    var name: String? = null
    var time_relative: String? = null
    var converted_cmmt_desc: String? = null
    var avatar: String? = null
    var avatar_url: String? = null
    var repo_encrypted: Boolean = false
    var more_files: Boolean = false
    var path: String? = null
    var op_type: String? = null
    var obj_type: String? = null
    var author_name: String? = null

    override fun getTitle(): String? = desc

    override fun getSubtitle(): String? = nick

    override fun getIcon(): Int = R.drawable.repo

    companion object {
        const val DEBUG_TAG: String = "SeafEvent"

        const val EVENT_TYPE_REPO_CREATE = "repo-create"
        const val EVENT_TYPE_REPO_DELETE = "repo-delete"

        @JvmStatic
        fun fromJson(obj: JSONObject): SeafEvent? {
            val event = SeafEvent()
            return try {
                event.author = obj.optString("author").also { author ->
                    if (author.isNullOrEmpty()) {
                        event.anonymous = true
                        event.author = "anonymous"
                    } else {
                        event.anonymous = false
                    }
                }

                event.repo_id = obj.optString("repo_id")
                event.nick = obj.optString("nick").ifNullOrEmpty { "anonymous" }
                event.author_name = obj.optString("author_name")
                event.path = obj.optString("path")
                event.op_type = obj.optString("op_type")
                event.obj_type = obj.optString("obj_type")
                event.etype = obj.optString("etype")
                event.repo_name = obj.optString("repo_name")
                event.v_time = obj.optString("time")
                event.time = obj.optLong("time")
                event.avatar = obj.optString("avatar")
                event.avatar_url = obj.optString("avatar_url")
                event.commit_id = obj.optString("commit_id")
                event.date = obj.optString("date")
                event.name = obj.optString("name")
                event.time_relative = obj.optString("time_relative")
                event.converted_cmmt_desc = obj.optString("converted_cmmt_desc")
                event.repo_encrypted = obj.optBoolean("repo_encrypted")
                event.more_files = obj.optBoolean("more_files")

                var description = obj.optString("desc")
                when (event.etype) {
                    EVENT_TYPE_REPO_CREATE -> description = "Created library \"${event.repo_name}\""
                    EVENT_TYPE_REPO_DELETE -> description = "Deleted library \"${event.repo_name}\""
                }
                event.desc = translateCommitDesc(description)
                event
            } catch (e: Exception) {
                Log.d(DEBUG_TAG, e.message ?: "Failed to parse SeafEvent")
                null
            }
        }

        private fun String?.ifNullOrEmpty(block: () -> String): String =
            if (this.isNullOrEmpty()) block() else this

        private fun fullMatch(pattern: Pattern, value: String): Matcher? {
            val matcher = pattern.matcher(value)
            return if (matcher.matches()) matcher else null
        }

        @JvmStatic
        fun translateCommitDesc(value: String?): String? {
            if (value.isNullOrEmpty()) {
                return value
            }
            var working = value
            if (working.startsWith("Reverted repo")) {
                working = working.replace("repo", "library")
            }

            if (working.startsWith("Reverted library")) {
                return working.replace("Reverted library to status at", "Reverted library to status at")
            } else if (working.startsWith("Reverted file")) {
                val regex = "Reverted file \"(.*)\" to status at (.*)"
                val pattern = Pattern.compile(regex)
                val matcher = fullMatch(pattern, working)
                if (matcher != null) {
                    val name = matcher.group(1)
                    val time = matcher.group(2)
                    return String.format("Reverted file \"%s\" to status at %s.", name, time)
                }
            } else if (working.startsWith("Recovered deleted directory")) {
                return working.replace("Recovered deleted directory", "Recovered deleted directory")
            } else if (working.startsWith("Changed library")) {
                return working.replace("Changed library name or description", "Changed library name or description")
            } else if (working.startsWith("Merged") || working.startsWith("Auto merge")) {
                return "Auto merge by seafile system"
            } else if (working.startsWith("Deleted")) {
                return working
            }

            val lines = working.split("\n".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
            val out = StringBuilder()
            for (i in lines.indices) {
                out.append(translateLine(lines[i]))
                if (i < lines.size - 1) {
                    out.append("\n")
                }
            }
            return out.toString()
        }

        private fun translateLine(line: String): String {
            val regex = String.format("(%s).* \"\\S+\\.\\S+\"\\s+(and ([0-9]+) more (files|directories))?", getOperations())
            val pattern = Pattern.compile(regex)
            val matcher = fullMatch(pattern, line) ?: return line

            val op = matcher.group(1)
            val fileName = matcher.group(2)
            val hasMore = matcher.group(3)
            val moreCount = matcher.group(4)
            val moreType = matcher.group(5)

            val opTranslate = verbsMap[op] ?: op

            return if (!hasMore.isNullOrEmpty()) {
                val type = if (moreType == "files") "files" else "directories"
                val more = "and $moreCount more"
                String.format("%s \"%s\" %s %s.", opTranslate, fileName, more, type)
            } else {
                String.format("%s \"%s\".", opTranslate, fileName)
            }
        }

        private val verbsMap: MutableMap<String, String> by lazy {
            hashMapOf(
                "Added" to "Added",
                "Deleted" to "Deleted",
                "Removed" to "Removed",
                "Modified" to "Modified",
                "Renamed" to "Renamed",
                "Moved" to "Moved",
                "Added directory" to "Added directory",
                "Removed directory" to "Removed directory",
                "Renamed directory" to "Renamed directory",
                "Moved directory" to "Moved directory"
            )
        }

        private fun getOperations(): String =
            "Added|Deleted|Removed|Modified|Renamed|Moved|Added directory|Removed directory|Renamed directory|Moved directory"
    }
}
