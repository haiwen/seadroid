package com.seafile.seadroid2.framework.model.adapter

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonParseException
import com.google.gson.JsonSerializationContext
import com.google.gson.JsonSerializer
import java.lang.reflect.Type
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

class OffsetDateTimeAdapter : JsonSerializer<OffsetDateTime>, JsonDeserializer<OffsetDateTime> {
    override fun serialize(
        src: OffsetDateTime,
        typeOfSrc: Type,
        context: JsonSerializationContext
    ): JsonElement = context.serialize(src.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))

    @Throws(JsonParseException::class)
    override fun deserialize(
        json: JsonElement,
        typeOfT: Type,
        context: JsonDeserializationContext
    ): OffsetDateTime = OffsetDateTime.parse(json.asString, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
}
