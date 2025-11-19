package com.seafile.seadroid2.annotation;

import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.TYPE;

import java.lang.annotation.Target;

/**
 * This feature is no longer supported and maintained.
 * After a certain period of time, it will be modified to @Deprecated
 */
@Target({TYPE, METHOD, CONSTRUCTOR, FIELD})
public @interface NoLongerSupported {
}
