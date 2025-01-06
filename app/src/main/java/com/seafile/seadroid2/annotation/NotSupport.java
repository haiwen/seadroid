package com.seafile.seadroid2.annotation;

import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.TYPE;

import java.lang.annotation.Target;

/**
 * There are no plans to develop this feature.
 */
@Target({TYPE, METHOD, CONSTRUCTOR, FIELD})
public @interface NotSupport {
}
