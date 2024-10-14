package com.seafile.seadroid2.annotation;

import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.CLASS;

import androidx.annotation.RequiresOptIn;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * In this project, it only means that this feature is not fully validated, please use it with caution.
 * <p>
 * Referenced in Media3.UnstableApi
 */
@Documented
@Retention(CLASS)
@Target({TYPE, METHOD, CONSTRUCTOR, FIELD})
@androidx.media3.common.util.UnstableApi
@RequiresOptIn(level = RequiresOptIn.Level.ERROR)
public @interface Unstable {
}
