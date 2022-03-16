package org.ep.model;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import static java.lang.annotation.RetentionPolicy.*;
import static java.lang.annotation.ElementType.*;

import com.pholser.junit.quickcheck.generator.GeneratorConfiguration;

@Target({PARAMETER, FIELD, ANNOTATION_TYPE, TYPE_USE})
@Retention(RUNTIME)
@GeneratorConfiguration
public @interface WithinGrid {
    double x() default 100_000d;
    double y() default 100_000d;
}
