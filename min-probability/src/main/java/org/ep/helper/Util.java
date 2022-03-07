package org.ep.helper;

import java.io.File;
import java.math.BigDecimal;
import java.math.RoundingMode;

import io.vavr.Predicates;
import io.vavr.control.Option;

public abstract class Util {
    private Util() {
        throw new RuntimeException("Not meant to be instantiated.");
    }

    public static BigDecimal newBD(Double coordinate) {
        return new BigDecimal(coordinate).setScale(15, RoundingMode.HALF_UP);
    }

    public static BigDecimal round(BigDecimal toRound) {
        return BigDecimal.ZERO;
    }

    public static Option<File> toValidFile(String fileName) {
        return Option.of(fileName)
                .filter(Predicates.not(String::isBlank))
                .map(File::new)
                .filter(File::exists)
                .filter(File::canRead)
                .filter(Predicates.not(File::isHidden));
    }
}
