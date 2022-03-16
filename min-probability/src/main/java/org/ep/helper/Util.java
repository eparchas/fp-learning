package org.ep.helper;

import java.io.File;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import io.vavr.Predicates;
import io.vavr.control.Option;

public abstract class Util {
    private Util() {
        throw new RuntimeException("Not meant to be instantiated.");
    }

    public static int SCALE = 20;
    public static RoundingMode ROUNDING_MODE = RoundingMode.HALF_EVEN;

    public static BigDecimal newBD(Double coordinate) {
        return new BigDecimal(coordinate).setScale(SCALE, ROUNDING_MODE);
    }

    public static BigDecimal round(BigDecimal toRound) {
        return round(toRound, 3);
    }

    public static BigDecimal round(BigDecimal toRound, int digits) {
        return toRound.round(new MathContext(digits, ROUNDING_MODE));
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
