package org.ep.helper;

import java.math.BigDecimal;
import java.math.RoundingMode;

public abstract class Util {
    private Util() {
        throw new RuntimeException("Not meant to be instantiated.");
    }

    public static BigDecimal newBD(Double coordinate) {
        return new BigDecimal(coordinate).setScale(15, RoundingMode.HALF_UP);
    }
}
