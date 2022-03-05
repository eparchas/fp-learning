package org.ep.model;

import java.math.BigDecimal;
import static org.ep.helper.Util.*;

public class Detector extends Pair<BigDecimal, BigDecimal> {

    public Detector(BigDecimal first, BigDecimal second) {
        super(first, second);
    }

    public static Detector of(Double x, Double y) {
        return new Detector(newBD(x), newBD(y));
    }

}
