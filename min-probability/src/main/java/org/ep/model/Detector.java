package org.ep.model;

import java.math.BigDecimal;

import io.vavr.Tuple2;

import static org.ep.helper.Util.*;

public class Detector extends Pair<BigDecimal, BigDecimal> {

    public Detector(BigDecimal first, BigDecimal second) {
        super(first, second);
    }

    protected Detector(Tuple2<BigDecimal, BigDecimal> delegate) {
        super(delegate);
    }

    public static Detector of(Double x, Double y) {
        return new Detector(newBD(x), newBD(y));
    }

    @Override
    protected <P extends Pair<BigDecimal, BigDecimal>> P ctor(Tuple2<BigDecimal, BigDecimal> delegate) {
        return (P) new Detector(delegate);
    }

}
