package org.ep.model;

import java.math.BigDecimal;

import io.vavr.Tuple2;

import static org.ep.helper.Util.*;

public class Point extends Pair<BigDecimal, BigDecimal> {

    public Point(BigDecimal first, BigDecimal second) {
        super(first, second);
    }

    protected Point(Tuple2<BigDecimal, BigDecimal> delegate) {
        super(delegate);
    }

    @Override
    protected <P extends Pair<BigDecimal, BigDecimal>> P ctor(Tuple2<BigDecimal, BigDecimal> delegate) {
        return (P) new Point(delegate);
    }

    public static Point of(double f, double s) {
        return new Point(newBD(f), newBD(s));
    }
}
