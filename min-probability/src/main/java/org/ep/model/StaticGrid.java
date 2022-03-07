package org.ep.model;

import java.math.BigDecimal;
import java.util.function.Function;

import io.vavr.collection.List;

public class StaticGrid implements Grid {
    final BigDecimal step;
    final BigDecimal minX;
    final BigDecimal maxX;
    final BigDecimal minY;
    final BigDecimal maxY;

    public StaticGrid(BigDecimal step, BigDecimal minX, BigDecimal maxX, BigDecimal minY, BigDecimal maxY) {
        this.step = step;
        this.minX = minX;
        this.maxX = maxX;
        this.minY = minY;
        this.maxY = maxY;
    }

    @Override
    public int size() {
        return maxX.subtract(minX).divide(step)
                .multiply(
                        maxY.subtract(minY).divide(step)
                ).intValue();
    }

    private BigDecimal addStep(BigDecimal dim) {
        return dim.add(step);
    }

    private BigDecimal subtractStep(BigDecimal dim) {
        return dim.subtract(step);
    }

    private boolean withinX(BigDecimal xdim) {
        return this.minX.compareTo(xdim) <= 0 &&
            xdim.compareTo(maxX) <= 0;
    }

    private boolean withinY(BigDecimal ydim) {
        return this.minY.compareTo(ydim) <= 0 &&
            ydim.compareTo(maxY) <= 0;
    }

    @Override
    public List<Point> adjacents(Point current) {
        return List.of(
            (Point) current.endomap(this::addStep, Function.identity()),
            current.endomap(this::addStep, this::subtractStep),
            current.endomap(this::addStep, this::addStep),
            current.endomap(this::subtractStep, Function.identity()),
            current.endomap(this::subtractStep, this::subtractStep),
            current.endomap(this::subtractStep, this::addStep),
            current.endomap(Function.identity(), this::addStep),
            current.endomap(Function.identity(), this::subtractStep)
        ).filter(p -> this.withinX(p.first())).filter(p -> this.withinY(p.second()));
    }
}
