package org.ep.model;

import com.pholser.junit.quickcheck.generator.GenerationStatus;
import com.pholser.junit.quickcheck.generator.Generator;
import com.pholser.junit.quickcheck.random.SourceOfRandomness;

import static org.ep.helper.Util.*;

import java.math.BigDecimal;

public class StaticGrids extends Generator<StaticGrid> {

    public StaticGrids() {
        super(StaticGrid.class);
    }

    @Override
    public StaticGrid generate(SourceOfRandomness random, GenerationStatus status) {
        final BigDecimal minX = newBD(random.nextDouble());
        final BigDecimal maxX = newBD(random.nextDouble(minX.doubleValue(), minX.doubleValue() + random.nextDouble()));

        final BigDecimal minY = newBD(random.nextDouble());
        final BigDecimal maxY = newBD(random.nextDouble(minY.doubleValue(), minY.doubleValue() + random.nextDouble()));

        final BigDecimal step = newBD(random.nextDouble(0d, Math.max(maxX.subtract(minX).doubleValue(), maxY.subtract(minY).doubleValue())));
        return new StaticGrid(step, minX, maxX, minY, maxY);
    }
    
}