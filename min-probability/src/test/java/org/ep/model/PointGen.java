package org.ep.model;

import java.math.BigDecimal;

import com.pholser.junit.quickcheck.generator.GenerationStatus;
import com.pholser.junit.quickcheck.generator.Generator;
import com.pholser.junit.quickcheck.random.SourceOfRandomness;

public class PointGen extends Generator<Point> {
    private WithinGrid withinGrid;

    public PointGen() {
        super(Point.class);
    }

    @Override
    public Point generate(SourceOfRandomness random, GenerationStatus status) {
        Double maxX = 100_000d, maxY = 100_000d;
        if (status.valueOf(RoomGen.LENGTH_KEY).isPresent()) {
            BigDecimal length = status.valueOf(RoomGen.LENGTH_KEY).get();
            maxX = maxY = length.doubleValue();
        } else if (this.withinGrid != null) {
            maxX = this.withinGrid.x();
            maxY = this.withinGrid.y();
        }
        return Point.of(random.nextDouble(0d, maxX), random.nextDouble(0d, maxY));
    }

    public void configure(WithinGrid withinGrid) {
        this.withinGrid = withinGrid;
    }
}