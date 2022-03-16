package org.ep.model;

import java.math.BigDecimal;
import java.util.Optional;

import com.pholser.junit.quickcheck.generator.GenerationStatus;
import com.pholser.junit.quickcheck.generator.Generator;
import com.pholser.junit.quickcheck.random.SourceOfRandomness;

public class DetectorGen extends Generator<Detector> {
    private WithinGrid withinGrid;

	public DetectorGen() {
        super(Detector.class);
    }

	@Override
	public Detector generate(SourceOfRandomness random, GenerationStatus status) {
        Double maxX = 100_000d, maxY = 100_000d;
        if (status.valueOf(RoomGen.LENGTH_KEY).isPresent()) {
            BigDecimal length = status.valueOf(RoomGen.LENGTH_KEY).get();
            maxX = maxY = length.doubleValue();
        } else if (this.withinGrid != null) {
            maxX = this.withinGrid.x();
            maxY = this.withinGrid.y();
        }
        return Detector.of(random.nextDouble(0d, maxX), random.nextDouble(0d, maxY));
	}
    
    public void configure(WithinGrid withinGrid) {
        this.withinGrid = withinGrid;
    }
}
