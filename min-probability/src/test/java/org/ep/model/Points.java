package org.ep.model;

import com.pholser.junit.quickcheck.generator.GenerationStatus;
import com.pholser.junit.quickcheck.generator.Generator;
import com.pholser.junit.quickcheck.random.SourceOfRandomness;

public class Points extends Generator<Point> {

    public Points() {
        super(Point.class);
    }

    @Override
    public Point generate(SourceOfRandomness random, GenerationStatus status) {
        return Point.of(random.nextDouble(), random.nextDouble());
    }
    
}