package org.ep.model;

import static org.ep.helper.Util.*;

import java.lang.annotation.Annotation;
import java.math.BigDecimal;

import com.pholser.junit.quickcheck.generator.GenerationStatus;
import com.pholser.junit.quickcheck.generator.Generator;
import com.pholser.junit.quickcheck.random.SourceOfRandomness;

import io.vavr.collection.Stream;

public class RoomGen extends Generator<Room> {
    public static GenerationStatus.Key<BigDecimal> LENGTH_KEY = new GenerationStatus.Key<>("length", BigDecimal.class);
    public RoomGen() {  
        super(Room.class);
    }

    @Override
    public Room generate(SourceOfRandomness random, GenerationStatus status) {
        final BigDecimal length = round(newBD(random.nextDouble(0d, 100_000d)), 15);
        final GenerationStatus newStatus = status.setValue(LENGTH_KEY, length);
        return new Room(length.setScale(SCALE, ROUNDING_MODE), 
            Stream.range(0, random.nextInt(0, 100))
                .map(_ignore -> gen().type(Detector.class).generate(random, newStatus))
                .toList()
        );
    }
}
