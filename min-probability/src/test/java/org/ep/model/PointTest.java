package org.ep.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigDecimal;

import org.junit.jupiter.api.Test;

public class PointTest {
    @Test
    void testEndoMap() {
        Point mapped = Point.of(1d, 2d).endomap(f -> f.add(BigDecimal.ONE), s -> s.add(BigDecimal.ONE));
        assertEquals(Point.of(2d, 3d), mapped);
    }
}
