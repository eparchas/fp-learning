package org.ep.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigDecimal;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;

import io.vavr.collection.List;

public class StaticGridTest {
    private Grid grid = new StaticGrid(BigDecimal.ONE, BigDecimal.ZERO, BigDecimal.TEN, BigDecimal.ZERO, BigDecimal.TEN);

    @Test
    void testAdjacentsAtGridStart() {
        List<Point> adjacents = grid.adjacents(Point.of(0d, 0d));
        assertEquals(3, adjacents.length());
        assertThat(adjacents, containsInAnyOrder(
            equalTo(Point.of(0d, 1d)),
            equalTo(Point.of(1d, 1d)),
            equalTo(Point.of(1d, 0d))
        ));
    }

    @Test
    void testAdjacentsAtGridEnd() {
        List<Point> adjacents = grid.adjacents(Point.of(10d, 10d));
        assertEquals(3, adjacents.length());
        assertThat(adjacents, containsInAnyOrder(
            equalTo(Point.of(9d, 10d)),
            equalTo(Point.of(9d, 9d)),
            equalTo(Point.of(10d, 9d))
        ));
    }

    @Test
    void testSize() {
        assertEquals(100, grid.size());
    }
}
