package org.ep.model;

import static org.junit.Assert.assertTrue;

import com.pholser.junit.quickcheck.Property;
import com.pholser.junit.quickcheck.runner.JUnitQuickcheck;

import org.junit.runner.RunWith;

@RunWith(JUnitQuickcheck.class)
public class StaticGridProperties {
    @Property(shrink = true)
    public void adjacentsAreAlwaysWithinTheGrid(StaticGrid grid, Point point) {
        assertTrue(grid.adjacents(point).filter(p -> 
            p.first().compareTo(grid.minX) < 0 || p.first().compareTo(grid.maxX) > 0 ||
            p.second().compareTo(grid.minY) < 0 || p.second().compareTo(grid.maxY) > 0
        ).isEmpty());
    }
}