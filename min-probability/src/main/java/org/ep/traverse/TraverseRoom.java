package org.ep.traverse;

import java.math.BigDecimal;

import org.ep.model.Grid;
import org.ep.model.Room;

public abstract class TraverseRoom {
    private TraverseRoom() {
        throw new RuntimeException("Not meant to be instantiated");
    }

    public static BigDecimal traverseUndetected(Room room) {
        // Create Grid from room
        // Find starting point
        // Find ending point
        // Move from starting point to next least probability point, performing DFS
        return BigDecimal.ONE;
    }
    
    public static Grid formGrid(Room room) {
        return null;
    }

}
