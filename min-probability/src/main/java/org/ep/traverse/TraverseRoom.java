package org.ep.traverse;

import java.math.BigDecimal;

import org.ep.model.Room;

public abstract class TraverseRoom {
    private TraverseRoom() {
        throw new RuntimeException("Not meant to be instantiated");
    }

    public static BigDecimal traverseUndetected(Room room) {
        return BigDecimal.ONE;
    }
    

}
