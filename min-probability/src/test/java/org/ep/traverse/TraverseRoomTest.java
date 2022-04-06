package org.ep.traverse;

import static org.ep.helper.Util.newBD;
import static org.junit.Assert.assertEquals;

import org.ep.model.Detector;
import org.ep.model.Room;
import org.junit.jupiter.api.Test;

import io.vavr.collection.List;

public class TraverseRoomTest {
    @Test
    void testTraverseNoDetectors() {
        assertEquals(newBD(0d), TraverseRoom.traverseUndetected(Room.of(100_000_000d, List.empty())));
    }

    @Test
    void testDetermineStep() {
        assertEquals(newBD(0.5d), TraverseRoom.determineStep(Room.of(1.0d, List.empty())));
        assertEquals(newBD(0.5d), TraverseRoom.determineStep(Room.of(1.0d, List.of(Detector.of(0.5d, 0.5d)))));
    }
}
