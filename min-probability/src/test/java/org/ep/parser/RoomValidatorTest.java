package org.ep.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.ep.model.Detector;
import org.ep.model.Room;
import org.junit.jupiter.api.Test;

import io.vavr.collection.List;
import io.vavr.collection.Seq;
import io.vavr.control.Validation;

public class RoomValidatorTest {
    @Test
    void testInvalidLength() {
        Validation<Seq<String>, Room> roomV = RoomValidator.validateRoom(Room.of(-1d, List.empty()));
        assertTrue(roomV.isInvalid());
        assertTrue(roomV.getError().contains(RoomValidator.ROOM_LENGTH_ERROR));
        assertEquals(1, roomV.getError().length());
    }

    @Test
    void testInvalidDetectorPlacement() {
        Validation<Seq<String>, Room> roomV = RoomValidator.validateRoom(Room.of(10d, List.of(
                Detector.of(0d, 1d),
                Detector.of(10d, 10d),
                Detector.of(-1d, 10.5d))));
        assertTrue(roomV.isInvalid());
        assertTrue(roomV.getError().contains(RoomValidator.DETECTOR_PLACEMENT_ERROR));
        assertEquals(1, roomV.getError().length());
    }

    @Test
    void testValidRoom() {
        Room room = Room.of(10d, List.of(
                Detector.of(0d, 1d),
                Detector.of(10d, 10d)));
        Validation<Seq<String>, Room> roomV = RoomValidator.validateRoom(room);
        assertTrue(roomV.isValid());
        assertEquals(room, roomV.get());
    }
}
