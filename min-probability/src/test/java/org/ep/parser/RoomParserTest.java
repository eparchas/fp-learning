package org.ep.parser;

import static org.ep.helper.Util.newBD;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.ep.model.Detector;
import org.ep.model.Room;
import org.javafp.parsecj.input.Input;
import org.junit.jupiter.api.Test;

import io.vavr.collection.List;
import io.vavr.control.Try;

public class RoomParserTest {
    @Test
    void simpleValid() {
        final Input<Character> roomDescription = Input.of("25.0\n" +
                "5\n" +
                "2.423929917008996 20.187139309438546\n" +
                "19.39788132776695 14.174570106439353\n" +
                "1.3175678970133191 10.019351994529405\n" +
                "1.0536920857525445 2.8936703202385115\n" +
                "16.739302303324447 15.87541372165791");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        final Room room = tryRoom.get();
        assertNotNull(room);
        assertEquals(newBD(25d), room.getLength());
        assertEquals(5, room.getDetectors().length());
        assertThat(room.getDetectors(), containsInAnyOrder(
                equalTo(Detector.of(2.423929917008996d, 20.187139309438546d)),
                equalTo(Detector.of(19.39788132776695d, 14.174570106439353d)),
                equalTo(Detector.of(1.3175678970133191d, 10.019351994529405d)),
                equalTo(Detector.of(1.0536920857525445d, 2.8936703202385115d)),
                equalTo(Detector.of(16.739302303324447d, 15.87541372165791d))));
    }

    @Test
    void validZeroDetectors() {
        List.of(Input.of("25.0\n" + "0\n"), Input.of("25.0\n" + "0"))
                .forEach(roomDescription -> {
                    final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
                    Room room = tryRoom.get();
                    assertNotNull(room);
                    assertEquals(newBD(25d), room.getLength());
                    assertTrue(room.getDetectors().isEmpty());
                });
    }

    @Test
    void invalidLength() {
        final Input<Character> roomDescription = Input.of("a25.0\n" +
                "5\n" +
                "2.423929917008996 20.187139309438546\n" +
                "19.39788132776695 14.174570106439353\n" +
                "1.3175678970133191 10.019351994529405\n" +
                "1.0536920857525445 2.8936703202385115\n" +
                "16.739302303324447 15.87541372165791");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        assertTrue(tryRoom.isFailure());
    }

    @Test
    void invalidCount() {
        final Input<Character> roomDescription = Input.of("25.0\n" +
                "a5\n" +
                "2.423929917008996 20.187139309438546\n" +
                "19.39788132776695 14.174570106439353\n" +
                "1.3175678970133191 10.019351994529405\n" +
                "1.0536920857525445 2.8936703202385115\n" +
                "16.739302303324447 15.87541372165791");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        assertTrue(tryRoom.isFailure());
    }

    @Test
    void invalidDetectorsCountMore() {
        final Input<Character> roomDescription = Input.of("25.0\n" +
                "1\n" +
                "2.423929917008996 20.187139309438546\n" +
                "19.39788132776695 14.174570106439353");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        assertTrue(tryRoom.isFailure());
    }

    @Test
    void invalidDetectorsCountLess() {
        final Input<Character> roomDescription = Input.of("25.0\n" +
                "3\n" +
                "2.423929917008996 20.187139309438546\n" +
                "19.39788132776695 14.174570106439353");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        assertTrue(tryRoom.isFailure());
    }

    @Test
    void additionalInputAtTheEnd() {
        final Input<Character> roomDescription = Input.of("25.0\n" +
                "2\n" +
                "2.423929917008996 20.187139309438546\n" +
                "19.39788132776695 14.174570106439353\n" +
                "Some extra content");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        assertTrue(tryRoom.isFailure());
    }

    @Test
    void invalidDetector() {
        final Input<Character> roomDescription = Input.of("25.0\n" +
                "1\n" +
                "19.39788132776695 14.174570106439353a");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        assertTrue(tryRoom.isFailure());
    }

    @Test
    void emptyInput() {
        final Input<Character> roomDescription = Input.of("");

        final Try<Room> tryRoom = RoomParser.parseRoom(roomDescription);
        assertTrue(tryRoom.isFailure());
    }
}
