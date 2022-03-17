package org.ep.parser;

import static org.javafp.parsecj.Combinators.count;
import static org.javafp.parsecj.Combinators.eof;
import static org.javafp.parsecj.Combinators.fail;
import static org.javafp.parsecj.Combinators.or;
import static org.javafp.parsecj.Combinators.retn;
import static org.javafp.parsecj.Text.dble;
import static org.javafp.parsecj.Text.intr;
import static org.javafp.parsecj.Text.space;
import static org.javafp.parsecj.Text.string;

import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.stream.Collectors;

import org.ep.model.Detector;
import org.ep.model.Room;
import org.javafp.data.IList;
import org.javafp.parsecj.Parser;
import org.javafp.parsecj.input.Input;

import io.vavr.control.Try;

/**
 * Provides methods to parse a room file to a room
 */
public abstract class RoomParser {
    public static final Parser<Character, String> EOL = string(System.lineSeparator());

    private RoomParser() {
        throw new RuntimeException("Not meant to be instantiated");
    }

    /**
     * TODO: This is dangerous as it loads all the file in memory. Should provide an
     * alternative implementation
     * of Input that buffers internally and starts again in case the parser needs to
     * backtrack.
     */
    public static Try<Room> parseRoom(File roomFile) {
        return Try.of(() -> {
            List<String> lines = Files.readAllLines(roomFile.toPath());
            return Input.of(lines.stream().collect(Collectors.joining(System.lineSeparator())));
        }).flatMap(RoomParser::parseRoom);
    }

    public static Try<Room> parseRoom(Input<Character> roomDescription) {
        Parser<Character, Double> roomLength = dble.bind(length -> EOL.then(retn(length))).label("room length");
        Parser<Character, Integer> detectorCnt = intr.bind(cnt -> or(EOL, eof()).then(retn(cnt)))
                .label("detector count");

        Parser<Character, Detector> detector = dble.bind(x -> space.then(dble).bind(y -> or(EOL, eof()).then(
                retn(createDetector(x, y))))).label("detector position");

        Parser<Character, Room> room = roomLength.bind(
                length -> detectorCnt.bind(cnt -> count(detector, cnt).bind(list -> retn(createRoom(length, list)))))
                .label("detectors");

        Parser<Character, Room> failRoom = fail();

        return Try.of(() -> room.parse(roomDescription)
                .match(
                        ok -> ok.rest.end() ? ok : failRoom.parse(ok.rest),
                        error -> error) // Identity didn't work here
                .getResult());

    }

    private static Detector createDetector(Double x, Double y) {
        return Detector.of(x, y);
    }

    private static Room createRoom(Double length, IList<Detector> detectors) {
        return Room.of(length, detectors);
    }

}
