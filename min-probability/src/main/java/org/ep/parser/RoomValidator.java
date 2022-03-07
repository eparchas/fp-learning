package org.ep.parser;

import java.math.BigDecimal;

import org.ep.model.Detector;
import org.ep.model.Room;

import io.vavr.Function1;
import io.vavr.Function2;
import io.vavr.collection.List;
import io.vavr.collection.Seq;
import io.vavr.control.Validation;

public abstract class RoomValidator {
    public static final String ROOM_LENGTH_ERROR = "Room length must be positive";
    public static final String DETECTOR_PLACEMENT_ERROR = "Detector should be placed within the room";

    private RoomValidator() {
        throw new RuntimeException("Not meant to be instantiated");
    }

    public static Validation<Seq<String>, Room> validateRoom(Room room) {
        Validation<Seq<String>, BigDecimal> lengthV = validateLength(room.getLength()).mapError(List::of);
        Validation<Seq<String>, Seq<Detector>> detectorsV = validateDetectors(room.getLength(), room.getDetectors());
        return Validation.combine(
                lengthV, 
                detectorsV
        ).ap(Room::new).mapError(s -> s.flatMap(is -> is));
    }

    public static Validation<String, BigDecimal> validateLength(BigDecimal length) {
        return BigDecimal.ZERO.compareTo(length) < 0 ? Validation.valid(length)
                : Validation.invalid(ROOM_LENGTH_ERROR);
    }

    public static Validation<Seq<String>, Seq<Detector>> validateDetectors(BigDecimal length, Seq<Detector> detectors) {
        Function2<BigDecimal, Detector, Validation<String, Detector>> validateDetectorByLength = RoomValidator::validateDetector;
        Function1<Detector, Validation<String, Detector>> validateDetector = validateDetectorByLength.apply(length);
        return Validation.sequence(detectors.map(validateDetector).map(v -> v.mapError(List::of)));
        
    }

    public static Validation<String, Detector> validateDetector(BigDecimal length, Detector detector) {
        return (detector.first().compareTo(BigDecimal.ZERO) >= 0 && detector.first().compareTo(length) <= 0 &&
                detector.second().compareTo(BigDecimal.ZERO) >= 0 && detector.second().compareTo(length) <= 0)
                        ? Validation.valid(detector)
                        : Validation.invalid(DETECTOR_PLACEMENT_ERROR);
    }
}
