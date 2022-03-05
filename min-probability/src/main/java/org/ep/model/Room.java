package org.ep.model;

import java.math.BigDecimal;

import org.javafp.data.IList;

import io.vavr.collection.List;
import static org.ep.helper.Util.*;

public class Room {
    private final BigDecimal length;
    private final List<Detector> detectors;

    public Room(BigDecimal length, List<Detector> detectors) {
        this.length = length;
        this.detectors = detectors;
    }

    public BigDecimal getLength() {
        return length;
    }

    public List<Detector> getDetectors() {
        return detectors;
    }

    public static Room of(Double length, IList<Detector> detectors) {
        return new Room(newBD(length), List.ofAll(detectors));
    }
}
