package org.ep.model;

import java.math.BigDecimal;
import java.util.Objects;

import org.javafp.data.IList;

import io.vavr.collection.List;
import io.vavr.collection.Seq;

import static org.ep.helper.Util.*;

public class Room {
    private final BigDecimal length;
    private final Seq<Detector> detectors;

    public Room(BigDecimal length, Seq<Detector> detectors) {
        this.length = length;
        this.detectors = detectors;
    }

    public BigDecimal getLength() {
        return length;
    }

    public Seq<Detector> getDetectors() {
        return detectors;
    }

    public static Room of(Double length, IList<Detector> detectors) {
        return new Room(newBD(length), List.ofAll(detectors));
    }

    public static Room of(Double length, Seq<Detector> detectors) {
        return new Room(newBD(length), detectors);
    }


    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Room)) {
            return false;
        }
        Room room = (Room) o;
        return Objects.equals(length, room.length) && Objects.equals(detectors, room.detectors);
    }

    @Override
    public int hashCode() {
        return Objects.hash(length, detectors);
    }

}
