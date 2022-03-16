package org.ep.traverse;

import static org.ep.helper.Util.*;

import java.math.BigDecimal;
import java.math.MathContext;

import org.ep.model.Detector;
import org.ep.model.Grid;
import org.ep.model.Point;
import org.ep.model.Room;
import org.ep.model.StaticGrid;

import io.vavr.Function1;
import io.vavr.Function3;
import io.vavr.Tuple2;
import io.vavr.collection.Seq;
import io.vavr.collection.Stream;

public abstract class TraverseRoom {
    private TraverseRoom() {
        throw new RuntimeException("Not meant to be instantiated");
    }

    public static BigDecimal traverseUndetected(Room room) {
        final BigDecimal middle = room.getLength().divide(newBD(2d), MathContext.DECIMAL64)
                .setScale(SCALE, ROUNDING_MODE);
        return traverseUndetected(room,
                new Point(middle, newBD(0d)),
                new Point(middle, room.getLength()));
    }

    public static BigDecimal traverseUndetected(Room room, Point start, Point end) {
        Function3<BigDecimal, Seq<Detector>, Point, BigDecimal> probabilityFunction = TraverseRoom::allDetectorsProbability;
        Function1<Point, BigDecimal> memoizedProbability = probabilityFunction
                .apply(room.getLength(), room.getDetectors()).memoized();
        return traverseMinProbability(formGrid(room), memoizedProbability, start, end);
    }

    public static BigDecimal traverseMinProbability(Grid grid, Function1<Point, BigDecimal> probability, Point start,
            Point end) {
                // System.out.println(end);
        Stream<Tuple2<Point, BigDecimal>> points = StreamState.init(start, grid::adjacents, probability).unfold();
        return points.map(p -> {
            // System.out.println(p);
            return p;
        }).filter(t -> t._1().equals(end)).headOption().map(Tuple2::_2).getOrElse(BigDecimal.ONE.negate());
    }

    public static Grid formGrid(Room room) {
        BigDecimal step = determineStep(room);
        return new StaticGrid(step, newBD(0d), room.getLength(), newBD(0d), room.getLength());
    }

    /**
     * Simple heuristic for determining the step based on
     * 
     * @param room
     * @return the step for the static grid
     */
    public static BigDecimal determineStep(Room room) {
        return room.getLength().divide(
                room.getDetectors().length() == 0 ? newBD(2d)
                        : newBD((double) (room.getDetectors().length() * 2)),
                MathContext.DECIMAL64)
                .setScale(SCALE, ROUNDING_MODE);
    }

    public static BigDecimal singleDetectorProbability(BigDecimal length, Detector detector, Point position) {
        BigDecimal distance = position.first().subtract(detector.first()).pow(2)
                .add(position.second().subtract(detector.second()).pow(2)).sqrt(MathContext.DECIMAL64);
        return newBD(Math.exp(
                newBD(Math.PI).multiply(distance).divide(length, MathContext.DECIMAL64).pow(2).negate().doubleValue()));
    }

    public static BigDecimal allDetectorsProbability(BigDecimal length, Seq<Detector> detectors, Point position) {
        final Function3<BigDecimal, Detector, Point, BigDecimal> singleDetectorProb = TraverseRoom::singleDetectorProbability;

        final Seq<BigDecimal> perDetector = detectors.map(singleDetectorProb.apply(length)::apply)
                .map(f -> f.apply(position));

        // The only case not to be detected is not to be detected by any sensor.
        // The probability of not being detected by any sensor is the product of 
        // 1 minus the detection probability of each sensor. The total probability
        // of detection is thus 1 - P(Non detection by all sensors)
        return newBD(1d).subtract(perDetector.map(p -> newBD(1d).subtract(p))
            .fold(newBD(1d), (a, b) -> a.multiply(b)));
    }
}
