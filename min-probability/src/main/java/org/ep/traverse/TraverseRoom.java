package org.ep.traverse;

import static org.ep.helper.Util.ROUNDING_MODE;
import static org.ep.helper.Util.SCALE;
import static org.ep.helper.Util.newBD;

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

/**
 * Provides methods to allow traversal of a room with minimum probability of detection
 */
public abstract class TraverseRoom {
    private TraverseRoom() {
        throw new RuntimeException("Not meant to be instantiated");
    }

    /**
     * Traverse the room keeping the probability of detection at a minimum
     * @param room to traverse
     * @return the minimum probability of detection for that room
     */
    public static BigDecimal traverseUndetected(Room room) {
        final BigDecimal middle = room.getLength().divide(newBD(2d), MathContext.DECIMAL64)
                .setScale(SCALE, ROUNDING_MODE);
        return traverseUndetected(room,
                new Point(middle, newBD(0d)),
                new Point(middle, room.getLength()));
    }

    /**
     * Traverse the room from start to end, keeping the probability of detection at a minimum
     * @param room to traverse
     * @param start starting point
     * @param end ending point
     * @return the minimum probability of detection if traversing the room from start to end
     */
    public static BigDecimal traverseUndetected(Room room, Point start, Point end) {
        Function3<BigDecimal, Seq<Detector>, Point, BigDecimal> probabilityFunction = TraverseRoom::allDetectorsProbability;
        Function1<Point, BigDecimal> memoizedProbability = probabilityFunction
                .apply(room.getLength(), room.getDetectors()).memoized();
        return traverseMinProbability(formGrid(room), memoizedProbability, start, end);
    }

    /**
     * Traverse the grid keeping the probability of detection at a minimum
     * @param grid to traverse
     * @param probability function to give the probability of detection for any given point
     * @param start the starting point in the grid
     * @param end the ending point in the grid
     * @return the minimum probability of detection if traversing the room from start to end
     */
    public static BigDecimal traverseMinProbability(Grid grid, Function1<Point, BigDecimal> probability, Point start,
            Point end) {
        Stream<Tuple2<Point, BigDecimal>> points = TraversalState.init(start, grid::adjacents, probability).unfold();
        return points
                .filter(t -> t._1().equals(end))
                .headOption()
                .map(Tuple2::_2)
                .getOrElse(BigDecimal.ONE.negate());
    }

    /**
     * Create a grid from a room
     * @param room to generate a grid for
     * @return some form of a grid (StaticGrid in this case)
     */
    public static Grid formGrid(Room room) {
        BigDecimal step = determineStep(room);
        return new StaticGrid(step, newBD(0d), room.getLength(), newBD(0d), room.getLength());
    }

    /**
     * Simple heuristic for determining the step based on the number of detectors in
     * the room
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

    /**
     * The probability of detection from a single detector, as given by the
     * definition of the exponential function
     * 
     * @param length   of the room
     * @param detector for which probability should be calculated
     * @param position in the room
     * @return the probability of the of detection for the given detector and point
     */
    public static BigDecimal singleDetectorProbability(BigDecimal length, Detector detector, Point position) {
        BigDecimal distance = position.first().subtract(detector.first()).pow(2)
                .add(position.second().subtract(detector.second()).pow(2)).sqrt(MathContext.DECIMAL64);
        return newBD(Math.exp(
                newBD(Math.PI).multiply(distance).divide(length, MathContext.DECIMAL64).pow(2).negate().doubleValue()));
    }

    /**
     * The only case not to be detected is not to be detected by any sensor.
     * The probability of not being detected by any sensor is the product of
     * 1 minus the detection probability of each sensor. The total probability
     * of detection is thus 1 - P(Non detection by all sensors)
     * 
     * @param length    of the room
     * @param detectors in the room
     * @param position  the point to calculate probability for
     * @return the probability of detection for the given point
     */
    public static BigDecimal allDetectorsProbability(BigDecimal length, Seq<Detector> detectors, Point position) {
        final Function3<BigDecimal, Detector, Point, BigDecimal> singleDetectorProb = TraverseRoom::singleDetectorProbability;

        final Seq<BigDecimal> perDetector = detectors.map(singleDetectorProb.apply(length)::apply)
                .map(f -> f.apply(position));

        return newBD(1d).subtract(perDetector.map(p -> newBD(1d).subtract(p))
                .fold(newBD(1d), (a, b) -> a.multiply(b)));
    }
}
