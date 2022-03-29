package org.ep.traverse;

import static org.ep.helper.Util.newBD;

import java.math.BigDecimal;
import java.util.function.Predicate;

import org.ep.model.Point;

import io.vavr.Function1;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.HashMap;
import io.vavr.collection.HashSet;
import io.vavr.collection.List;
import io.vavr.collection.Map;
import io.vavr.collection.PriorityQueue;
import io.vavr.collection.Set;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

/**
 * An immutable class that represents the traversal state of a room / grid and
 * allows unfolding towards adjacent points following the least probability of
 * detection path
 */
public final class TraversalState {
    private final Set<Point> visited;
    private final PriorityQueue<Tuple2<Point, BigDecimal>> minHeap;
    private final Map<Point, BigDecimal> probabilityIdx;

    private TraversalState(Set<Point> visited, PriorityQueue<Tuple2<Point, BigDecimal>> minHeap,
            Map<Point, BigDecimal> probabilityIdx) {
        this.visited = visited;
        this.minHeap = minHeap;
        this.probabilityIdx = probabilityIdx;
    }

    /**
     * Create a state for a specific point
     * 
     * @param startPoint  the starting point of this state
     * @param adjacents   a function to generate adjacent points to a given point
     * @param probability a function to calculate the detection probability for a
     *                    given point
     * @return a stream state that can be unfolded
     */
    private static TraversalState init(Point startPoint, BigDecimal probability) {
        return new TraversalState(
                HashSet.<Point>empty(),
                PriorityQueue.of((a, b) -> a._2().compareTo(b._2()),
                        Tuple.of(startPoint, probability)),
                HashMap.of(startPoint, probability));
    }

    public static Stream<Tuple2<Point, BigDecimal>> unfold(final Point start, final Function1<Point, List<Point>> adjacents, final Function1<Point, BigDecimal> probability) {
        return unfold(TraversalState.init(start, probability.apply(start)), adjacents, probability);
    }

    /**
     * Unfold the initial state into a stream of points and probabilities
     * 
     * @return a Stream of points and their corresponding minimum probabilities of
     *         detection
     */
    private static Stream<Tuple2<Point, BigDecimal>> unfold(final TraversalState initial,
            final Function1<Point, List<Point>> adjacents, final Function1<Point, BigDecimal> probability) {
        return Stream.unfoldRight(initial, ss -> {
            Option<Tuple2<Tuple2<Point, BigDecimal>, PriorityQueue<Tuple2<Point, BigDecimal>>>> dequeueOpt = ss.minHeap
                    .dequeueOption();
            if (dequeueOpt.isDefined()) {
                Tuple2<Tuple2<Point, BigDecimal>, PriorityQueue<Tuple2<Point, BigDecimal>>> headAndTail = dequeueOpt.get();
                Tuple2<Point, BigDecimal> head = headAndTail._1;
                PriorityQueue<Tuple2<Point, BigDecimal>> tail = headAndTail._2;
                Point current = head._1();
                BigDecimal currentProb = head._2();
                if (ss.visited.contains(current)) {
                    return Option.some(Tuple.of(head, new TraversalState(ss.visited, tail, ss.probabilityIdx)));
                }
                Set<Point> nextVisited = ss.visited.add(current);

                Map<Point, BigDecimal> adjacentsMap = HashMap.ofEntries(
                    adjacents
                        .apply(current)
                        .filter(Predicate.not(nextVisited::contains))
                        .map(point -> Tuple.of(point, currentProb
                                        .max(probability.apply(point)))));

                Map<Point, BigDecimal> nextProbabilityIdx = ss.probabilityIdx.merge(adjacentsMap,
                        BigDecimal::min);

                PriorityQueue<Tuple2<Point, BigDecimal>> nextMinHeap = tail.enqueueAll(
                        adjacentsMap.map(e -> Tuple.of(e._1(),
                                nextProbabilityIdx.getOrElse(e._1(), newBD(1d)))));

                return Option
                        .some(Tuple.of(head,
                                new TraversalState(nextVisited, nextMinHeap,
                                        nextProbabilityIdx)));
            }
            return Option.none();
        });
    }
}