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

public final class StreamState {
    private final Set<Point> visited;
    private final PriorityQueue<Tuple2<Point, BigDecimal>> minHeap;
    private final Map<Point, BigDecimal> probabilityIdx;
    private final Function1<Point, List<Point>> adjacents;
    private final Function1<Point, BigDecimal> probability;

    private StreamState(Set<Point> visited, PriorityQueue<Tuple2<Point, BigDecimal>> minHeap,
            Map<Point, BigDecimal> probabilityIdx, Function1<Point, List<Point>> adjacents,
            Function1<Point, BigDecimal> probability) {
        this.visited = visited;
        this.minHeap = minHeap;
        this.probabilityIdx = probabilityIdx;
        this.adjacents = adjacents;
        this.probability = probability;
    }

    public static StreamState init(Point startPoint, Function1<Point, List<Point>> adjacents,
            Function1<Point, BigDecimal> probability) {
        return new StreamState(
                HashSet.<Point>empty(),
                PriorityQueue.of((a, b) -> a._2().compareTo(b._2()),
                        Tuple.of(startPoint, probability.apply(startPoint))),
                HashMap.of(startPoint, probability.apply(startPoint)),
                adjacents,
                probability);
    }

    public Stream<Tuple2<Point, BigDecimal>> unfold() {
        return Stream.unfoldRight(this, ss -> {
            Option<Tuple2<Tuple2<Point, BigDecimal>, PriorityQueue<Tuple2<Point, BigDecimal>>>> dequeueOpt = ss.minHeap
                    .dequeueOption();
            if (dequeueOpt.isDefined()) {
                Tuple2<Tuple2<Point, BigDecimal>, PriorityQueue<Tuple2<Point, BigDecimal>>> headAndTail = dequeueOpt
                        .get();
                Tuple2<Point, BigDecimal> head = headAndTail._1;
                PriorityQueue<Tuple2<Point, BigDecimal>> tail = headAndTail._2;
                Point current = head._1();
                BigDecimal currentProb = head._2();
                if (visited.contains(current)) {
                    return Option
                            .some(Tuple.of(head,
                                    new StreamState(ss.visited, tail, ss.probabilityIdx, adjacents, probability)));
                }
                Set<Point> nextVisited = ss.visited.add(current);

                Map<Point, BigDecimal> adjacentsMap = HashMap
                        .ofEntries(adjacents.apply(current).filter(Predicate.not(nextVisited::contains))
                                .map(point -> Tuple.of(point, currentProb.max(ss.probability.apply(point)))));

                Map<Point, BigDecimal> nextProbabilityIdx = ss.probabilityIdx.merge(adjacentsMap, BigDecimal::min);

                PriorityQueue<Tuple2<Point, BigDecimal>> nextMinHeap = tail.enqueueAll(
                        adjacentsMap.map(e -> Tuple.of(e._1(), nextProbabilityIdx.getOrElse(e._1(), newBD(1d)))));

                return Option
                        .some(Tuple.of(head,
                                new StreamState(nextVisited, nextMinHeap, nextProbabilityIdx, adjacents, probability)));
            }
            return Option.none();
        });
    }
}