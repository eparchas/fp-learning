package org.ep.traverse;

import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeThat;
import static org.hamcrest.Matchers.greaterThan;

import java.math.BigDecimal;

import com.pholser.junit.quickcheck.Property;
import com.pholser.junit.quickcheck.runner.JUnitQuickcheck;

import org.ep.model.Detector;
import org.ep.model.Point;
import org.ep.model.Room;
import org.junit.runner.RunWith;

import io.vavr.Predicates;
import io.vavr.collection.Seq;

@RunWith(JUnitQuickcheck.class)
public class TraverseRoomProperties {
    @Property
    public void probabilityShouldAlwaysBeBetween0And1(Room room) {
        BigDecimal minProb = TraverseRoom.traverseUndetected(room);
        assertTrue("" + minProb, BigDecimal.ZERO.compareTo(minProb) <= 0 && minProb.compareTo(BigDecimal.ONE) <= 0);
    }

    @Property(trials = 10)
    public void probabilityOfDetectionShouldGoUpOrStaySameWhenAddingADetector(Room room) {
        assumeThat(room.getDetectors().length(), greaterThan(1));
        Seq<Detector> shortDetectors = room.getDetectors().removeLast(Predicates.isNotNull());
        final BigDecimal minProb = TraverseRoom.traverseUndetected(new Room(room.getLength(), shortDetectors));
        final BigDecimal augmentedMinProb = TraverseRoom.traverseUndetected(room);
        assertTrue(minProb + " <= " + augmentedMinProb, minProb.compareTo(augmentedMinProb) <= 0);
    }

    @Property
    public void probabilityForEachPointShouldBeBetween0And1(Room room, Point point) {
        BigDecimal pointProbability = TraverseRoom.allDetectorsProbability(room.getLength(), room.getDetectors(), point);
        assertTrue("" + pointProbability, BigDecimal.ZERO.compareTo(pointProbability) <= 0 && pointProbability.compareTo(BigDecimal.ONE) <= 0);
    }
}
