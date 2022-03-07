package org.ep;

import org.ep.helper.Util;
import org.ep.parser.RoomParser;
import org.ep.traverse.TraverseRoom;

import io.vavr.control.Option;
import io.vavr.control.Try;

public class FindMinProbability {
    public static void main(String[] args) {
        extractFileNameFromArgs(args)
                .map(Util::toValidFile).flatMap(Option::toTry)
                .flatMap(RoomParser::parseRoom)
                .map(TraverseRoom::traverseUndetected)
                .map(Util::round)
                .andThen(prob -> System.out.println(prob)) // method reference didn't work here
                .onFailure((_ex) -> {
                    System.err.println(_ex.getMessage());
                    System.exit(-1);
                });
    }

    private static Try<String> extractFileNameFromArgs(String[] args) {
        return Try.of(() -> {
            if (args.length != 1)
                throw new RuntimeException("Invalid number of arguments");
            if (args[0].isEmpty())
                throw new RuntimeException("Invalid file name");
            return args[0];
        });
    }
}