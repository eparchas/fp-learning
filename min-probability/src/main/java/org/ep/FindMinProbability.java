package org.ep;

import java.io.File;
import java.math.BigDecimal;

import org.ep.helper.Util;
import org.ep.model.Room;
import org.ep.parser.RoomParser;

import io.vavr.Predicates;
import io.vavr.control.Option;
import io.vavr.control.Try;

public class FindMinProbability {
    public static void main(String[] args) {
        extractFileNameFromArgs(args)
                .map(FindMinProbability::toValidFile).flatMap(Option::toTry)
                .flatMap(RoomParser::parseRoom)
                .map(FindMinProbability::traverse)
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

    private static Option<File> toValidFile(String fileName) {
        return Option.of(fileName)
                .filter(Predicates.not(String::isBlank))
                .map(File::new)
                .filter(File::exists)
                .filter(File::canRead)
                .filter(Predicates.not(File::isHidden));
    }

    public static BigDecimal traverse(Room room) {
        return BigDecimal.ONE;
    }
}