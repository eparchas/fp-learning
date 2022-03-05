package org.ep;

import java.io.File;
import java.math.BigDecimal;

import org.ep.model.Room;
import org.ep.parser.RoomParser;

import io.vavr.control.Option;
import io.vavr.control.Try;

public class FindMinProbability {
    public static void main(String[] args) {
        extractFileNameFromArgs(args)
                .map(FindMinProbability::toFile).flatMap(Option::toTry)
                .flatMap(RoomParser::parseRoom)
                .map(FindMinProbability::traverse)
                .map(FindMinProbability::round)
                .andThen(prob -> System.out.println(prob))
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

    private static Option<File> toFile(String fileName) {
        if (fileName != null && !fileName.isEmpty()) {
            final File file = new File(fileName);
            if (file.exists() && file.canRead() && !file.isHidden()) {
                return Option.of(file);
            }
        }
        return Option.none();
    }

    public static BigDecimal traverse(Room room) {
        return BigDecimal.ONE;
    }

    private static BigDecimal round(BigDecimal toRound) {
        return BigDecimal.ZERO;
    }
}