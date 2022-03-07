package org.ep.model;

import io.vavr.collection.List;

public interface Grid {
    int size();

    List<Point> adjacents(Point p);
}
