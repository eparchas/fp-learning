package org.ep.model;

import java.util.Objects;

import io.vavr.Tuple;
import io.vavr.Tuple2;

public class Pair<F, S> {
    private final Tuple2<F, S> delegate;

    public Pair(F first, S second) {
        this.delegate = Tuple.of(first, second);
    }

    public F first() {
        return delegate._1();
    }

    public S second() {
        return delegate._2();
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Pair)) {
            return false;
        }
        Pair pair = (Pair) o;
        return Objects.equals(delegate, pair.delegate);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(delegate);
    }



    @Override
    public String toString() {
        return delegate.toString();
    }

}
