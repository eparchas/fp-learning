package org.ep.model;

import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;

import io.vavr.Tuple;
import io.vavr.Tuple2;

public abstract class Pair<F, S> {
    private final Tuple2<F, S> delegate;

    public Pair(F first, S second) {
        this.delegate = Tuple.of(first, second);
    }

    protected Pair(Tuple2<F, S> delegate) {
        this.delegate = delegate;
    }
    
    public F first() {
        return delegate._1();
    }

    public S second() {
        return delegate._2();
    }

    protected abstract <P extends Pair<F, S>> P ctor(Tuple2<F, S> delegate);

    public <P extends Pair<F, S>> P endomap(BiFunction<? super F, ? super S, Tuple2<F, S>> mapper) {
        return this.ctor(this.delegate.map(mapper));
    }

    public <P extends Pair<F, S>> P endomap(Function<? super F, ? extends F> f1, Function<? super S, ? extends S> f2) {
        return this.ctor(this.delegate.map(f1, f2));
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
