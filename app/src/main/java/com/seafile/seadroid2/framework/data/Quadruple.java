package com.seafile.seadroid2.framework.data;

import android.util.Pair;

import androidx.annotation.Nullable;

import java.util.Objects;

/**
 * If you're going to use the {@link Pair} class, you can use this as well.
 */
public class Quadruple<FI, SE, TH, FO> {
    public final FI first;
    public final SE second;
    public final TH third;
    public final FO fourth;


    /**
     * Constructor for a Pair.
     *
     * @param first  the first object in the Pair
     * @param second the second object in the pair
     */
    public Quadruple(FI first, SE second, TH third, FO fourth) {
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
    }

    @Override
    public boolean equals(@Nullable Object o) {
        if (!(o instanceof Pair)) {
            return false;
        }
        Quadruple<?, ?, ?, ?> p = (Quadruple<?, ?, ?, ?>) o;
        return Objects.equals(p.first, first)
                && Objects.equals(p.second, second)
                && Objects.equals(p.third, third)
                && Objects.equals(p.fourth, fourth);
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second, third, fourth);
    }

    @Override
    public String toString() {
        return "Quadruple{" + String.valueOf(first)
                + " " + String.valueOf(second)
                + " " + String.valueOf(third)
                + " " + String.valueOf(fourth) + "}";
    }

    public static <A, B, C, D> Quadruple<A, B, C, D> create(A a, B b, C c, D d) {
        return new Quadruple<>(a, b, c, d);
    }
}
