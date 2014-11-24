package com.seafile.seadroid2.data;

public class TwoTuple<X, Y> {
    private X x;
    private Y y;

    public TwoTuple(X x, Y y) {
        this.x = x;
        this.y = y;
    }

    public X getFirst() {
        return x;
    }

    public Y getSecond() {
        return y;
    }

    public static <U,V> TwoTuple<U, V> newInstance(U u, V v) {
        return new TwoTuple<U,V>(u, v);
    }
}
