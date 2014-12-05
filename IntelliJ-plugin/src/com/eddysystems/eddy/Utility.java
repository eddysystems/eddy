package com.eddysystems.eddy;

import java.util.concurrent.Callable;

public class Utility {
  public static interface Timed<T> {
    public abstract T call();
  }

  public static <T> T timed(String name, Timed<T> f) {
    long start = System.nanoTime();
    T x = f.call();
    long end = System.nanoTime();
    System.out.println("elapsed "+name+" = "+(end-start)/1e9);
    return x;
  }
}
