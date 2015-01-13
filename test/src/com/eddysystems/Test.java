package eddysystems;

public class Test {

    int x;
    int y;
    public static void f(int x, double b) {}

    private <T> T fun(T y) {
        return y;
    }

    void x() {
        int a = 1;
        double x = 0;
        f a x
        f(a,x)
    }
}
