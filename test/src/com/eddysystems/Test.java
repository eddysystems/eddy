package eddysystems;

public class Test {

    class Q {}

    int x;
    int y;
    public static void blubb(int x, double b) {}

    private <T> T fun(Object y) {
        return (T)y;
    }

    void x() {
        int a = 1;
        double x = 0;
        f(a, x);
        f(a);
        Q q = fun(new Object());
    }
}
