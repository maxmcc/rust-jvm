package java.lang;

import java.io.PrintStream;

import moon.RustStdout;

public final class System {
    public static PrintStream out = new PrintStream(new RustStdout());

    private static native PrintStream setStdout();

    public static native void arraycopy(Object src, int srcPos, Object dest,
            int destPos, int length);

    private System() {
    }
}
