package java.lang;

import java.io.PrintStream;

public final class System {
    public static PrintStream out = setStdout();

    private static native PrintStream setStdout();

    public static native void arraycopy(Object src, int srcPos, Object dest,
            int destPos, int length);

    private System() {
    }
}
