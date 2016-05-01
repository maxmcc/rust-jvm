package moon;

import java.io.IOException;
import java.io.OutputStream;

public final class RustStdout extends OutputStream {
    public RustStdout() {
    }

    @Override
    public void write(int b) throws IOException {
        byte[] arr = new byte[1];
        arr[0] = (byte) b;
        write(arr);
    }

    @Override
    public native void write(byte[] b, int off, int len) throws IOException;
}
