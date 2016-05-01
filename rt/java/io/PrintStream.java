package java.io;

// TODO can you make one of these without automatic flushing?
// TODO InterruptedIOException (see checkError() docs)
public class PrintStream extends FilterOutputStream {
    private boolean error;

    public PrintStream(OutputStream out) {
        super(out);
        error = false;
    }

    @Override
    public void flush() {
        try {
            super.flush();
        } catch (IOException iox) {
            error = true;
        }
    }

    @Override
    public void close() {
        try {
            flush();
            super.close();
        } catch (IOException iox) {
            error = true;
        }
    }

    public boolean checkError() {
        return error;
    }

    protected void setError() {
        error = true;
    }

    protected void clearError() {
        error = false;
    }

    @Override
    public void write(int b) {
        try {
            super.write(b);
            if (b == '\n') {
                flush();
            }
        } catch (IOException iox) {
            error = true;
        }
    }

    @Override
    public void write(byte[] b, int off, int len) {
        try {
            super.write(b, off, len);
            flush();
        } catch (IOException iox) {
            error = true;
        }
    }

    public void print(String s) {
        byte[] b;
        if (s == null) {
            b = "null".getBytes();
        } else {
            b = s.getBytes();
        }
        write(b, 0, b.length);
    }

    public void println() {
        // TODO because fuck portability
        write('\n');
    }

    public void println(String x) {
        print(x);
        println();
    }
}
