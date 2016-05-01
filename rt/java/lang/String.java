package java.lang;

public final class String {
    private final char[] value;

    public String(char[] value) {
        this.value = new char[value.length];
        System.arraycopy(value, 0, this.value, 0, value.length);
    }

    public byte[] getBytes() {
        // TODO we're assuming UTF-16 here
        byte[] b = new byte[2 * value.length];
        for (int i = 0; i < value.length; i++) {
            b[2 * i] = (byte) (value[i] & 0xff00 >>> 8);
            b[2 * i + 1] = (byte) (value[i] & 0x00ff);
        }
        return b;
    }
}
