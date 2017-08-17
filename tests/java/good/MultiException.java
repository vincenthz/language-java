public class MultiException {
    void test() {
        try {
        } catch (IOException | IllegalArgumentException e) {
        }
    }
}
