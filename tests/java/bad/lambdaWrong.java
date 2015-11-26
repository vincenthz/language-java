public class LambdaWrong {
    // illegal: can't mix inferred and declared types
    Type x = (x, int y) -> x + y;
    // Illegal: no modifiers with inferred types
    Type x = (x, final y) -> x-y;
}