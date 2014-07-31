import java.util.Random;
class MiscMath<T extends Number>{
	int divisor;
	MiscMath(int divisor) {
		this.divisor = divisor;
	}
        float ratio(long l) {
                try {
                        l /= divisor;
                } catch (Exception e) {
                        if (e instanceof ArithmeticException)
                                l = Long.MAX_VALUE;
                        else
                                l = 0;
                }
                return (float)l;
        }
        double gausser() {
                Random r = new Random();
                double[] val = new double[2];
                val[0] = r.nextGaussian();
                val[1] = r.nextGaussian();
                return (val[0] + val[1]) / 2;
	}
	Collection<Number> fromArray(Number[] na) {
		Collection<Number> cn = new ArrayList<Number>();
		for (Number n : na) {
			cn.add(n);
		}
		return cn;
	}
   <S> void loop(S s){ this.<S>loop(s);}
}