import java.util.Random;
class MiscMath<T extends Number>{
	Collection<Number> fromArray(Number[] na) {
		Collection<Number> cn = new ArrayList<Number>();
		for (Number n : na) {
                   cn.add(n);
		}
		return cn;
	}
}