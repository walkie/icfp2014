import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

/**
 * Named label
 * @author Felix Rieger
 *
 */
public class Label {
	static HashMap<String, Integer> labels = new HashMap<String, Integer>();
	
	public static void putLabel(String name, int line) {
		labels.put(name.toLowerCase(), line);
	}
	
	public static int get(String name) {
		return labels.get(name.toLowerCase());
	}
	
	public static Set<Entry<String, Integer>> getEntries() {
		return labels.entrySet();
	}
	
	public static void dbg() {
		System.out.println(labels);
	}
}
