import java.util.HashMap;
import java.util.Set;
import java.util.Map.Entry;

/**
 * Names variable
 * @author Felix Rieger
 *
 */
public class NVar {
	static HashMap<String, String> vars = new HashMap<String, String>();
	
	public static void putConstant(String name, String value) {
		vars.put(name.toLowerCase(), value);
	}
	
	public static String get(String name) {
		return vars.get(name.toLowerCase());
	}
	
	public static Set<Entry<String, String>> getEntries() {
		return vars.entrySet();
	}

	
	
	public static void dbg() {
		System.out.println(vars);
	}
}
