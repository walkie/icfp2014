import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

/**
 * Named constant
 * @author Felix Rieger
 *
 */
public class NCons {
	static HashMap<String, String> constants = new HashMap<String, String>();
	
	public static void putConstant(String name, String value) {
		constants.put(name.toLowerCase(), value);
	}
	
	public static String get(String name) {
		return constants.get(name.toLowerCase());
	}
	
	public static Set<Entry<String, String>> getEntries() {
		return constants.entrySet();
	}
	
	public static void dbg() {
		System.out.println(constants);
	}
}
