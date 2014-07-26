import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.Stack;
import java.util.StringTokenizer;

public class GhcMacroAsm {

	static String DATA_SEGMENT = "$data";
	static String VAR_SEGMENT = "$var";
	static String PGM_SEGMENT = "$pgm";
	
	static String delim = " \t";

	public static void main(String[] args) throws IOException {

		if (args.length != 2) {
			System.out.println("usage: INPUT_FILE OUTPUT_FILE");
			System.exit(-1);
		}
		

		String inFile = args[0];
		String outFile = args[1];
		
		
		ArrayList<String> programLines = new ArrayList<String>(500);
		ArrayList<ArrayList<String>> tokenizedProgram = new ArrayList<ArrayList<String>>();

		Scanner sc = new Scanner(new File(inFile));
		
		while(sc.hasNextLine()) {
			programLines.add(sc.nextLine());
		}
		sc.close();
		
		

		// tokenize the program
		
		for (String pline : programLines) {
			StringTokenizer tkn = new StringTokenizer(pline, delim, false);
			ArrayList<String> tokens = new ArrayList<String>(16);
			while (tkn.hasMoreElements()) {
				tokens.add(tkn.nextToken());
			}
			tokenizedProgram.add(tokens);
		}

		System.out.println(tokenizedProgram);
		
		
		// look for the data segment

		ArrayList<ArrayList<String>> dataSegment = getSubList(tokenizedProgram, getSegmentBounds(tokenizedProgram, DATA_SEGMENT));
		ArrayList<ArrayList<String>> varSegment = getSubList(tokenizedProgram, getSegmentBounds(tokenizedProgram, VAR_SEGMENT));
		ArrayList<ArrayList<String>> pgmSegment = getSubList(tokenizedProgram, getSegmentBounds(tokenizedProgram, PGM_SEGMENT));

		
		
		System.out.println(dataSegment);
		System.out.println(varSegment);
		System.out.println(pgmSegment);
		
		
		// add emulated things
		for (int i = 0; i < pgmSegment.size(); i++) {
			if (pgmSegment.get(i).size() == 0) {
				continue;
			}
			
			String instruction = (pgmSegment.get(i).get(0));
			processInstruction(instruction, pgmSegment, i);
		}
		
		
		
		// build variables and constants
		
		
		for (int i = 1; i < dataSegment.size(); i++) {
			ArrayList<String> line = dataSegment.get(i);
			if (line.size() == 0) {
				continue;
			}
			
			for (int u = 0; u < line.size(); u++) {
				String tkn = line.get(u);
				if (tkn.startsWith("'")) {
					// -> constant definition
					// store the next token in the named constant store
					NCons.putConstant(tkn, line.get(u+1));
					break;
				}
			}
		}
		
		
		NCons.dbg();
		
		for (int i = 1; i < varSegment.size(); i++) {
			ArrayList<String> line = varSegment.get(i);
			if (line.size() == 0) {
				continue;
			}
			
			for (int u = 0; u < line.size(); u++) {
				String tkn = line.get(u);
				if (tkn.startsWith(".")) {
					// -> constant definition
					// store the next token in the named constant store
					NVar.putConstant(tkn, line.get(u+1));
					break;
				}
			}
		}

		NVar.dbg();
		
		
		// replace variables and constants in the program

		for (Entry<String, String> e : NCons.getEntries()) {
			replaceAll(pgmSegment, e.getKey(), e.getValue());
		}
		for (Entry<String, String> e : NVar.getEntries()) {
			replaceAll(pgmSegment, e.getKey(), e.getValue());
		}

		
		
		
		System.out.println(pgmSegment);
		
		// remove garbage from program segment
		Stack<Integer> cleanup = new Stack<Integer>();
		cleanup.push(0);
		for (int i = 0; i < pgmSegment.size(); i++) {
			ArrayList<String> line = pgmSegment.get(i);
			if (line.size() == 0) {
				cleanup.push(i);
				continue;
			}
			if (line.get(0).trim().startsWith(";")) {
				// remove comment lines
				cleanup.push(i);
			}
		}
		
		System.err.println(cleanup);

		while(!cleanup.empty()) {
			pgmSegment.remove((int) cleanup.pop());
		}
		
		

		
		
		// get labels
		
		Stack<Integer> labelCleanup = new Stack<Integer>();
		int labelOffset = 0;
		for (int i = 0; i < pgmSegment.size(); i++) {
			ArrayList<String> line = pgmSegment.get(i);
			if (line.size() == 0) {
				// shouldn't happen
				labelOffset++;
				continue;
			}
			if (line.get(0).trim().startsWith("\"")) {
				// label found
				int labelNameEnd = line.get(0).trim().indexOf("\"", 1) + 1;
				String labelName = line.get(0).trim().substring(0, labelNameEnd);
				
				labelCleanup.push(i);
				Label.putLabel(labelName, i - labelOffset);
				labelOffset++;
			}
		}
		
		while(!labelCleanup.empty()) {
			pgmSegment.remove((int) labelCleanup.pop());
		}

		
		// replace labels

		for (Entry<String, Integer> e : Label.getEntries()) {
			replaceAll(pgmSegment, e.getKey(), "" + e.getValue());
		}


		// indentation of comments
		replaceAll(pgmSegment, ";", "\t\t\t;");
		
		System.out.println(generateAsm(pgmSegment, true));
		
		Label.dbg();
		
		FileWriter fw = new FileWriter(new File(outFile));
		fw.write(generateAsm(pgmSegment, false));
		fw.close();
		
		System.out.println("wrote " + outFile);
		
	}
	
	
	public static int[] getSegmentBounds(ArrayList<ArrayList<String>> tokenizedProgram, String segmentName) {
		int lineIdx = 0;
		boolean foundStart = false;
		int[] segmentBounds = {-1, -1};
		for (lineIdx = 0; lineIdx < tokenizedProgram.size(); lineIdx++) {
			ArrayList<String> currentLine = tokenizedProgram.get(lineIdx);
			System.out.println(currentLine);
			if (currentLine.size() == 0) {
				// nothing 
			} else if (!foundStart && currentLine.get(0).equalsIgnoreCase(segmentName)) {
				// found start of data segment
				foundStart = true;
				segmentBounds[0] = lineIdx;
			} else if (foundStart && currentLine.get(0).startsWith("$")) {
				// end of segment
				segmentBounds[1] = lineIdx - 1;
				break;
			}
		}
		if ((segmentBounds[0] != -1) && (segmentBounds[1] == -1)) {
			segmentBounds[1] = lineIdx;	// last segment of the program
		}

		return segmentBounds;
	}
	
	public static ArrayList<ArrayList<String>> getSubList(ArrayList<ArrayList<String>> oldList, int[] segmentBounds) {
		return getSubList(oldList, segmentBounds[0], segmentBounds[1]);
	}
	
	public static ArrayList<ArrayList<String>> getSubList(ArrayList<ArrayList<String>> oldList, int startidx, int endidx) {
		ArrayList<ArrayList<String>> newList = new ArrayList<ArrayList<String>>();
		for (int i = startidx; i < endidx; i++) {
			newList.add(oldList.get(i));
		}
		
		return newList;
	}
	
	
	
	public static void replaceAll(ArrayList<ArrayList<String>> program, String searchFor, String replaceBy) {
		for (int i = 0; i < program.size(); i++) {
			for (int u = 0; u < program.get(i).size(); u++) {
				String str = program.get(i).get(u).toLowerCase();
				if (str.contains(searchFor.toLowerCase())) {
					System.out.println("match found!" + str + " -> " + replaceBy);
					program.get(i).set(u, str.replace(searchFor.toLowerCase(), replaceBy.toLowerCase()));
				}
			}
		}
	}
	
	public static String generateAsm(ArrayList<ArrayList<String>> program, boolean addLineNumbersComment) {
		StringBuilder sb = new StringBuilder();
		int currentLine = 0;
		for (ArrayList<String> line : program) {
			for (String tkn : line) {
				sb.append(tkn + " ");
			}
			if (addLineNumbersComment) {
				sb.append("; \t" + currentLine);
			}
			sb.append("\n");
			currentLine++;
		}
		
		return sb.toString();
	}
	
	public static void processInstruction(String instructionName, ArrayList<ArrayList<String>> program, int indexInProgram) {
		String newCode = null;
		switch(instructionName.toLowerCase()) {
		case "nop":
			newCode = "mov pc, pc" + " ; emulated nop";
			break;
		case "ret":
			newCode = "mov pc, [255]" + " ; emulated ret";
			break;
		case "jmp":
			String destLabel = program.get(indexInProgram).get(1).trim().toLowerCase();
			// TODO: add rest
			String rest = " ; emulated jmp ";
			newCode = "mov pc, " + destLabel + rest;
			break;
		case "jsr":
			String destLabel2 = program.get(indexInProgram).get(1).trim().toLowerCase();
			newCode = "mov [255], pc   ; emulated jsr" + "\n"
					+ "mov pc, " + destLabel2 + " ; emulated jsr";
			break;
		case "dbg":
			String dbgVar = program.get(indexInProgram).get(1).trim().toLowerCase();
			newCode = "mov f, " + dbgVar + "	; debug \n"
					+  "int 8 	; debug ";
			break;
		case "set-direction":
			newCode = "int 0	; set direction";
			break;
		case "get-first-coords":
			newCode = "int 1	; get first lambdaman coordinates";
			break;
		case "get-second-coords":
			newCode = "int 2	; get second lambdaman coordinates";
			break;
		case "get-ghost-id":
			newCode = "int 3	; get ghost id";
			break;
		case "get-ghost-start-coords":
			newCode = "int 4	; get ghost start coords";
			break;
		case "get-ghost-coords":
			newCode = "int 5	; get ghost coords";
			break;
		case "get-ghost-status":
			newCode = "int 6	; get ghost status";
			break;
		case "get-map":
			newCode = "int 7	; get map";
			break;
		case "dump":
			newCode = "int 8";
			break;
		}
		
		if (newCode != null) {
			StringTokenizer lineTkn = new StringTokenizer(newCode, "\n", false);
			ArrayList<String> lines = new ArrayList<String>();
			
			// remove old instruction
			program.remove(indexInProgram);
			
			while(lineTkn.hasMoreTokens()) {
				lines.add(lineTkn.nextToken());
			}
			
			int lineIdx = 0;
			for (String lne : lines) {
				StringTokenizer tkn = new StringTokenizer(lne, delim, false);
				
				ArrayList<String> tkns = new ArrayList<String>();
				while(tkn.hasMoreTokens()) {
					tkns.add(tkn.nextToken());
				}
				
				program.add(indexInProgram + lineIdx, tkns);
				
				lineIdx++;
			}
		}
	}
}
