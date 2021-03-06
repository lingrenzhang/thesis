package Facebook;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;


public class FBOutputPostProcess {
	private static String OUTPUT_DIR = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\FBOutput\\";
	private static String POST_DIR = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\FBPostProcess\\";
	
	private static int startBatch = 0;
	private static int endBatch = 741; // non-inclusive
	private static int batchSize = 600;
	private static int totalSize = 444039;
	
	private static String[] readOutput(int batchNum) throws IOException{
		int idx = batchNum * batchSize;
		int curBatchSize = (batchNum == endBatch - 1) ? totalSize - batchNum * batchSize : batchSize;
		int subidx = 0;
		String fileName = "part-" + String.format("%04d", batchNum) + ".txt";
		BufferedReader reader = Files.newBufferedReader(Paths.get(OUTPUT_DIR + fileName), Charset.forName("UTF-8"));
		String[] retval = new String[curBatchSize];
		String line = "";
		while((line = reader.readLine()) != null) {
			if (line.startsWith(String.valueOf(idx)) || idx == 0) {
				retval[subidx] = line;
				subidx ++;
				idx ++; 
			} else {
				retval[subidx - 1] = retval[subidx - 1] + line;
			}
		}
		if(idx != batchNum * batchSize + curBatchSize) throw new IOException("Unexpected output for batch number " + batchNum);
		return retval;
	}
	
	private static String arrToString(String[] arr, String sep) {
		StringBuilder builder = new StringBuilder();
		for(int index = 0; index < arr.length; index ++) {
			if(index > 0) {
				builder.append(sep);
			}
			builder.append(arr[index]);
		}
		return builder.toString();
	}
	
	private static void processOutput(int batchNum, String[] output) throws IOException{
		if(batchNum == 0) {
			output[0] = output[0].substring(1); // no idea what happened there
		}
		for(int i = 0; i < output.length; i++) {
			String origin = output[i];
			String[] segments = origin.split("\t");
			if(segments.length != 6) {
				System.out.println("Warning: Unexpected output at row " + i + " of batch number " + batchNum);
				String[] temp = {segments[0], segments[1], "", "", "0", "false"};
				output[i] = arrToString(temp, "\t");
			} else {
				// delete name
				String[] temp = {segments[0], segments[1], segments[2], segments[4], segments[5]};
				output[i] = arrToString(temp, "\t");				
			}
		}
	}
	
	private static void writeOutput(int batchNum, String[] output) throws FileNotFoundException, UnsupportedEncodingException {
		String fileName = "part-" + String.format("%04d", batchNum) + ".txt";
		PrintWriter writer = new PrintWriter(POST_DIR + fileName, "UTF-8");
		for(String line : output) {
			writer.println(line);
		}
		writer.close();
	}
	
	public static void main(String[] args) throws IOException {
		for(int batchNum = startBatch; batchNum < endBatch; batchNum++){
			String[] output = readOutput(batchNum);
			processOutput(batchNum, output);
			writeOutput(batchNum, output);
		}
	}

}
