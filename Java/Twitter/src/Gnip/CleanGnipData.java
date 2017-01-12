package Gnip;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.*;
import org.json.simple.*;

public class CleanGnipData {
	private static final int NUM_ROWS = 26796;
	private static final int NUM_MONTHS = 12;
	private static final int YEAR = 2006;
	//private static final int START_MONTH = 2010 * 12 + 12;
	
	private static int getYear(String filePath) {
		// assume the path looks like ....\yyyy\mm\dd\hh\mm_activities.txt
		int len = filePath.length();
		int yearOffset1 = -31;
		int yearOffset2 = -27;
		String yearStr = filePath.substring(len + yearOffset1, len + yearOffset2);
		return Integer.valueOf(yearStr);
	}
	
	private static int getMonth(String filePath) {
		// assume the path looks like ....\yyyy\mm\dd\hh\mm_activities.txt
		int len = filePath.length();
		int yearOffset1 = -26;
		int yearOffset2 = -24;
		String monStr = filePath.substring(len + yearOffset1, len + yearOffset2);
		return Integer.valueOf(monStr);		
	}
	
	private static String[] getTwitterScreenNames() throws IOException{
		String fileName = "C:\\Users\\lz7\\Dropbox\\Default Prediction For Small Business Loans\\Data\\TwitterScreenNames.txt";
		BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("windows-1252"));
		String[] retval = new String[NUM_ROWS];
		String line = null;
		int count = 0;
		while((line = reader.readLine()) != null) {
			retval[count] = "@" + line;
			count++;
		}
		reader.close();
		return retval;
	}
	
	public static void main(String[] args) throws IOException  {
		File root = new File("C:/Users/lz7/Desktop/Gnip Data/" + YEAR);
		Collection<File> fileList = FileUtils.listFiles(root, new RegexFileFilter(".*"), DirectoryFileFilter.DIRECTORY);
		Iterator<File> fileIter = fileList.iterator();
		String[] firmNames = getTwitterScreenNames();
		
		String[] firmNamesLowerCase = new String[firmNames.length];
		for(int i = 0; i < firmNames.length; i++) {
			firmNamesLowerCase[i] = firmNames[i].toLowerCase();
		}
		
		int[][] activityCount = new int[NUM_ROWS][NUM_MONTHS];
		int count = 0;
		
		while(fileIter.hasNext()) {
			File f = fileIter.next();
			String fileName = f.getPath();
			//int year = getYear(fileName);
			int month = getMonth(fileName);
			int colIndex = month - 1;
			//int colIndex = year * 12 + month - START_MONTH;

			try{ 
				BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("UTF-8"));
//				BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("windows-1252"));
				String line = null;
				while((line = reader.readLine()) != null) {
					if(line.length() == 0) continue;
					JSONObject obj = (JSONObject) JSONValue.parse(line);
					if(!obj.containsKey("body")) continue;
					String tweet = obj.get("body").toString().toLowerCase();
					for(int i = 0; i < NUM_ROWS; i++) {
						if(tweet.contains(firmNamesLowerCase[i])) {
							activityCount[i][colIndex] ++;
						}
					}
				}
				reader.close();
			} catch (Exception e) {
				System.out.println(fileName);
				System.out.println(e);
			}
			count ++;
			if(count % 500 == 0) System.out.println(count);
		}
		
		PrintWriter writer = new PrintWriter("C:/Users/lz7/Dropbox/Default Prediction For Small Business Loans/Data/Monthly Tweet Count " + YEAR + ".txt", "UTF-8");
		for(int row = 0; row < NUM_ROWS; row++) {
			StringBuilder builder = new StringBuilder();
			builder.append(firmNames[row]);
			for(int col = 0; col < NUM_MONTHS; col++) {
				builder.append("\t" + activityCount[row][col]);
			}
			writer.println(builder.toString());
		}
		writer.close();
    }
}
