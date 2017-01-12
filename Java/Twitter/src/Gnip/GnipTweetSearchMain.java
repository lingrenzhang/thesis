package Gnip;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
// STEP 1
public class GnipTweetSearchMain {

	private static final int startYear = 2007;
	private static final int endYear = 2011;
	private static final String month = "01";
	private static final String QUOTED_STATUS = "quoted";
	//private static final String COMPLETE_STATUS = "Job delivered and available for download.";
	
	private static final int NUM_RULE_PER_JOB = 900; // up to 1000	
	private static final int NUM_ACCOUNT_PER_RULE = 30;
	private static final int NUM_TWITTER = 26796; // sanity check
	
	
	private static ArrayList<String> getTwitterScreenNames() throws IOException{
		String fileName = "C:\\Users\\lz7\\Dropbox\\Default Prediction For Small Business Loans\\Data\\TwitterScreenNames.txt";
		BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("windows-1252"));
		ArrayList<String> retval = new ArrayList<String>();
		String line = null;
		while((line = reader.readLine()) != null) {
			retval.add(line);
		}
		return retval;
	}
	
	// returns {"value":"@name1 OR @name2 OR @name3"}
	private static String getRule(ArrayList<String> screenNames, String tag) throws IOException {
		int size = screenNames.size();
		if(size > NUM_ACCOUNT_PER_RULE) {
			throw new IOException("number of accounts exceeded per rule limit");
		}
		if(size == 0) {
			throw new IOException("Unexpected empty screen name list");
		}
		
		StringBuilder builder = new StringBuilder();
		builder.append("{\"value\":\"");
		for(int i = 0; i < size; i++) {
			if(i > 0) {
				builder.append(" OR ");
			}
//			builder.append("@" + screenNames.get(i)); // regular
			builder.append(screenNames.get(i)); // alternative 1
//			builder.append("contains:" + "\\\"" + screenNames.get(i) + "\\\""); // alternative 2
		}
		builder.append("\",\"tag\":\"" + tag + "\"}");
		return builder.toString();
	}
	
	private static String getRules(ArrayList<String> screenNames, int startIdx, int endIdx) throws IOException {
		StringBuilder builder = new StringBuilder();
		builder.append("[");
		while(startIdx < endIdx) {
			ArrayList<String> namesForRule = new ArrayList<String>();
			for(int i = startIdx; i < Math.min(endIdx, startIdx + NUM_ACCOUNT_PER_RULE); i++) {
				namesForRule.add(screenNames.get(i));
			}
			String tag = Integer.toString(startIdx);
			builder.append(getRule(namesForRule, tag));
			startIdx += NUM_ACCOUNT_PER_RULE;
			if(startIdx < endIdx) {
				builder.append(",");			
			}
		}
		builder.append("]");
		return builder.toString();
	}
	
	public static void main(String[] args) throws IOException {
		ArrayList<String> screenNames = getTwitterScreenNames();
		int startIdx = 0;
		int endIdx = screenNames.size(); // non-inclusive
		if(endIdx != NUM_TWITTER) {
			throw new IOException("Unexpected number of twitter accounts");
		}
		
		String rules = getRules(screenNames, 0, endIdx);
		//System.out.println(rules.substring(0,100));
		
		//for(int i = 0; i < 45; i++) {
		//	System.out.println(rules.substring(i*10000, (i+1)*10000));
		//}
		//System.out.println(rules.substring(450000));
		//System.out.println(rules);
		
		//String jobURL = GnipTweetSearchLib.createHistoricalJob(rules, "200612", "200701", "batch8");
		//System.out.println(jobURL);
		
		//for(int year = startYear; year < endYear; year++) {
		//	String startMonth = Integer.toString(year) + month;
		//	String endMonth = Integer.toString(year + 1) + month;
		//	String jobURL = GnipTweetSearchLib.createHistoricalJob(rules, startMonth, endMonth, "batch9");
		//	System.out.println(jobURL);
		//}

		//if(jobURL.length() == 0) {
		//	System.out.println("job create failed");
		//	return;
		//}
		//String jobURL = "https://historical.gnip.com:443/accounts/StanfordExplore/publishers/twitter/historical/track/jobs/t9bkajyv12.json";
		
		//while(GnipTweetSearchLib.getJobStatus(jobURL).compareTo(QUOTED_STATUS) != 0) {
		//	try {
		//		Thread.sleep(10*1000);
		//	} catch (InterruptedException e) {
		//	}
		//}
		
		//System.out.println(GnipTweetSearchLib.acceptJob(jobURL));
	}

}
