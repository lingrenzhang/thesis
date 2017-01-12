package Facebook;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;

import javax.net.ssl.HttpsURLConnection;

import org.apache.http.client.utils.URIBuilder;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;



public class FacebookPageSearch {
	private static int BATCH_SIZE = 600;
	private static int QUERY_LIMIT = 300;
	//private static int FILE_SIZE = 444039;
	
	private static String OUTPUT_DIR = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\FBOutput\\";
	private static String CACHE_DIR = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\FBCache\\";
	
	//SMETest
	//private static String APP_ID = "810836462334971";
	//private static String APP_SECRET = "c39a7bf89283b6d37f34997be897357c";
	
	//SMETest1
	private static String APP_ID = "1628326834064742";
	private static String APP_SECRET = "56ee20596c8bd9f306795c9e8679abcc";
	
	private static String SEARCH_END_POINT = "https://graph.facebook.com/search";
	private static String PAGE_END_POINT = "https://graph.facebook.com/";
	
	//https://graph.facebook.com/search?q=duke&type=page&access_token=810836462334971|c39a7bf89283b6d37f34997be897357c
	//https://graph.facebook.com/102875824455?fields=likes
	
	private static ArrayList<String> getKeywordList() throws IOException{
		String fileName = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\SBA_7a_2015_names.txt";
		BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("windows-1252"));
		ArrayList<String> retval = new ArrayList<String>();
		String line = null;
		while((line = reader.readLine()) != null) {
			if(line.isEmpty()) {
				retval.add("\"\"");
				continue;
			}
			
			if(line.charAt(0) == '\"') line = line.substring(1);
			if(line.charAt(line.length()-1) == '\"') line = line.substring(0, line.length() - 1);
			line = "\"" + FBExcelUtil.preprocessKeyword(line) + "\"";
			retval.add(line);
		}
		return retval;
	}
	
	private static ArrayList<Boolean> getTwitterExist() throws IOException{
		String fileName = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\TwitterAccountExist.txt";
		BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("windows-1252"));
		ArrayList<Boolean> retval = new ArrayList<Boolean>();
		String line = null;
		while((line = reader.readLine()) != null) {
			if(line.equals("TRUE")) {
				retval.add(true);
				continue;
			}
			if(line.equals("FALSE")) {
				retval.add(false);
				continue;
			}
			throw new IOException("Unexpected input from " + fileName);
		}
		return retval;
	}
	
	private static String fetchPageSearchResponse (String endPointURL, String keyword) throws IOException {
		HttpsURLConnection connection = null;
		
		try{
			URI uri = new URIBuilder(endPointURL)
				.addParameter("q", keyword)
				.addParameter("type", "page")
				.addParameter("access_token", APP_ID + "|" + APP_SECRET)
				.build();
			URL url = new URL(uri.toString());
			
			connection = (HttpsURLConnection) url.openConnection();
			connection.setDoOutput(true);
			connection.setDoInput(true);
			connection.setRequestMethod("GET");
			connection.setRequestProperty("Host", "graph.facebook.com");
		    connection.setUseCaches(false);
			
			String response = FBSearchEngineUtils.readResponse(connection);
			return response;
		} catch (MalformedURLException|URISyntaxException e) {
			throw new IOException("Invalid endpoint URL specified.", e);
		} finally {
			if (connection != null) {
				connection.disconnect();
			}
		}
	}
	

	private static String fetchPageById (String endPointURL, String pageId) throws IOException {
		HttpsURLConnection connection = null;
		
		try{
			URI uri = new URIBuilder(endPointURL + pageId).build();
			URL url = new URL(uri.toString());
			
			connection = (HttpsURLConnection) url.openConnection();
			connection.setDoOutput(true);
			connection.setDoInput(true);
			connection.setRequestMethod("GET");
			connection.setRequestProperty("Host", "graph.facebook.com");
		    connection.setUseCaches(false);
			
			String response = FBSearchEngineUtils.readResponse(connection);
			return response;
		} catch (MalformedURLException|URISyntaxException e) {
			throw new IOException("Invalid endpoint URL specified.", e);
		} finally {
			if (connection != null) {
				connection.disconnect();
			}
		}
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
	
	private static void retrievePageList(ArrayList<String> keywords, ArrayList<Boolean> twtExist) throws IOException{
		int batchIndex = 641;
		int startIndex = batchIndex * BATCH_SIZE;
		int queryCount = 0;
		
		while(startIndex < keywords.size()) {
			String fileName = "part-" + String.format("%04d", batchIndex) + ".txt";
			PrintWriter outputWriter = new PrintWriter(OUTPUT_DIR + fileName, "UTF-8");
			PrintWriter cacheWriter = new PrintWriter(CACHE_DIR + fileName, "UTF-8");
			int endIndex = Math.min(startIndex + BATCH_SIZE, keywords.size());
			for(int keywordIndex = startIndex; keywordIndex < endIndex; keywordIndex ++) {
				String keyword = keywords.get(keywordIndex);
				if(!twtExist.get(keywordIndex)) {
					cacheWriter.println("[]");
					String[] outputs = {String.valueOf(keywordIndex), keyword, "", "", "0", "false"};
					outputWriter.println(arrToString(outputs, "\t"));
					continue;
				}
				
				String pageSearchResponse = new String("{}");
				try {
					pageSearchResponse = fetchPageSearchResponse(SEARCH_END_POINT, keyword);
				} catch (IOException e) {
					// authentication is flaky, retry once on fail
					try {
						Thread.sleep(1000);
						pageSearchResponse = fetchPageSearchResponse(SEARCH_END_POINT, keyword);				
					} catch (InterruptedException|IOException e1) {
					}
				}
				
				cacheWriter.println(pageSearchResponse);
			    JSONObject responseObj = (JSONObject) JSONValue.parse(pageSearchResponse);
			    JSONArray pageArr = (JSONArray) responseObj.get("data");
			    if(pageArr != null && pageArr.size() > 0) {
					JSONObject searchPageObj = (JSONObject) pageArr.get(0); // just grab the first search result
					String pageId = searchPageObj.get("id").toString();
					
					String pageResponse = fetchPageById(PAGE_END_POINT, pageId);
					JSONObject pageObj = (JSONObject) JSONValue.parse(pageResponse);
					
					String name 	= pageObj.get("name").toString();
					int likes = Integer.parseInt(pageObj.get("likes").toString());
					String[] outputs = {String.valueOf(keywordIndex), keyword, pageId, name, 
							String.valueOf(likes), "true"};
					outputWriter.println(arrToString(outputs, "\t"));
				} else {
					String[] outputs = {String.valueOf(keywordIndex), keyword, "", "", "0", "false"};
					outputWriter.println(arrToString(outputs, "\t"));
				}
				
			    queryCount++;
			    if(queryCount >= QUERY_LIMIT) {
			    	queryCount = 0; // reset query count and break for 10 minutes
					try {
						Thread.sleep(10*60*1000);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
			    }
			}
			startIndex += BATCH_SIZE;
			System.out.println(startIndex);
			Date d = new Date();
			System.out.println(d);
			outputWriter.close();
			cacheWriter.close();
			batchIndex ++;
		}
	}
	
	public static void main(String[] args) throws IOException {
		ArrayList<String> keywords = getKeywordList();
		ArrayList<Boolean> twtExist = getTwitterExist();
		retrievePageList(keywords, twtExist);
	}
}
