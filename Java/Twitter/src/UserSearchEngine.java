import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

import javax.net.ssl.HttpsURLConnection;

import org.apache.http.client.utils.URIBuilder;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;


public class UserSearchEngine {
	private static int DEFAULT_SEARCH_COUNT = 5;
	private static int JOB_SIZE = 150;
	private static int BATCH_SIZE = JOB_SIZE * AuthenticationCredentials.AGENT_NUM;
	private static int FILE_SIZE = 444039;
	
	private static String OUTPUT_DIR = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\Output\\";
	private static String CACHE_DIR = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\Cache\\";
		
	private static String fetchUserSearchResponse(String endPointURL, TreeMap<String, String> requestParameters, AuthenticationConfig config) throws IOException {
		HttpsURLConnection connection = null;
		
		try{
			URIBuilder uriBuilder =  new URIBuilder(endPointURL);
			for(Map.Entry<String, String> e : requestParameters.entrySet()) {
				uriBuilder.addParameter(e.getKey(), e.getValue());
			}
			URI uri = uriBuilder.build();
			URL url = new URL(uri.toString().replaceAll("\\+", "%20"));
			//System.out.println(url.toString());
			connection = (HttpsURLConnection) url.openConnection();
			connection.setDoOutput(true);
			connection.setDoInput(true);
			connection.setRequestMethod("GET");
			connection.setRequestProperty("Host", "api.twitter.com");
	        connection.setRequestProperty("User-Agent", config.getUserAgent());
		    connection.setRequestProperty("Authorization", config.getAuthorizationString(requestParameters));
		    connection.setUseCaches(false);
		    
		    String response = SearchEngineUtils.readResponse(connection);
		    return response;
		} catch (MalformedURLException|URISyntaxException e) {
			throw new IOException("Invalid endpoint URL specified.", e);
		} finally {
			if (connection != null) {
				connection.disconnect();
			}
		}
	}
	
	private static ArrayList<String> getKeywordList() throws IOException{
		String fileName = "C:\\Users\\lz7\\Dropbox\\Social Network\\Data\\SBA_7a_2015_names.txt";
		BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.defaultCharset());
		ArrayList<String> retval = new ArrayList<String>();
		String line = null;
		while((line = reader.readLine()) != null) {
			if(line.isEmpty()) {
				retval.add("\"\"");
				continue;
			}
			
			if(line.charAt(0) == '\"') line = line.substring(1);
			if(line.charAt(line.length()-1) == '\"') line = line.substring(0, line.length() - 1);
			line = "\"" + ExcelUtil.preprocessKeyword(line) + "\"";
			retval.add(line);
		}
		return retval;
	}
	
	private static ArrayList<String> getTestKeywordList() throws IOException{
		ArrayList<String> retval = new ArrayList<String>();
		retval.add("\"Menestys\"");
		retval.add("\"Solution X\"");
		retval.add("\"Gaucho PR\"");
		retval.add("\"GARLANS\"");
		retval.add("\"D & A ELECTRIC\"");
		retval.add("\"ISTABILIZER\"");
		retval.add("\"SHANTINATH\"");
		return retval;
	}
	
	private static boolean nameMatch(String substr, String str) {
		String substr1 = substr.replaceAll("\\s", "");
		String str1 = str.replaceAll("\\s", "");
		return str.toLowerCase().contains(substr.toLowerCase()) || str1.toLowerCase().contains(substr1.toLowerCase());
	}
	
	private static User userFilter(String keyword, JSONArray userArr) {
		int size = userArr.size();
		ArrayList<User> userList = new ArrayList<User>();
		for(int i = 0; i < size; i++) {
			JSONObject userObj = (JSONObject) userArr.get(i);
			String description = userObj.get("description").toString();
			String name = userObj.get("name").toString();
			String screenName = userObj.get("screen_name").toString();
			int matchCount = 0;
			String keywordWOQuotation = keyword.substring(1, keyword.length() - 1);
			if(nameMatch(keywordWOQuotation, description)) matchCount++;
			if(nameMatch(keywordWOQuotation, name)) matchCount++;
			if(nameMatch(keywordWOQuotation, screenName)) matchCount++;
			if(matchCount == 0) continue;
			
			int followerCount = Integer.parseInt(userObj.get("followers_count").toString());
			int friendCount = Integer.parseInt(userObj.get("friends_count").toString());
			int statusCount = Integer.parseInt(userObj.get("statuses_count").toString());
			userList.add(new User(keyword, description, name, screenName, followerCount, friendCount, statusCount, false, true, matchCount));
		}
		if(userList.size() == 0) return new User(keyword, "", "", "", 0, 0, 0, false, false, 0);
		if(userList.size() > 1) userList.get(0).setUniqueAmongFilteredResults(false);
		return userList.get(0);
	}
	
	private static void retrieveUserList(ArrayList<String> keywords) throws IOException{
		int batchIndex = 697;
		int startIndex = batchIndex * BATCH_SIZE;
		
		while(startIndex < keywords.size()) {
			String fileName = "part-" + String.format("%04d", batchIndex) + ".txt";
			PrintWriter outputWriter = new PrintWriter(OUTPUT_DIR + fileName, "UTF-8");
			PrintWriter cacheWriter = new PrintWriter(CACHE_DIR + fileName, "UTF-8");
			for(int agentIndex = 0; agentIndex < AuthenticationCredentials.AGENT_NUM; agentIndex++) {
				if(startIndex >= keywords.size()){
					break;
				}
				AuthenticationConfig config = new AuthenticationConfig(agentIndex);
				int endIndex = Math.min(startIndex + JOB_SIZE, keywords.size());
				for(int keywordIndex = startIndex; keywordIndex < endIndex; keywordIndex ++) {
					String keyword = keywords.get(keywordIndex);
					TreeMap<String, String> requestParameters = new TreeMap<String, String>();
					requestParameters.put("q", keyword);
					requestParameters.put("count", Integer.toString(DEFAULT_SEARCH_COUNT));
					String userSearchResponse = new String();
					try {
						userSearchResponse = fetchUserSearchResponse(AuthenticationConfig.SEARCH_URL, requestParameters, config);
					} catch (IOException e) {
						// authentication is flaky, retry once on fail
						try {
							Thread.sleep(1000);
							userSearchResponse = fetchUserSearchResponse(AuthenticationConfig.SEARCH_URL, requestParameters, config);				
						} catch (InterruptedException|IOException e1) {
						}
					}
					
					cacheWriter.println(userSearchResponse);
				    JSONArray userArr = (JSONArray) JSONValue.parse(userSearchResponse);
				    if(userArr != null) {
						int size = userArr.size();
						if(size == 0) {
							String[] outputs = {String.valueOf(keywordIndex), keyword, "", "", "", "0", "0", "0", "false", "false", "0"};
							outputWriter.println(SearchEngineUtils.arrToString(outputs, "\t"));
							continue;
						}
						if(size > 1) {
							User user = userFilter(keyword, userArr);
							String[] outputs = {
									String.valueOf(keywordIndex), 
									keyword, 
									user.getDescription(), 
									user.getName(),
									user.getScreenName(), 
									String.valueOf(user.getFollowerCount()), 
									String.valueOf(user.getFriendCount()), 
									String.valueOf(user.getStatusCount()), 
									String.valueOf(user.getUniqueAmongRawResults()), 
									String.valueOf(user.getUniqueAmongFilteredResults()), 
									String.valueOf(user.getMatchScore())};
							outputWriter.println(SearchEngineUtils.arrToString(outputs, "\t"));
						} else {
							JSONObject userObj = (JSONObject) userArr.get(0);
							String description = userObj.get("description").toString();
							String name = userObj.get("name").toString();
							String screenName = userObj.get("screen_name").toString();
							int followerCount = Integer.parseInt(userObj.get("followers_count").toString());
							int friendCount = Integer.parseInt(userObj.get("friends_count").toString());
							int statusCount = Integer.parseInt(userObj.get("statuses_count").toString());
							String[] outputs = {String.valueOf(keywordIndex), keyword, description, name, screenName, 
									String.valueOf(followerCount), 
									String.valueOf(friendCount),
									String.valueOf(statusCount),
									"true", "true", "3"};
							outputWriter.println(SearchEngineUtils.arrToString(outputs, "\t"));
						}
					} else {
						String[] outputs = {String.valueOf(keywordIndex), keyword, "", "", "", "0", "0", "0", "false", "false", "0"};
						outputWriter.println(SearchEngineUtils.arrToString(outputs, "\t"));
					}
					
				}
				startIndex += JOB_SIZE;
				System.out.println(startIndex);
				Date d = new Date();
				System.out.println(d);
			}
			outputWriter.close();
			cacheWriter.close();
			batchIndex ++;
			try {
				Thread.sleep(15*60*1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		ArrayList<String> keywords = getKeywordList();
		retrieveUserList(keywords);
	}
}
