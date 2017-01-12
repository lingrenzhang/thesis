import java.io.*;
import java.net.*;
import java.util.ArrayList;

import javax.net.ssl.HttpsURLConnection;

import org.apache.http.client.utils.URIBuilder;
import org.apache.commons.codec.binary.Base64;
import org.json.simple.*;

public class TweetSearchEngine {
	private static final String CONSUMER_KEY = "REUR3kn6BfEpsoxb8yVjS0EDH";
	private static final String CONSUMER_SECRET = "3TSYaSZ3cjKDYPQKhc5JjSux0c7Ym908wphx3ebEOBbn7coCiU";
	private static final String OAUTH_URL = "https://api.twitter.com/oauth2/token";
	private static final String SEARCH_URL = "https://api.twitter.com/1.1/search/tweets.json";
	


	private static String requestBearerToken(String endPointURL) throws IOException {
		String encodedCredentials = SearchEngineUtils.encodeKeys(CONSUMER_KEY, CONSUMER_SECRET);
		HttpsURLConnection connection = null;
		
		try{
			URL url = new URL(endPointURL);
			connection = (HttpsURLConnection) url.openConnection();
			connection.setDoOutput(true);
			connection.setDoInput(true);
			connection.setRequestMethod("POST");
			connection.setRequestProperty("Host", "api.twitter.com");
	        connection.setRequestProperty("User-Agent", "SentimentTestSME");
		    connection.setRequestProperty("Authorization", "Basic " + encodedCredentials);
		    connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded;charset=UTF-8");
		    connection.setRequestProperty("Content-Length", "29");
			connection.setUseCaches(false);
			
			SearchEngineUtils.writeRequest(connection, "grant_type=client_credentials");
			JSONObject obj = (JSONObject) JSONValue.parse(SearchEngineUtils.readResponse(connection));
			//System.out.println(obj.toString());
			if(obj != null) {
				String tokenType = (String) obj.get("token_type");
				String token = (String) obj.get("access_token");
				
				return ((tokenType.equals("bearer")) && (token != null)) ? token : "";
			}
			return new String();
		} catch (MalformedURLException e) {
			throw new IOException("Invalid endpoint URL specified.", e);
		} finally {
			if (connection != null) {
				connection.disconnect();
			}
		}
	}

	private static ArrayList<String> fetchTweets(String endPointURL, String bearerToken, String keyword, int count) throws IOException {
		HttpsURLConnection connection = null;
		
		try{
			URI uri = new URIBuilder(endPointURL)
				.addParameter("q", keyword)
				.addParameter("count", Integer.toString(count))
				.build();
			URL url = new URL(uri.toString());
			
			connection = (HttpsURLConnection) url.openConnection();
			connection.setDoOutput(true);
			connection.setDoInput(true);
			connection.setRequestMethod("GET");
			connection.setRequestProperty("Host", "api.twitter.com");
	        connection.setRequestProperty("User-Agent", "SentimentTestSME");
		    connection.setRequestProperty("Authorization", "Bearer " + bearerToken);
		    connection.setUseCaches(false);
			
			JSONObject obj = (JSONObject) JSONValue.parse(SearchEngineUtils.readResponse(connection));
			ArrayList<String> retval = new ArrayList<String>();
			if(obj != null) {
				JSONArray arr = (JSONArray) obj.get("statuses");
				int size = arr.size();
				for(int i = 0; i < size; i++) {
					String tweet = ((JSONObject) arr.get(i)).get("text").toString();
					retval.add(tweet);
				}
			}
			return retval;
		} catch (MalformedURLException|URISyntaxException e) {
			throw new IOException("Invalid endpoint URL specified.", e);
		} finally {
			if (connection != null) {
				connection.disconnect();
			}
		}
	}
	public static void main(String[] args) throws IOException {
		String bearerToken = requestBearerToken(OAUTH_URL);
		String keyword = "%23betterment";
		int count = 5;
		ArrayList<String> tweets = fetchTweets(SEARCH_URL, bearerToken, keyword, count);
		
		for(String s : tweets) {
			System.out.println(s);
		}
	}
}
