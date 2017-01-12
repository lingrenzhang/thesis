import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;

import javax.net.ssl.HttpsURLConnection;

import org.apache.commons.codec.binary.Base64;
import org.apache.http.client.utils.URIBuilder;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;


public class UserSearchEngine {
	private static final String CONSUMER_KEY = "REUR3kn6BfEpsoxb8yVjS0EDH";
	private static final String CONSUMER_SECRET = "3TSYaSZ3cjKDYPQKhc5JjSux0c7Ym908wphx3ebEOBbn7coCiU";
	private static final String NONCE = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"; // change to generated for each unique request
	private static final String ACCESS_TOKEN = "1656896509-ZB863KPYi4eMNJPmjCiD5NixLVxErrNIyBA7qu3";
	private static final String ACCESS_TOKEN_SECRET = "RiBLJ7R47eRutab5QlNdMmy42UdVjHtXrUiP3zjIUC0O2";
	private static final String SEARCH_URL = "https://api.twitter.com/1.1/search/tweets.json";
//	private static final String SEARCH_URL = "https://api.twitter.com/1.1/users/search.json";

	private static TreeMap<String, String> getConnectionParameters() {
		TreeMap<String, String> retval = new TreeMap<String, String>();
		retval.put("oauth_consumer_key", CONSUMER_KEY);
		retval.put("oauth_nonce", NONCE);
		retval.put("oauth_signature_method", "HMAC-SHA1");
		int unixTime = (int) (System.currentTimeMillis() / 1000L);
		retval.put("oauth_timestamp", Integer.toString(unixTime));
		retval.put("oauth_token", ACCESS_TOKEN);
		retval.put("oauth_version", "1.0");
		return retval;
	}
	
	private static String getParameterString(TreeMap<String, String> parameters) {
		StringBuilder builder = new StringBuilder();
		for(Map.Entry<String, String> e : parameters.entrySet()) {
			builder.append("&" + e.getKey() + "=" + e.getValue());
		}
		return builder.substring(1);
	}
	
	private static String getOauthSignature(TreeMap<String, String> parameters) {
		try {
			String parameterString = URLEncoder.encode(getParameterString(parameters), "UTF-8");
			String signatureBaseString = "GET&" + URLEncoder.encode(SEARCH_URL, "UTF-8") + "&" + parameterString;
			//System.out.println(signatureBaseString);
			String signingKey = CONSUMER_SECRET + "&" + ACCESS_TOKEN_SECRET;
			String signature = SearchEngineUtils.calculateRFC2104HMAC(signatureBaseString, signingKey);
			return new String(Base64.encodeBase64(signature.getBytes()));
		} catch (UnsupportedEncodingException|SignatureException|NoSuchAlgorithmException|InvalidKeyException e) {
			e.printStackTrace();
			return new String();
		} 
	}
	
	private static String getAuthorizationString(TreeMap<String, String> requestParameters) {
		TreeMap<String, String> allParameters = getConnectionParameters();
		// signature needs request parameters
		for(Map.Entry<String, String> e : requestParameters.entrySet()) {
			allParameters.put(e.getKey(), e.getValue());
		}
		
		String signature = getOauthSignature(allParameters);
		TreeMap<String, String> oauthParameters = getConnectionParameters();
		oauthParameters.put("oauth_signature", signature);
		
		StringBuilder builder = new StringBuilder();
		for(Map.Entry<String, String> e : oauthParameters.entrySet()) {
			builder.append(", ");
			builder.append(e.getKey());
			builder.append("=\"");
			try {
				builder.append(URLEncoder.encode(e.getValue(), "UTF-8"));
			} catch (UnsupportedEncodingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			builder.append("\"");
		}
		
		return "OAuth " + builder.substring(2);
	}

	private static ArrayList<String> fetchUsers(String endPointURL, TreeMap<String, String> requestParameters) throws IOException {
		HttpsURLConnection connection = null;
		
		try{
			URIBuilder uriBuilder =  new URIBuilder(endPointURL);
			for(Map.Entry<String, String> e : requestParameters.entrySet()) {
				uriBuilder.addParameter(e.getKey(), e.getValue());
			}
			URI uri = uriBuilder.build();
			URL url = new URL(uri.toString());
			
			connection = (HttpsURLConnection) url.openConnection();
			connection.setDoOutput(true);
			connection.setDoInput(true);
			connection.setRequestMethod("GET");
			connection.setRequestProperty("Host", "api.twitter.com");
	        connection.setRequestProperty("User-Agent", "SentimentTestSME");
		    connection.setRequestProperty("Authorization", getAuthorizationString(requestParameters));
		    connection.setUseCaches(false);
		    System.out.println(SearchEngineUtils.readResponse(connection));
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
		//String keyword = "Damon & Associates, Inc.";
		String keyword = "#betterment";
		int count = 5;
		TreeMap<String, String> requestParameters = new TreeMap<String, String>();
		requestParameters.put("q", keyword);
		requestParameters.put("count", Integer.toString(count));
		ArrayList<String> users = fetchUsers(SEARCH_URL, requestParameters);
		
		//for(String s : users) {
		//	System.out.println(s);
		//}
	}
}
