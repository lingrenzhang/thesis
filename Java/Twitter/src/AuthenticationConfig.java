import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.codec.binary.Base64;


public class AuthenticationConfig {
	private String CONSUMER_KEY;
	private String CONSUMER_SECRET;
	private String ACCESS_TOKEN;
	private String ACCESS_TOKEN_SECRET;
	private String USER_AGENT;
	public static final String SEARCH_URL = "https://api.twitter.com/1.1/users/search.json";

	public AuthenticationConfig (int index) {
		CONSUMER_KEY = AuthenticationCredentials.CONSUMER_KEY_ARR[index];
		CONSUMER_SECRET = AuthenticationCredentials.CONSUMER_SECRET_ARR[index];
		ACCESS_TOKEN = AuthenticationCredentials.ACCESS_TOKEN_ARR[index];
		ACCESS_TOKEN_SECRET = AuthenticationCredentials.ACCESS_TOKEN_SECRET_ARR[index];
		USER_AGENT = AuthenticationCredentials.AGENT_ARR[index];
	}
	
	public String getUserAgent () {
		return USER_AGENT;
	}
	
	private TreeMap<String, String> getAuthenticationParameters(String nonce) {
		TreeMap<String, String> retval = new TreeMap<String, String>();
		retval.put("oauth_consumer_key", CONSUMER_KEY);
		retval.put("oauth_nonce", nonce);
		retval.put("oauth_signature_method", "HMAC-SHA1");
		int unixTime = (int) (System.currentTimeMillis() / 1000L);
		retval.put("oauth_timestamp", Integer.toString(unixTime));
		retval.put("oauth_token", ACCESS_TOKEN);
		retval.put("oauth_version", "1.0");
		return retval;
	}
	
	private static String getParameterString(TreeMap<String, String> parameters) throws UnsupportedEncodingException {
		StringBuilder builder = new StringBuilder();
		for(Map.Entry<String, String> e : parameters.entrySet()) {
//			builder.append("&" + URLEncoder.encode(e.getKey(), "UTF-8") + "=" + URLEncoder.encode(e.getValue(), "UTF-8"));
			builder.append("&" + SearchEngineUtils.percentEncode(e.getKey()) + "=" + SearchEngineUtils.percentEncode(e.getValue()));
		}
		return builder.substring(1);
	}
	
	private String getOauthSignature(TreeMap<String, String> parameters) {
		try {
			String parameterString = getParameterString(parameters);
			//System.out.println(parameterString);
			String signatureBaseString = "GET&" + URLEncoder.encode(SEARCH_URL, "UTF-8") + "&" + URLEncoder.encode(parameterString, "UTF-8");
			//System.out.println(signatureBaseString);
			String signingKey = CONSUMER_SECRET + "&" + ACCESS_TOKEN_SECRET;
			byte[] signature = SearchEngineUtils.calculateRFC2104HMAC(signatureBaseString, signingKey);
			return new String(Base64.encodeBase64(signature));
		} catch (UnsupportedEncodingException|SignatureException|NoSuchAlgorithmException|InvalidKeyException e) {
			e.printStackTrace();
			return new String();
		} 
	}
	
	public String getAuthorizationString(TreeMap<String, String> requestParameters) {
		String keyword = requestParameters.get("q");
		String nonce = SearchEngineUtils.randomString(keyword);
		
		TreeMap<String, String> allParameters = getAuthenticationParameters(nonce);
		// signature needs request parameters
		for(Map.Entry<String, String> e : requestParameters.entrySet()) {
			allParameters.put(e.getKey(), e.getValue());
		}
		
		String signature = getOauthSignature(allParameters);
		TreeMap<String, String> oauthParameters = getAuthenticationParameters(nonce);
		oauthParameters.put("oauth_signature", signature);
		
		StringBuilder builder = new StringBuilder();
		for(Map.Entry<String, String> e : oauthParameters.entrySet()) {
			builder.append(", ");
			builder.append(e.getKey());
			builder.append("=\"");
			try {
				builder.append(URLEncoder.encode(e.getValue(), "UTF-8"));
			} catch (UnsupportedEncodingException e1) {
				e1.printStackTrace();
			}
			builder.append("\"");
		}
		
		return "OAuth " + builder.substring(2);
	}

}
