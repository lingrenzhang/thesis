package Facebook;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;
import java.util.Formatter;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import javax.net.ssl.HttpsURLConnection;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.binary.Hex;


public class FBSearchEngineUtils {
	private static final String HMAC_SHA1_ALGORITHM = "HmacSHA1";
	
	// Writes a request to a connection
	public static boolean writeRequest(HttpsURLConnection connection, String textBody) {
		try {
			BufferedWriter wr = new BufferedWriter(new OutputStreamWriter(connection.getOutputStream()));
			wr.write(textBody);
			wr.flush();
			wr.close();
				
			return true;
		}
		catch (IOException e) { return false; }
	}
		
	// Reads a response for a given connection and returns it as a string.
	public static String readResponse(HttpsURLConnection connection) throws IOException {
		//try {
		StringBuilder str = new StringBuilder();
		
		BufferedReader br = new BufferedReader(new InputStreamReader(connection.getInputStream()));
		String line = "";
		while((line = br.readLine()) != null) {
			str.append(line + System.getProperty("line.separator"));
		}
		return str.toString();
		//}
		//catch (IOException e) { 
			//e.printStackTrace();
		//	StringBuilder str = new StringBuilder();
		//	BufferedReader br = new BufferedReader(new InputStreamReader(connection.getErrorStream()));
		//	String line = "";
		//	try {
		//		while((line = br.readLine()) != null) {
		//			str.append(line + System.getProperty("line.separator"));
		//		}
		//	} catch (IOException e1) {
				// TODO Auto-generated catch block
		//		e1.printStackTrace();
		//	}
		//	System.out.println(str.toString());
		//	return new String();
		//}
	}
	
	public static String encodeKeys(String consumerKey, String consumerSecret) {
		try {
			String encodedConsumerKey = URLEncoder.encode(consumerKey, "UTF-8");
			String encodedConsumerSecret = URLEncoder.encode(consumerSecret, "UTF-8");
			String fullKey = encodedConsumerKey + ":" + encodedConsumerSecret;
			String encodedCredentials = new String(Base64.encodeBase64(fullKey.getBytes())); 
			return encodedCredentials;
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			return new String();
		}
	}
	
	private static String toHexString(byte[] bytes) {
		Formatter formatter = new Formatter();
		
		for (byte b : bytes) {
			formatter.format("%02x", b);
		}
		
		String retval = formatter.toString();
		formatter.close();
		return retval;
	}
 
	public static byte[] calculateRFC2104HMAC(String data, String key)
		throws SignatureException, NoSuchAlgorithmException, InvalidKeyException
	{
		SecretKeySpec signingKey = new SecretKeySpec(key.getBytes(), HMAC_SHA1_ALGORITHM);
		Mac mac = Mac.getInstance(HMAC_SHA1_ALGORITHM);
		mac.init(signingKey);
		return mac.doFinal(data.getBytes());
	}
	public static String percentEncode(String string) {
		try {
			return URLEncoder.encode(string, "UTF-8").replaceAll("\\+", "%20");
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return new String();
		}
	}
	public static String randomString(String key) {
		int outputLength = 48;
		int inputLength = outputLength / 2;
		String formattedKey = (key.length() >= inputLength) ? key.substring(0, inputLength) : String.format("%-" + inputLength + "s", key);
		String hexString = Hex.encodeHexString(formattedKey.getBytes());
		if(hexString.length() != outputLength) {
			System.out.println(String.format("Expected random string of length %d, got random string of length %d!", outputLength, hexString.length()));
			//throw new IOException("randomString function is not working");
		}
		return hexString;
	}
}
