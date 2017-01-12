

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;

import org.apache.commons.codec.binary.Base64;


public class EncodingTest {
	private static final String CONSUMER_SECRET = "3TSYaSZ3cjKDYPQKhc5JjSux0c7Ym908wphx3ebEOBbn7coCiU";
	private static final String ACCESS_TOKEN_SECRET = "RiBLJ7R47eRutab5QlNdMmy42UdVjHtXrUiP3zjIUC0O2";

	public static void main(String[] args) throws InvalidKeyException, SignatureException, NoSuchAlgorithmException {
		String signatureBase = "GET&https%3A%2F%2Fapi.twitter.com%2F1.1%2Fsearch%2Ftweets.json&oauth_consumer_key%3DREUR3kn6BfEpsoxb8yVjS0EDH%26oauth_nonce%3Daf43f997f32b1cb35341ada3d937d9cc%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1421107932%26oauth_token%3D1656896509-ZB863KPYi4eMNJPmjCiD5NixLVxErrNIyBA7qu3%26oauth_version%3D1.0%26q%3D%2523betterment";
		String signingKey = CONSUMER_SECRET + "&" + ACCESS_TOKEN_SECRET;
		byte[] signature = SearchEngineUtils.calculateRFC2104HMAC(signatureBase, signingKey);
		System.out.println(new String(Base64.encodeBase64(signature)));
	}

}
