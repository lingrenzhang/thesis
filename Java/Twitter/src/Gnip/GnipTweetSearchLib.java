package Gnip;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.*;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

import sun.misc.BASE64Encoder;

public class GnipTweetSearchLib {
    private static final String USERNAME = "lz7@stanford.edu";
    private static final String PASSWORD = "zlr195366";
    private static final String ACCOUNT_NAME = "StanfordResearch";
    private static final String CHARSET = "UTF-8";
    private static final String PUBLISHER = "twitter";
    private static final String STREAM_TYPE = "track";
    private static final String DATA_FORMAT = "activity-streams";
	private static final String STATUS_KEY = "status";

    private static final String SERVICE_USERNAME = "LingrenZhang"; // This is the Twitter username your company white listed with Gnip for access.

    
	public static String createHistoricalJob(String rules, String startMonth, String endMonth, String tag) throws IOException{
        String gnipURL = "https://historical.gnip.com/accounts/" + ACCOUNT_NAME + "/jobs.json";

        String fromDate = startMonth + "010001";
        String toDate = endMonth + "010001";
        String jobTitle = "Test" + "_" + tag + "_" + startMonth + "_" + endMonth;
        String jobData = "{\"publisher\":\"" + PUBLISHER + "\",\"streamType\":\"" + STREAM_TYPE + "\",\"dataFormat\":\"" + DATA_FORMAT + "\",\"fromDate\":\"" + fromDate + "\",\"toDate\":\"" + toDate + "\",\"title\":\"" + jobTitle + "\",\"serviceUsername\":\"" + SERVICE_USERNAME + "\",\"rules\": " + rules + "}";
        HttpURLConnection connection = null;
        InputStream inputStream = null;

        try {
            connection = GnipTweetSearchLib.getConnection(gnipURL, USERNAME, PASSWORD, "POST");
            connection.setDoOutput(true);
            connection.setRequestProperty("Accept-Charset", CHARSET);
            connection.setRequestProperty("Content-Type", "text/json");
            OutputStream output = null;
            try {
                 output = connection.getOutputStream();
                 output.write(jobData.getBytes(CHARSET));
            } finally {
                 if (output != null) try { output.close(); } catch (IOException logOrIgnore) {}
            }

            int responseCode = connection.getResponseCode();
//            String responseMessage = connection.getResponseMessage();
            if (responseCode >= 200 && responseCode <= 299) {
                
                  inputStream  = connection.getInputStream();

                  InputStreamReader reader = new InputStreamReader(inputStream);
                  BufferedReader bReader = new BufferedReader(reader);
                  String line = bReader.readLine();
                  JSONObject obj = (JSONObject) JSONValue.parse(line);
                  return obj.get("jobURL").toString();
            } else {
                 handleNonSuccessResponse(connection);
                 inputStream = connection.getErrorStream();
                 InputStreamReader reader2 = new InputStreamReader(inputStream);
                 BufferedReader bReader2 = new BufferedReader(reader2);
                 String line2 = bReader2.readLine();
                 while (line2 != null){
                      System.out.println(line2);
                      line2 = bReader2.readLine();
                 }
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (connection != null) {}
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
        return "";
	}
	

	
	public static String getJobStatus(String jobURL) throws IOException {
        HttpURLConnection connection = null;
        InputStream inputStream = null;

        try {
            connection = getConnection(jobURL, USERNAME, PASSWORD, "GET");

            inputStream = connection.getInputStream();
            int responseCode = connection.getResponseCode();

            if (responseCode >= 200 && responseCode <= 299) {

                BufferedReader reader = new BufferedReader(new InputStreamReader((inputStream), CHARSET));
                String line = reader.readLine();
                JSONObject obj = (JSONObject) JSONValue.parse(line);
                String status = obj.get(STATUS_KEY).toString();
                return status;
            } else {
                handleNonSuccessResponse(connection);
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (connection != null) {
                handleNonSuccessResponse(connection);
            }
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
        return "";
	}	
	
    public static void handleNonSuccessResponse(HttpURLConnection connection) throws IOException {
        int responseCode = connection.getResponseCode();
        String responseMessage = connection.getResponseMessage();
        System.out.println("Response Code: " + responseCode + " -- " + responseMessage);
    }

    public static HttpURLConnection getConnection(String urlString, String username, String password, String method) throws IOException {
        URL url = new URL(urlString);

        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod(method);
        connection.setReadTimeout(1000 * 60 * 60);
        connection.setConnectTimeout(1000 * 10);
        connection.setRequestProperty("Authorization", createAuthHeader(username, password));
        return connection;
    }

    private static String createAuthHeader(String username, String password) throws UnsupportedEncodingException {
        BASE64Encoder encoder = new BASE64Encoder();
        String authToken = username + ":" + password;
        return "Basic " + encoder.encode(authToken.getBytes());
    }

}
