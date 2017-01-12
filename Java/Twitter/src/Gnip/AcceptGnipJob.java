package Gnip;

import java.io.*;
import java.net.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;
// STEP 2
public class AcceptGnipJob {
    private static final String USERNAME = "lz7@stanford.edu";
    private static final String PASSWORD = "zlr195366";
    private static final String CHARSET = "UTF-8";

	private static boolean acceptJob(String jobURL) throws IOException {
	    String choice = "accept"; // Switch this to 'reject' if you want to reject the job.
        String putData = String.format("{\"status\":\"%s\"}", java.net.URLEncoder.encode(choice, CHARSET));

        HttpURLConnection connection = null;
        InputStream inputStream = null;

        try {
            connection = GnipTweetSearchLib.getConnection(jobURL, USERNAME, PASSWORD, "PUT");
            connection.setDoOutput(true);
            connection.setRequestProperty("Accept-Charset", CHARSET);
            connection.setRequestProperty("Content-Type", "text/json");
            OutputStream output = null;
            try {
                 output = connection.getOutputStream();
                 output.write(putData.getBytes(CHARSET));
            } finally {
                 if (output != null) try { output.close(); } catch (IOException logOrIgnore) {}
            }

            int responseCode = connection.getResponseCode();
            String responseMessage = connection.getResponseMessage();
            if (responseCode >= 200 && responseCode <= 299) {
            	return true;
            } else {
            	 System.out.println(connection.getURL().toString());
            	 GnipTweetSearchLib.handleNonSuccessResponse(connection);
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
        return false;
	}	
	
	private static String getStatus(String jobURL) throws IOException {
	       HttpURLConnection connection = null;
	        InputStream inputStream = null;

	        try {
	            connection = GnipTweetSearchLib.getConnection(jobURL, USERNAME, PASSWORD, "GET");
	            inputStream = connection.getInputStream();
	            
	            int responseCode = connection.getResponseCode();
	            String responseMessage = connection.getResponseMessage();
	            if (responseCode >= 200 && responseCode <= 299) {
	            	BufferedReader reader = new BufferedReader(new InputStreamReader((inputStream), CHARSET));
	                String line = reader.readLine();
	                return line;
	            } else {
	            	 GnipTweetSearchLib.handleNonSuccessResponse(connection);
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
	        return "Status Error";
	}
	
	private static ArrayList<String> getUrlList(String fileName) throws IOException {
		ArrayList<String> retval = new ArrayList<String>();
		BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("windows-1252"));
		String line = null;
		while((line = reader.readLine()) != null) {
			retval.add(line);
		}
		return retval;	}
	
	public static void main(String[] args) throws IOException  {
		ArrayList<String> urlList = getUrlList("C:\\Users\\lz7\\Dropbox\\Default Prediction For Small Business Loans\\Java\\Twitter\\src\\Gnip\\batch10_submit.txt");
		int size = urlList.size();
		for(int i = 0; i < size; i++) {
			boolean accepted = acceptJob(urlList.get(i));
			System.out.println(accepted);
			//String status = getStatus(urlList.get(i));
			//System.out.println(status);
		}
    }
}
