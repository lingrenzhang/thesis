package Gnip;

import java.io.*;
import java.net.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.zip.*;

import org.json.simple.*;
//import org.apache.commons.io.FileUtils;

// STEP 3
public class DownloadGnipJob {
    private static final String USERNAME = "lz7@stanford.edu";
    private static final String PASSWORD = "zlr195366";
    private static final String CHARSET = "UTF-8";
    
    private static ArrayList<String> getDownloadUrlList(String dataURL) throws IOException {
	       HttpURLConnection connection = null;
	       InputStream inputStream = null;
	       ArrayList<String> retval = new ArrayList<String>();
	       try {
	            connection = GnipTweetSearchLib.getConnection(dataURL, USERNAME, PASSWORD, "GET");
	            inputStream = connection.getInputStream();
	            
	            int responseCode = connection.getResponseCode();
//	            String responseMessage = connection.getResponseMessage();
	            if (responseCode >= 200 && responseCode <= 299) {
	            	BufferedReader reader = new BufferedReader(new InputStreamReader((inputStream), CHARSET));
	                String line = reader.readLine();
	                JSONObject obj = (JSONObject) JSONValue.parse(line);
	                JSONArray jsonUrlList = (JSONArray) obj.get("urlList");
	                int size = jsonUrlList.size();
	                for(int i = 0; i < size; i++) {
	                	retval.add(jsonUrlList.get(i).toString());
	                }
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
	        return retval;
	}
    
	private static ArrayList<String> getDataUrlList(String fileName) throws IOException {
		ArrayList<String> retval = new ArrayList<String>();
		BufferedReader reader = Files.newBufferedReader(Paths.get(fileName), Charset.forName("windows-1252"));
		String line = null;
		while((line = reader.readLine()) != null) {
			retval.add(line);
		}
		return retval;
	}
	
	private static String getFileLocation(String url) {
		String dir = "C:/Users/lz7/Desktop/Gnip Data/";
		int idx = url.indexOf("activities.json.gz");
		int offset1 = -17;
		int offsetDir = -4;
		int offset2 = 10;
		
		String path = dir + url.substring(idx + offset1, idx + offsetDir);
		File f = new File(path);
		if(!f.exists()){
			f.mkdirs();
		}
		String retval = dir + url.substring(idx + offset1, idx + offset2) + ".txt";
		return retval;
	}
	
	private static void downloadFileFromUrl(String url) throws IOException {
		URL fileURL = new URL(url);
		HttpURLConnection urlConnection = (HttpURLConnection) fileURL.openConnection();
		InputStream in  = new BufferedInputStream(urlConnection.getInputStream());
		GZIPInputStream gzipIn = new GZIPInputStream(in);
		OutputStream out = new FileOutputStream(getFileLocation(url));
		
		int read = 0;
		byte[] bytes = new byte[1024];
		while((read = gzipIn.read(bytes)) != -1) {
			out.write(bytes, 0, read);
		}
		gzipIn.close();
		out.close();
	}
	
	public static void main(String[] args) throws IOException  {
		ArrayList<String> dataUrlList = getDataUrlList("C:\\Users\\lz7\\Dropbox\\Default Prediction For Small Business Loans\\Java\\Twitter\\src\\Gnip\\batch10_data_url.txt");
		int size = dataUrlList.size();
		for(int i = 0; i < size; i++) {
			ArrayList<String> downloadUrlList = getDownloadUrlList(dataUrlList.get(i));
			int size2 = downloadUrlList.size();
			for(int j = 0; j < size2; j++) {
				if(j % 1000 == 0){
					System.out.println("" + i + " " + j);
				} 
				String fileUrl = downloadUrlList.get(j);
				try {
					downloadFileFromUrl(fileUrl);
				} catch (Exception e) {
					System.out.println(fileUrl);
				}
			}
		}
    }
}
