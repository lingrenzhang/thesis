package Facebook;
import java.io.*;
import java.util.*;

import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.*;

public class FBExcelUtil {
	private static int getColIdx(XSSFSheet sheet, String colName) throws IOException {
		Row header = sheet.getRow(0);
		Iterator<Cell> iter = header.cellIterator();
		int counter = 0;
		while(iter.hasNext()) {
			Cell c  = iter.next();
			if(c.toString().compareTo(colName) == 0){
				return counter;
			}
			counter++;
		}
		throw new IOException("Unknown column name: " + colName);
	}
	
	public static String preprocessKeyword(String keyword) {
		String[] parts = keyword.split(",");
		String retval = keyword;
		if(parts.length > 1) {
			retval = retval.substring(0, keyword.length() - parts[parts.length - 1].length() - 1);
		}

		String[] postfixToIgnore = new String[]{" inc", " inc.", " llc", " llc."};
		for(String toIgnore : postfixToIgnore) {
			if(retval.length() < toIgnore.length()) continue;
			String postfix = retval.substring(retval.length() - toIgnore.length());
			if(postfix.equalsIgnoreCase(toIgnore)) {
				retval = retval.substring(0, retval.length() - toIgnore.length());
			}
		}
		return retval;
	}
	
	public static ArrayList<String> readColumn(String fileName, String colName, int startRow, int rowCount) throws IOException {
		ArrayList<String> retval = new ArrayList<String>();
		FileInputStream fis = new FileInputStream(new File(fileName));
		XSSFWorkbook workbook = new XSSFWorkbook(fis);
		XSSFSheet sheet = workbook.getSheetAt(0);
		int colIdx = getColIdx(sheet, colName);
		for(int rowIdx = startRow; rowIdx < startRow + rowCount; rowIdx++) {
			retval.add(sheet.getRow(rowIdx).getCell(colIdx).toString());
		}
		workbook.close();
		fis.close();
		return retval;
	}
	
	public static void writeColumn(String fileName, String colName, int startRow, ArrayList<String> toWrite) {
	}
}
