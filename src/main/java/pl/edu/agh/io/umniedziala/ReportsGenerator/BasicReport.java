package pl.edu.agh.io.umniedziala.ReportsGenerator;

import com.opencsv.CSVWriter;
import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class BasicReport {

    File file;
    FileWriter outputFile;
    CSVWriter writer;

    public BasicReport(String filePath) throws IOException {
        try {
            file = new File(filePath);
            outputFile = new FileWriter(file);
            writer = new CSVWriter(outputFile, ' ');
            writer.writeNext("This is header.".split(" "),false);
        }
        catch(IOException e) {
            throw new IOException(e.getMessage());
        }
    }

    public void writeToFile(){
        List<String []> allRows = new ArrayList<>();
        try {
            ResultSet result = QuerryExecutor.read(String.format("SELECT * FROM application"));
            ResultSetMetaData resultmd = result.getMetaData();
            int columnsNumber = resultmd.getColumnCount();
            while(result.next()){
                String[] currentRow = new String[columnsNumber];
                for (int i=1; i<=columnsNumber;i++) {
                      currentRow[i - 1] = result.getString(i);
                }
                allRows.add(currentRow);
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        //testing purposes only
        allRows.add("appName startTime endTime".split(" "));
        allRows.forEach(e -> writer.writeNext(e, false));

    }
    public void close(){
        try {
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
