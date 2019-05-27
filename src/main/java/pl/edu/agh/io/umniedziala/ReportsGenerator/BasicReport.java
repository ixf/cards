package pl.edu.agh.io.umniedziala.ReportsGenerator;

import com.opencsv.CSVWriter;

import pl.edu.agh.io.umniedziala.model.ReportEntryEntity;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class BasicReport {

    File file;
    FileWriter outputFile;
    CSVWriter writer;
    private final String LONG_SPACE = "                           ";
    private final String MID_SPACE = "     ";
    private final String SHORT_SPACE = "  ";
    LocalDate from;
    LocalDate to;

    public BasicReport(String filePath) throws IOException {
        try {
            file = new File(filePath);
            outputFile = new FileWriter(file);
            writer = new CSVWriter(outputFile, '|');
            writeHeader();
        }
        catch(IOException e) {
            throw new IOException(e.getMessage());
        }
    }

    public BasicReport(LocalDate from, LocalDate to) throws IOException {
        try {
            String filePath = "./report_" + from.toString() + "_" + to.toString()+".csv";
            file = new File(filePath);
            outputFile = new FileWriter(file);
            writer = new CSVWriter(outputFile, '|');
            writeHeader();
            this.from = from;
            this.to = to;
        }
        catch(IOException e) {
            throw new IOException(e.getMessage());
        }
    }

    public void run() {
        writeToFile(from.toString(),to.toString());
        close();
    }

    public void writeHeader(){
        writeDashLine();
        writer.writeNext(String.format("%s WorkMonitor %s,%s REPORT",LONG_SPACE,SHORT_SPACE,SHORT_SPACE).split(","),false);
    }

     public void writeDashLine(){
        writer.writeNext("------------------------------------------------------------------------------------------".split(" "),false);
     }

     public void writeMetadata(){
        writeDashLine();
        writer.writeNext(("    application name ,     date     ,   total time  , active time ,  start time   ,   end time ").split(","),false);
        writeDashLine();
    }

    public void writeDateRangeOfReport(String from, String to){

        writeDashLine();
        writer.writeNext(String.format("%s REPORT RANGE  ,  from %s to  %s",LONG_SPACE,from,to).split(","),false);
    }

    public void addBlanks(String[] line){
        for(int i = 0; i<line.length;i++){
            String tmp = MID_SPACE;
            tmp += line[i];
            tmp += MID_SPACE;
            line[i] = tmp;
        }
    }


    public void writeToFile(String from, String to){
        List<String []> allRows = new ArrayList<>();

        writeDateRangeOfReport(from, to);
        writeMetadata();
        allRows = parseResultSet(ReportEntryEntity.getReportEntries(from,to));
        allRows.forEach(e -> addBlanks(e));
        allRows.forEach(e -> writer.writeNext(e, false));

    }
    public void close(){
        try {
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static List<String []> parseResultSet(ResultSet result){

        ReportEntryEntity tmp = new ReportEntryEntity();
        List<String []> allRows = new ArrayList<>();

        try {
            ResultSetMetaData resultmd = result.getMetaData();
            System.out.println(result.next());
            while(result.next()){

                String[] currentRow;
                tmp.setApplicationName(result.getString(result.findColumn("name")));
                tmp.setDate(result.getString(result.findColumn("start_time")).split(" ")[0]);
                tmp.setStartTime(result.getString(result.findColumn("start_time")).split(" ")[1]);
                tmp.setEndTime(result.getString(result.findColumn("end_time")).split(" ")[1]);

                SimpleDateFormat sdf = new SimpleDateFormat("hh:mm");
                Date firstDate = null;
                Date secondDate = null;
                try {
                    firstDate = sdf.parse(tmp.getStartTime());
                    secondDate = sdf.parse(tmp.getEndTime());

                } catch (ParseException e) {
                    e.printStackTrace();
                }

                long diffInMillies = Math.abs(secondDate.getTime() - firstDate.getTime());
                Long diff = TimeUnit.HOURS.convert(diffInMillies, TimeUnit.MILLISECONDS);

                tmp.setWorkingHours(diff.toString());
                tmp.setActiveWorkingHours("8.25");

                currentRow = tmp.toString().split(" ");
                allRows.add(currentRow);
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return allRows;
    }
}
