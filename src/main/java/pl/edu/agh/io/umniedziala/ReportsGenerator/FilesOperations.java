package pl.edu.agh.io.umniedziala.ReportsGenerator;

import com.opencsv.CSVWriter;
import java.io.IOException;
import java.util.List;


public class FilesOperations {

    CSVWriter writer;

    public FilesOperations(CSVWriter writer) {
        this.writer = writer;
    }

    public void writeMetadata(String[] elements){
        writer.writeNext(elements,false);
    }

    public void writeDateRangeOfReport(String from, String to){
        writer.writeNext(String.format("REPORT RANGE,from,%s,to,%s",from,to).split(","),false);
    }

    public void close(){
        try {
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void writeToFile(List<String> entries){
        for(String s : entries){
            String[] tmp = s.split("#");
            writer.writeNext(tmp,false);
        }
    }
}
