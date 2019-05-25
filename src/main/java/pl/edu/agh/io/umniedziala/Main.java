package pl.edu.agh.io.umniedziala;

import pl.edu.agh.io.umniedziala.ReportsGenerator.BasicReport;
import pl.edu.agh.io.umniedziala.activeApplicationMonitor.ActiveApplicationListener;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.Locale;

public class Main {
    public static void main(String[] args) {

        DateTimeFormatter germanFormatter = DateTimeFormatter.ofLocalizedDate(
                FormatStyle.MEDIUM).withLocale(Locale.GERMAN);


//        testing purposes insertion into DB
//        ApplicationEntity.create("idea64.exe","C:\\Program Files\\JetBrains\\IntelliJ ",4);
//        RunningPeriodEntity.create("2019-01-01 17:12","2019-01-01 22:34",1);
//
//        ApplicationEntity.create("chrome.exe","C:\\Program Files\\Google\\Chrome ",4);
//        RunningPeriodEntity.create("2019-01-02 13:32","2019-01-03 20:17",2);
//
//         testing purposes dates
        LocalDate from = LocalDate.parse("22.05.2019", germanFormatter);
        LocalDate to = LocalDate.parse("24.05.2019", germanFormatter);

        try {
            BasicReport report = new BasicReport(from,to);  // generate report
            //report.createReportWithApps();  // or
            report.createReportWithoutApps();
        } catch (IOException e) {
            e.printStackTrace();
        }

        new ActiveApplicationListener().start();


    }
}
