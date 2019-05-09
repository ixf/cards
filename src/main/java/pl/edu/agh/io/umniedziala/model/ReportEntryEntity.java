package pl.edu.agh.io.umniedziala.model;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.sql.ResultSet;
import java.sql.SQLException;


public class ReportEntryEntity {

    private final int NUMBER_OF_DATE_COLUMN = 2;

    private String applicationName;
    private String date;
    private String workingHours;
    private String activeWorkingHours;
    private String startTime;
    private String endTime;


    public ReportEntryEntity() {}

    public static ResultSet getReportEntries(String from, String to){
        ResultSet result = null;
        try {
             result = QuerryExecutor.read(String.format("SELECT name, end_time, start_time FROM application join running_period " +
                    "on application.id = running_period.application_id " +
                     "where DATE(start_time) >= '%s' and Date(end_time) <= '%s'",from,to));
             return result;
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return result;
    }

    public String getStartTime() {
        return startTime;
    }

    public String getEndTime() {
        return endTime;
    }

    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public void setWorkingHours(String workingHours) {
        this.workingHours = workingHours;
    }

    public void setActiveWorkingHours(String activeWorkingHours) {
        this.activeWorkingHours = activeWorkingHours;
    }

    public void setStartTime(String startTime) {
        this.startTime = startTime;
    }

    public void setEndTime(String endTime) {
        this.endTime = endTime;
    }

    @Override
    public String toString() {
        return String.format("%s %s %s %s %s %s",applicationName,date,workingHours,activeWorkingHours,startTime,endTime);
    }
}
