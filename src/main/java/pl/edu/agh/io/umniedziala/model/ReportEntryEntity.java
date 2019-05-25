package pl.edu.agh.io.umniedziala.model;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;


public class ReportEntryEntity {

    private String applicationName;
    private Date date;
    private Date startTime;
    private Date endTime;


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

    public Date getStartTime() {
        return startTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public Date getDate() {
        return date;
    }

    public String getApplicationName() {
        return applicationName;
    }

    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

}
