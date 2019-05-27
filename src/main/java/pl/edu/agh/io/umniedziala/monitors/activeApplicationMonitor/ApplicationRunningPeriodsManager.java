package pl.edu.agh.io.umniedziala.monitors.activeApplicationMonitor;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;
import pl.edu.agh.io.umniedziala.model.ApplicationEntity;
import pl.edu.agh.io.umniedziala.model.RunningPeriodEntity;

import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class ApplicationRunningPeriodsManager {

    public ApplicationRunningPeriodsManager() {

        // when program is shutting down try to update database
        Runtime.getRuntime().addShutdownHook(new Thread(this::handleCurrentRunningPeriod));
    }

    private String lastApplicationCheckedName = null;   // if null => dont update | if not null => update

    private String startTime;
    private int lastRunningPeriodId;

    public void handleApplicationRunningPeriod(String applicationName) {

        if(isThisApplicationMonitored((applicationName))) {
            if (applicationName.equals(lastApplicationCheckedName)) {
                // update record
                updateRunningPeriodInDatabase();
            } else {
                // add new record
                lastApplicationCheckedName = applicationName;
                startNewRunningPeriod();
            }
        } else {
            handleCurrentRunningPeriod();
            lastApplicationCheckedName = null;
        }
    }

    private void handleCurrentRunningPeriod() {
        if (lastApplicationCheckedName != null)
            updateRunningPeriodInDatabase();
    }

    private void startNewRunningPeriod() {
        startTime = getCurrentDateTime();

        int applicationId = ApplicationEntity.findByName(lastApplicationCheckedName).get().getId();

        lastRunningPeriodId = RunningPeriodEntity.create(startTime, startTime, applicationId).get().getId();
    }

    private void updateRunningPeriodInDatabase() {
        RunningPeriodEntity.update(lastRunningPeriodId, startTime, getCurrentDateTime());
    }


    private boolean isThisApplicationMonitored(String applicationName) {
        try {
            return QuerryExecutor
                    .read(String.format("SELECT * FROM application WHERE name = '%s'", applicationName))
                    .next();
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return false;
    }

    private String getCurrentDateTime() {
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date date = new Date();
        return dateFormat.format(date);
    }
}