package pl.edu.agh.io.umniedziala.activeApplicationMonitor;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class ApplicationRunningPeriodsManager {

    public ApplicationRunningPeriodsManager() {

        // when program is shutting down try to update database
        Runtime.getRuntime().addShutdownHook(new Thread(this::handleCurrentRunningPeriod));
    }

    private String lastApplicationCheckedName = null;

    private String startTime;

    public void handleApplicationRunningPeriod(String applicationName) {

        if (!applicationName.equals(lastApplicationCheckedName)) {
            handleCurrentRunningPeriod();

            if (isThisApplicationMonitored(applicationName)) {
                lastApplicationCheckedName = applicationName;

                startNewRunningPeriod();
            } else {
                lastApplicationCheckedName = null;
            }
        }

    }

    private void handleCurrentRunningPeriod() {
        if (lastApplicationCheckedName != null)
            insertRunningPeriodInDatabase();
    }

    private void insertRunningPeriodInDatabase() {
        try {
            int lastApplicationCheckedId = QuerryExecutor
                    .read(String.format("SELECT id FROM application WHERE name = '%s'", lastApplicationCheckedName))
                    .getInt("id");

            QuerryExecutor.createAndObtainId(
                    String.format(
                            "INSERT INTO running_period (start_time, end_time, application_id) " +
                                    "VALUES('%s', '%s', %d)"
                            , startTime, getCurrentDateTime(), lastApplicationCheckedId
                    )
            );

        } catch (SQLException e) {
            System.err.println("Couldnt create new running period");
            e.printStackTrace();
        }
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

    private void startNewRunningPeriod() {
        startTime = getCurrentDateTime();
    }


    private String getCurrentDateTime() {
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date date = new Date();
        return dateFormat.format(date);
    }
}
