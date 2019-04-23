package pl.edu.agh.io.umniedziala.activeApplicationMonitor;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class ApplicationRunningPeriodsManager {

    public ApplicationRunningPeriodsManager() { }


    private String lastApplicationCheckedName = null;
    private int lastRunningPeriodId;

    public void handleApplicationRunningPeriod(String applicationName) {

        if (isThisApplicationMonitored(applicationName)) {
            if (applicationName.equals(lastApplicationCheckedName)) {
                updateCurrentRunningPeriod();
            } else {
                lastApplicationCheckedName = applicationName;

                createNewRunningPeriod();
            }
        } else {
            lastApplicationCheckedName = null;
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

    private void updateCurrentRunningPeriod() {
        try {
            QuerryExecutor
                    .update(String.format("UPDATE running_period "
                        + "SET end_time = '%s' "
                        + "WHERE id = %d"
                            , getCurrentDateTime(), lastRunningPeriodId)
                    );
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }


    private void createNewRunningPeriod() {
        try {
            int lastApplicationCheckedId = QuerryExecutor
                    .read(String.format("SELECT id FROM application WHERE name = '%s'", lastApplicationCheckedName))
                    .getInt("id");

            lastRunningPeriodId = QuerryExecutor.createAndObtainId(
                    String.format(
                            "INSERT INTO running_period (start_time, end_time, application_id) " +
                            "VALUES('%s', '%s', %d)"
                            , getCurrentDateTime(), getCurrentDateTime(), lastApplicationCheckedId
                    )
            );

        } catch (SQLException e) {
            System.err.println("Couldnt create new running period");
            e.printStackTrace();
        }
    }


    private String getCurrentDateTime() {
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date date = new Date();
        return dateFormat.format(date);
    }
}
