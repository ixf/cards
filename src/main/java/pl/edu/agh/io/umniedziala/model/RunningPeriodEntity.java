package pl.edu.agh.io.umniedziala.model;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class RunningPeriodEntity extends Period {

    public static final String TABLE_NAME = "running_period";

    protected int applicationId;

    public RunningPeriodEntity(final int id, final String startTime, final String endTime, final int applicationId) {
        this.id = id;
        this.startTime = startTime;
        this.endTime = endTime;
        this.applicationId = applicationId;
    }

    public static Optional<RunningPeriodEntity> create(final String startTime, final String endTime, final int applicationId) {
        String insertSql = String.format(
                "INSERT INTO %s (%s, %s, %s) VALUES ('%s', '%s', %d)"
                , TABLE_NAME, Columns.START_TIME, Columns.END_TIME, Columns.APPLICATION_ID
                , startTime, endTime, applicationId
        );

        int id = 0;

        try {
            id = QuerryExecutor.createAndObtainId(insertSql);
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return RunningPeriodEntity.findById(id);
    }


    public static void update(final int id, final String startTime, final String endTime) {
        String updateSql = String.format(
                "UPDATE %s SET %s = '%s', %s = '%s' WHERE %s = %d"
                , TABLE_NAME
                , Columns.START_TIME, startTime
                , Columns.END_TIME, endTime
                , Columns.ID, id
        );

        try {
            QuerryExecutor.update(updateSql);
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    public static Optional<RunningPeriodEntity> findById(final int id) {
        String findByIdSql = String.format("SELECT * FROM %s WHERE %s = %s", TABLE_NAME, Columns.ID, id);

        try {
            ResultSet rs = QuerryExecutor.read(findByIdSql);
            return returnRunningPeriod(rs);
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    public static List<RunningPeriodEntity> findByStartDate(final String startDate, final String appName) {
        String findByStartDateSql = String.format(
                "SELECT * FROM %s " +
                        "INNER JOIN %s ON %s.%s = %s.%s " +
                        "WHERE %s >= Datetime('%s 00:00:00') and %s <= Datetime('%s 23:59:59') " +
                        "and %s = '%s'"
                , TABLE_NAME
                , ApplicationEntity.TABLE_NAME, ApplicationEntity.TABLE_NAME, ApplicationEntity.Columns.ID
                , TABLE_NAME, Columns.APPLICATION_ID
                , Columns.START_TIME, startDate
                , Columns.START_TIME, startDate
                , ApplicationEntity.Columns.NAME, appName
        );

        Optional<ResultSet> rs = Optional.empty();
        try {
            rs = Optional.of(QuerryExecutor.read(findByStartDateSql));
        } catch (SQLException e) {
            e.printStackTrace();
        }

        List<RunningPeriodEntity> resutList = new ArrayList<>();
        if (rs.isPresent()) {
            try {
                while (rs.get().next()) {
                    resutList.add(new RunningPeriodEntity(
                            rs.get().getInt(Columns.ID),
                            rs.get().getString(Columns.START_TIME),
                            rs.get().getString(Columns.END_TIME),
                            rs.get().getInt(Columns.APPLICATION_ID)
                    ));
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }

        }

        return resutList;
    }



    public static Optional<RunningPeriodEntity> returnRunningPeriod(ResultSet rs) {
        try {
            return Optional.of(new RunningPeriodEntity(
                    rs.getInt(Columns.ID),
                    rs.getString(Columns.START_TIME),
                    rs.getString(Columns.END_TIME),
                    rs.getInt(Columns.APPLICATION_ID)
            ));
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    public int getApplicationId() {
        return applicationId;
    }

    @Override
    public String getColor() {
        Optional<ApplicationEntity> app = ApplicationEntity.findById(this.applicationId);

        String color;
        if (app.isPresent())
            color = app.get().color;
        else {
            System.err.println("Application not found, setting color as #FFFFFF");
            color = "#FFFFFF";
        }

        return color;
    }


    public static class Columns {
        public static final String ID = "id";
        public static final String START_TIME = "start_time";
        public static final String END_TIME = "end_time";
        public static final String APPLICATION_ID = "application_id";
    }
}
