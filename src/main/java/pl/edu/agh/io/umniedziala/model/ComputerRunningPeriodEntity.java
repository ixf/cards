package pl.edu.agh.io.umniedziala.model;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class ComputerRunningPeriodEntity {

    public static final String TABLE_NAME = "computer_running_period";

    private final int id;
    private final String startTime;
    private final String endTime;

    public ComputerRunningPeriodEntity(final int id, final String startTime, final String endTime) {
        this.id = id;
        this.startTime = startTime;
        this.endTime = endTime;
    }

    public static Optional<ComputerRunningPeriodEntity> create(final String startTime, final String endTime) {
        String insertSql = String.format(
                "INSERT INTO %s (%s, %s) VALUES ('%s', '%s')"
                , TABLE_NAME, Columns.START_TIME, Columns.END_TIME
                , startTime, endTime
        );

        int id = 0;

        try {
            id = QuerryExecutor.createAndObtainId(insertSql);
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return ComputerRunningPeriodEntity.findById(id);
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

    public static Optional<ComputerRunningPeriodEntity> findById(final int id) {
        String findByIdSql = String.format("SELECT * FROM %s WHERE %s = %d", TABLE_NAME, Columns.ID, id);

        try {
            ResultSet rs = QuerryExecutor.read(findByIdSql);
            return returnRunningPeriod(rs);
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    public static List<ComputerRunningPeriodEntity> findByStartDate(final String startDate) {
        String findByStartDateSql = String.format(
                "SELECT * FROM %s " +
                        "WHERE %s >= Datetime('%s 00:00:00') and %s <= Datetime('%s 23:59:59')"
                , TABLE_NAME
                , Columns.START_TIME, startDate
                , Columns.START_TIME, startDate
        );

        Optional<ResultSet> rs = Optional.empty();
        try {
            rs = Optional.of(QuerryExecutor.read(findByStartDateSql));
        } catch (SQLException e) {
            e.printStackTrace();
        }

        List<ComputerRunningPeriodEntity> resutList = new ArrayList<>();
        if (rs.isPresent()) {
            try {
                while (rs.get().next()) {
                    resutList.add(new ComputerRunningPeriodEntity(
                            rs.get().getInt(Columns.ID),
                            rs.get().getString(Columns.START_TIME),
                            rs.get().getString(Columns.END_TIME)
                    ));
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }

        }

        return resutList;
    }

    private static Optional<ComputerRunningPeriodEntity> returnRunningPeriod(ResultSet rs) {
        try {
            return Optional.of(new ComputerRunningPeriodEntity(
                    rs.getInt(Columns.ID),
                    rs.getString(Columns.START_TIME),
                    rs.getString(Columns.END_TIME)
            ));
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    public int getId() {
        return id;
    }

    public String getStartTime() {
        return startTime;
    }

    public String getEndTime() {
        return endTime;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ComputerRunningPeriodEntity) {
            return
                    this.startTime.equals(((ComputerRunningPeriodEntity) obj).startTime)
                    && this.endTime.equals(((ComputerRunningPeriodEntity) obj).endTime);
        }

        return super.equals(obj);
    }

    public static class Columns {
        public static final String ID = "id";
        public static final String START_TIME = "start_time";
        public static final String END_TIME = "end_time";
    }
}
