package pl.edu.agh.io.umniedziala.databaseUtilities;

import pl.edu.agh.io.umniedziala.model.RunningPeriodEntity;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class QuerryExecutor {

    public static int createAndObtainId(final String insertSql) throws SQLException {
        try (final PreparedStatement statement = DataBaseConnectionProvider.getConnection().prepareStatement(insertSql, Statement.RETURN_GENERATED_KEYS)) {
            statement.execute();
            try (final ResultSet resultSet = statement.getGeneratedKeys()) {
                return readIdFromResultSet(resultSet);
            }
        }
    }

    public static void update(final String updateSql) throws SQLException {
        try (final PreparedStatement statement = DataBaseConnectionProvider.getConnection().prepareStatement(updateSql)) {
            statement.execute();
        }
    }

    private static int readIdFromResultSet(final ResultSet resultSet) throws SQLException {
        return resultSet.next() ? resultSet.getInt(1) : -1;
    }

    public static ResultSet read(final String sql) throws SQLException {
        final Statement statement = DataBaseConnectionProvider.getConnection().createStatement();
        return statement.executeQuery(sql);
    }

    public static Map<Integer, String> getAppNames() throws SQLException {
        ResultSet resultSet = QuerryExecutor.read("SELECT * FROM application");
        Map<Integer, String> names = new HashMap<>();
        while (resultSet.next()) {
            names.put(resultSet.getInt("id"), resultSet.getString("name"));
        }
        resultSet.close();
        return names;
    }

    public static List<RunningPeriodEntity> getPeriodsForDay(java.util.Date date) throws SQLException {
        LocalDate today = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        java.sql.Date dateAtStart = java.sql.Date.valueOf(today);
        java.sql.Date dateAtEnd = java.sql.Date.valueOf(today.plusDays(1));

        String sql = "SELECT * FROM running_period WHERE start_time > ? AND end_time < ?";

        ResultSet resultSet;
        try (final PreparedStatement statement = DataBaseConnectionProvider.getConnection().prepareStatement(sql)) {
            statement.setString(1, dateAtStart.toString());
            statement.setString(2, dateAtEnd.toString());

            resultSet = statement.executeQuery();

            List<RunningPeriodEntity> periods = new ArrayList<>();
            while (resultSet.next()) {
                RunningPeriodEntity period = RunningPeriodEntity.returnRunningPeriod(resultSet).orElseThrow(() -> new RuntimeException("Couldn't read RunningPeriodEntity from resultSet"));
                periods.add( period );
            }
            resultSet.close();

            return periods;
        }
    }

    public static void create(final String insertSql) throws SQLException {
        try (final PreparedStatement statement = DataBaseConnectionProvider.getConnection().prepareStatement(insertSql)) {
            statement.execute();
        }
    }

    public static void delete(final String deleteSql) throws SQLException {
        try (final PreparedStatement statement = DataBaseConnectionProvider.getConnection().prepareStatement(deleteSql)) {
            statement.execute();
        }
    }

}
