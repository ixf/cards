package pl.edu.agh.io.umniedziala.databaseUtilities;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

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
