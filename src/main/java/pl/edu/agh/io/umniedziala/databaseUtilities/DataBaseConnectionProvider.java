package pl.edu.agh.io.umniedziala.databaseUtilities;

import java.sql.*;
import java.util.Optional;

public class DataBaseConnectionProvider {

    private static final String JDBC_DRIVER = "org.sqlite.JDBC";
    private static final String JDBC_ADDRESS = "jdbc:sqlite:sample.db";
    private static Optional<Connection> connection = Optional.empty();

    static {
        init(JDBC_ADDRESS);
    }

    private static void init(String jdbcAddress) {
        try {
            close();
            Class.forName(JDBC_DRIVER);
            connection = Optional.of(DriverManager.getConnection(jdbcAddress));
        } catch (Exception e) {
            System.err.println("Couldnt initialize connection to database");
        }

        initDatabase();
    }

    private static void initDatabase() {
        try {
            QuerryExecutor.create("create table if not exists application (" +
                    "  id integer not null primary key autoincrement," +
                    "  name  varchar(100) not null," +
                    "  application_path text not null, " +
                    "  color integer" +
                    ");");
        } catch (SQLException e) {
            System.err.println("Couldnt create application table");
            e.printStackTrace();
        }

        try {
            QuerryExecutor.create("create table if not exists running_period(" +
                    "  id integer not null primary key autoincrement," +
                    "  start_time datetime not null," +
                    "  end_time datetime not null," +
                    "  application_id int not null" +
                    "    constraint application_id___fk" +
                    "    references application" +
                    ");");
        } catch (SQLException e) {
            System.err.println("Couldnt create table running_period");
            e.printStackTrace();
        }
    }

    private DataBaseConnectionProvider() {
        throw new UnsupportedOperationException();
    }


    
    public static Connection getConnection() {
        return connection.orElseThrow(() -> new RuntimeException("Connection is not valid."));
    }

    public static void close() throws SQLException {
        if (connection.isPresent()) {
            connection.get().close();
            connection = Optional.empty();
        }
    }

}
