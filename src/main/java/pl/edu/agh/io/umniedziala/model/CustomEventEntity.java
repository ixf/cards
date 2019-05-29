package pl.edu.agh.io.umniedziala.model;

import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class CustomEventEntity extends Period {
    public static final String TABLE_NAME = "custom_event";

    protected String name;
    protected String description;
    protected String color;

    public CustomEventEntity(final int id,
                             final String startTime,
                             final String endTime,
                             final String name,
                             final String description,
                             final String color) {
        this.id = id;
        this.startTime = startTime;
        this.endTime = endTime;
        this.name = name;
        this.description = description;
        this.color = color;
    }

    public static Optional<CustomEventEntity> create(final String startTime, final String endTime, final String name) {
        return create(startTime, endTime, name, "No description", "#000000");
    }

    public static Optional<CustomEventEntity> create(final String startTime,
                                                     final String endTime,
                                                     final String name,
                                                     final String description,
                                                     final String color) {
        String insertSql = String.format(
                "INSERT INTO %s (%s, %s, %s, %s, %s) VALUES ('%s', '%s', '%s', '%s', '%s')"
                , TABLE_NAME, Columns.START_TIME, Columns.END_TIME, Columns.NAME, Columns.DESCRIPTION, Columns.COLOR
                , startTime, endTime, name, description, color
        );

        int id = 0;

        try {
            id = QuerryExecutor.createAndObtainId(insertSql);
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return CustomEventEntity.findById(id);
    }

    public static void update(final int id, final String startTime, final String endTime, final String name, final String description, final String color) {
        String setString = getSetString(startTime, endTime, name, description, color);

        if (setString == null) {
            System.err.println("Wrong update");
        } else {
            String updateSql = String.format(
                    "UPDATE %s SET %s WHERE %s = %d"
                    , TABLE_NAME, getSetString(startTime, endTime, name, description, color), Columns.ID, id
            );

            try {
                QuerryExecutor.update(updateSql);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
    }

    private static String getSetString(String startTime, String endTime, String name, String description, String color) {
        StringBuilder setString = new StringBuilder();

        if (startTime != null)
            setString.append(Columns.START_TIME + " = '" + startTime + "', ");
        if (endTime != null)
            setString.append(Columns.END_TIME + " = '" + endTime + "', ");
        if (name != null)
            setString.append(Columns.NAME + " = '" + name + "', ");
        if (description != null)
            setString.append(Columns.DESCRIPTION + " = '" + description + "', ");
        if (color != null)
            setString.append(Columns.COLOR + " = '" + color + "', ");

        return setString.toString().equals("") ? null : setString.replace(setString.lastIndexOf(","), setString.lastIndexOf(",") + 1," ").toString();
    }


    public static Optional<CustomEventEntity> findById(final  int id) {
        String findByIdSql = String.format("SELECT * FROM %s WHERE %s = %s", TABLE_NAME, Columns.ID, id);

        try {
            ResultSet rs = QuerryExecutor.read(findByIdSql);
            return returnCustomEvent(rs);
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    public static List<CustomEventEntity> findByStartDate(final String startDate) {
        String findByStartDateSql = String.format(
                "SELECT * FROM %s " +
                        "WHERE %s >= Datetime('%s 00:00:00') and %s <= Datetime('%s 23:59:59') "
                , TABLE_NAME
                , Columns.START_TIME, startDate, Columns.START_TIME, startDate
        );

        return getResultList(findByStartDateSql);
    }

    public static List<CustomEventEntity> findByName(final String name) {
        String findByNameSql = String.format(
                "SELECT * FROM %s WHERE %s = '%s'"
                , TABLE_NAME, Columns.NAME, name
        );

        return getResultList(findByNameSql);
    }

    private static List<CustomEventEntity> getResultList(String sqlQuerry) {
        Optional<ResultSet> rs = Optional.empty();
        try {
            rs = Optional.of(QuerryExecutor.read(sqlQuerry));
        } catch (SQLException e) {
            e.printStackTrace();
        }

        List<CustomEventEntity> resultList = new ArrayList<>();
        rs.ifPresent(resultSet -> {
            try {
                while (resultSet.next()) {
                    resultList.add(new CustomEventEntity(
                            resultSet.getInt(Columns.ID),
                            resultSet.getString(Columns.START_TIME),
                            resultSet.getString(Columns.END_TIME),
                            resultSet.getString(Columns.NAME),
                            resultSet.getString(Columns.DESCRIPTION),
                            resultSet.getString(Columns.COLOR)
                    ));
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }
        });

        return resultList;
    }


    private static Optional<CustomEventEntity> returnCustomEvent(ResultSet rs) {
        try {
            return Optional.of(new CustomEventEntity(
                    rs.getInt(Columns.ID),
                    rs.getString(Columns.START_TIME),
                    rs.getString(Columns.END_TIME),
                    rs.getString(Columns.NAME),
                    rs.getString(Columns.DESCRIPTION),
                    rs.getString(Columns.COLOR)
            ));
        } catch (SQLException e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }


    public String getName() {
        return name;
    }
    public String getDescription() {
        return description;
    }
    public String getColor() {
        return color;
    }


    public static class Columns {
        public static final String ID = "id";
        public static final String NAME = "name";
        public static final String START_TIME = "start_time";
        public static final String END_TIME = "end_time";
        public static final String DESCRIPTION = "description";
        public static final String COLOR = "color";
    }

}
