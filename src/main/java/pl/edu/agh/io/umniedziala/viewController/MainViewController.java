/**
 * Created by kuba on 06/04/2019
 */

package pl.edu.agh.io.umniedziala.viewController;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.chart.CategoryAxis;
import javafx.scene.chart.NumberAxis;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;
import pl.edu.agh.io.umniedziala.model.ComputerRunningPeriodEntity;
import pl.edu.agh.io.umniedziala.model.CustomEventEntity;
import pl.edu.agh.io.umniedziala.model.Period;
import pl.edu.agh.io.umniedziala.model.RunningPeriodEntity;
import pl.edu.agh.io.umniedziala.view.TimeChart;
import sun.java2d.pipe.SpanShapeRenderer;

import java.io.File;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

public class MainViewController {
    private AppController appController;
    DateFormat dateFormat = new SimpleDateFormat("EEEE, dd.MM.yyyy");
    final FileChooser fileChooser = new FileChooser();
    ManagingApplicationsController managingApplicationsController;
    final static String DEFAULT_COLOR = "#000000";

    private Date currentDate;

    @FXML
    private Label activity;

    @FXML
    private Pane menu;

    @FXML
    private Pane border;

    @FXML
    private ImageView logo;

    @FXML
    private Label app_name;

    @FXML
    private Text date;

    @FXML
    private ImageView left_date;

    @FXML
    private ImageView right_date;

    @FXML
    private TimeChart activity_chart;

    @FXML
    private CategoryAxis app_axis;

    @FXML
    private NumberAxis time_axis;

    @FXML
    private MenuItem appButton;

    @FXML
    private MenuItem reportButton;

    @FXML
    private MenuItem settingsButton;

    @FXML
    private MenuButton menuButton;

    @FXML
    private ImageView menuView;

    @FXML
    private MenuItem eventButton;

    @FXML
    public void initialize() {
        currentDate = new Date();
        date.setText(dateFormat.format(currentDate));
        managingApplicationsController = new ManagingApplicationsController();

        startTimechartUpdates();
    }

    private void startTimechartUpdates() {
        // Nasz timechart jest szeroki a bazę aktualizujemy często. Nie ma chyba potrzeby, żeby aktualizować
        // wykresy przy każdej zmianie w bazie. Uruchamiam tutaj timer, który co kilka minut aktualizuje wykres.

        long repeatTime = 1 * 15 * 1000; // w milisekundach
        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                Platform.runLater(() -> {
                    try {
                        addTrackedAppsToTimechart();
                        loadExistingDataToTimechart(currentDate);
                    } catch (SQLException e) {
                        // TODO jakiś ładny alert
                        e.printStackTrace();
                    }
                });
            }
        }, 0, repeatTime);
    }

    public void setAppController(AppController appController) {
        this.appController = appController;
    }

    private void loadExistingDataToTimechart(Date date) throws SQLException {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        String sDate = sdf.format(date);
        List<Period> results = QuerryExecutor.getPeriodsForDay(date);
        results.addAll(ComputerRunningPeriodEntity.findByStartDate(sDate));
        results.addAll(CustomEventEntity.findByName(sDate));
        activity_chart.setDataByResults(results);
    }

    public void addTrackedAppsToTimechart() throws SQLException {
        Map<Integer, String> appNames = QuerryExecutor.getAppNames();
        appNames.put(0, "ACTIVITY");
        app_axis.setCategories(FXCollections.observableArrayList(appNames.values()));
        activity_chart.setAppNames(appNames);
    }

    @FXML
    public void handle_left_date(MouseEvent event) throws ParseException, SQLException {
        Calendar cal = Calendar.getInstance();
        cal.setTime(currentDate);
        cal.add(Calendar.DATE, -1);
        currentDate.setTime(cal.getTimeInMillis());

        date.setText(dateFormat.format(currentDate.getTime()));
        loadExistingDataToTimechart(new Date(cal.getTimeInMillis()));
    }

    @FXML
    public void handle_right_date(MouseEvent event) throws ParseException, SQLException {
        Calendar cal = Calendar.getInstance();
        cal.setTime(currentDate);
        Date today = new Date();
        String today_text = dateFormat.format(today);
        today = dateFormat.parse(today_text);
        if (currentDate.compareTo(today) < 0) {
            cal.add(Calendar.DATE, 1);
            currentDate.setTime(cal.getTimeInMillis());
            date.setText(dateFormat.format(currentDate.getTime()));

            loadExistingDataToTimechart(new Date(cal.getTimeInMillis()));
        }
    }

    @FXML
    public void handleAppButton(ActionEvent event) {
        List<File> list =
                fileChooser.showOpenMultipleDialog(new Stage());
        if (list != null) {
            if (!list.isEmpty()) {
                for (File file : list) {
                    managingApplicationsController.addNewApplicationByPath(file.getAbsolutePath(), DEFAULT_COLOR);
                }
            }
        }
    }

    @FXML
    public void handleReportButton(ActionEvent event) {
        appController.showReportGenerationWindow();
    }

    @FXML
    public void handleSettingsButton(ActionEvent event){
        appController.showSettingsWindow();
    }

    @FXML
    public void handleMenuButton(MouseEvent event) {
        menuButton.show();
    }

    @FXML
    public void handleEventButton(ActionEvent event) {
        appController.showCustomEventView();
    }

}