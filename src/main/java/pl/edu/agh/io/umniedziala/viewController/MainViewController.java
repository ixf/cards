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
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;
import pl.edu.agh.io.umniedziala.model.RunningPeriodEntity;
import pl.edu.agh.io.umniedziala.view.TimeChart;

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
    final static int DEFAULT_COLOR = 0;

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
    private Button app_add;

    @FXML
    private Button generate_report;

    @FXML
    public void initialize(){
        Date current_date = new Date();
        date.setText(dateFormat.format(current_date));
        managingApplicationsController = new ManagingApplicationsController();

        startTimechartUpdates(current_date);
    }

    private void startTimechartUpdates(Date current_date) {
        // Nasz timechart jest szeroki a bazę aktualizujemy często. Nie ma chyba potrzeby, żeby aktualizować
        // wykresy przy każdej zmianie w bazie. Uruchamiam tutaj timer, który co kilka minut aktualizuje wykres.

        long repeatTime = 1 * 15 * 1000; // w milisekundach
        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                Platform.runLater(() ->{
                    try {
                        addTrackedAppsToTimechart();
                        loadExistingDataToTimechart(current_date);
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
        List<RunningPeriodEntity> results = QuerryExecutor.getPeriodsForDay(date);
        activity_chart.setDataByResults(results);
    }

    public void addTrackedAppsToTimechart() throws SQLException {
        Map<Integer, String> appNames = QuerryExecutor.getAppNames();
        app_axis.setCategories(FXCollections.observableArrayList(appNames.values()));
        activity_chart.setAppNames(appNames);
    }

    @FXML
    public void handle_left_date(MouseEvent event) throws ParseException {
        String date_text = date.getText();
        Date curr_date = dateFormat.parse(date_text);
        Calendar cal = Calendar.getInstance();
        cal.setTime(curr_date);
        cal.add(Calendar.DATE,-1);
        date.setText(dateFormat.format(cal.getTime()));
    }

    @FXML
    public void handle_right_date(MouseEvent event) throws ParseException {
        String date_text = date.getText();
        Date curr_date = dateFormat.parse(date_text);
        Calendar cal = Calendar.getInstance();
        cal.setTime(curr_date);
        Date today = new Date();
        String today_text = dateFormat.format(today);
        today = dateFormat.parse(today_text);
        if (curr_date.compareTo(today) < 0 ) {
            cal.add(Calendar.DATE, 1);
            date.setText(dateFormat.format(cal.getTime()));
        }
    }

    @FXML
    public void handle_app_add(ActionEvent event) {
        List<File> list =
                fileChooser.showOpenMultipleDialog(new Stage());
        if (list != null){
            if (!list.isEmpty()) {
                for (File file : list) {
                    managingApplicationsController.addNewApplicationByPath(file.getAbsolutePath(),DEFAULT_COLOR);
                }
            }
        }
    }


    @FXML
    public void handleReportGeneration(ActionEvent event){
        appController.showReportGenerationWindow();
    }

}