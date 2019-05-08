/**
 * Created by kuba on 06/04/2019
 */

package pl.edu.agh.io.umniedziala.viewController;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.chart.BarChart;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableView;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.ManagingApplicationsController;
import pl.edu.agh.io.umniedziala.databaseUtilities.QuerryExecutor;

import java.io.File;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static pl.edu.agh.io.umniedziala.databaseUtilities.DataBaseConnectionProvider.getConnection;

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
    private BarChart activity_chart;

    @FXML
    private Button app_add;

    @FXML
    private TableView table;

    @FXML
    private Button generate_report;

    @FXML
    public void initialize(){
        Date current_date = new Date();
        date.setText(dateFormat.format(current_date));
        managingApplicationsController = new ManagingApplicationsController();
        getTrackedApps();
    }

    public void setAppController(AppController appController) {
        this.appController = appController;
    }

    private void getTrackedApps(){
        try {
            ResultSet result = QuerryExecutor.read("SELECT * FROM application");
            while (result.next()){
                //TODO: dodać aplikacje do TableView table
            }
        } catch (SQLException e){
            System.out.println("Application not loaded from db");
        }
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