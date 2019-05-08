/**
 * Created by kuba on 06/04/2019
 */

package pl.edu.agh.io.umniedziala.viewController;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.chart.BarChart;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

public class MainViewController {
    private AppController appController;
    DateFormat dateFormat = new SimpleDateFormat("EEEE, dd.MM.yyyy");
    final FileChooser fileChooser = new FileChooser();


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
    private Button generate_report;

    @FXML
    public void initialize(){
        Date current_date = new Date();
        date.setText(dateFormat.format(current_date));
    }

    public void setAppController(AppController appController) {
        this.appController = appController;
    }

    @FXML
    public void handle_left_date(MouseEvent event) throws IOException, ParseException {
        String date_text = date.getText();
        Date curr_date = dateFormat.parse(date_text);
        Calendar cal = Calendar.getInstance();
        cal.setTime(curr_date);
        cal.add(Calendar.DATE,-1);
        date.setText(dateFormat.format(cal.getTime()));
    }

    @FXML
    public void handle_right_date(MouseEvent event) throws IOException, ParseException {
        String date_text = date.getText();
        Date curr_date = dateFormat.parse(date_text);
        Calendar cal = Calendar.getInstance();
        cal.setTime(curr_date);
        cal.add(Calendar.DATE,1);
        date.setText(dateFormat.format(cal.getTime()));
    }

    @FXML
    public void handle_app_add(ActionEvent event) throws IOException {
        List<File> list =
                fileChooser.showOpenMultipleDialog(new Stage()); //TODO: add file to DB
        for (File file : list) {
            System.out.println(file.getAbsolutePath());
        }
    }


    @FXML
    public void handleReportGeneration(ActionEvent event){
        appController.showReportGenerationWindow();
    }

}