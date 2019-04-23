/**
 * Created by kuba on 06/04/2019
 */

package pl.edu.agh.io.umniedziala.viewController;

import javafx.fxml.FXML;
import javafx.scene.chart.BarChart;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.scene.text.Text;

public class MainViewController {
    private AppController appController;

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
    private Button left_date;

    @FXML
    private Button right_date;

    @FXML
    private BarChart activity_chart;

    @FXML
    private Button app_add;

    @FXML
    public void initialize(){

    }

    public void setAppController(AppController appController) {
        this.appController = appController;
    }

}