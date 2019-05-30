/**
 * Created by kuba on 2019-05-29
 */

package pl.edu.agh.io.umniedziala.viewController;


import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Modality;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.configuration.Configuration;

import java.io.IOException;

public class SettingsViewController {
    private AppController appController;
    private Stage stage;
    private Stage colorsStage;

    private Long checkInterval;
    private Long activityTime;
    private int chartStart;
    private int chartEnd;

    @FXML
    private TextField chartStartField;

    @FXML
    private TextField chartEndField;

    @FXML
    private TextField computerActivityField;

    @FXML
    private TextField applicationActivityField;

    @FXML
    private Label errorLabel;


    public void setAppController(AppController appController) {
        this.appController = appController;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    public void loadData(){
        chartStart = Configuration.getInstance().getChartStart().intValue();
        chartEnd = Configuration.getInstance().getChartEnd().intValue();

        checkInterval = Configuration.getInstance().getCheckInterval();
        activityTime = Configuration.getInstance().getInactivityPeriod();

        chartStartField.setText(Integer.toString(chartStart));
        chartEndField.setText(Integer.toString(chartEnd));
        computerActivityField.setText(Long.toString(activityTime));
        applicationActivityField.setText(Long.toString(checkInterval));
    }

    public void handleSave(ActionEvent event) {
        try {
            chartStart = Integer.parseInt(chartStartField.getText());
            chartEnd = Integer.parseInt(chartEndField.getText());

            checkInterval = Long.parseLong(applicationActivityField.getText());
            activityTime = Long.parseLong(computerActivityField.getText());

            if(colorsStage != null) {
                colorsStage.close();
            }

            Configuration.getInstance().setChartStart((long) chartStart);
            Configuration.getInstance().setChartEnd((long) chartEnd);

            Configuration.getInstance().setCheckInterval(checkInterval);
            Configuration.getInstance().setInactivityPeriod(activityTime);

            stage.close();
        } catch (NumberFormatException e){
            errorLabel.setText("INCORRECT VALUES");
        }
    }

    public void handleCancel(ActionEvent event) {
        if(colorsStage != null) {
            colorsStage.close();
        }
        stage.close();
    }

    public void chooseColors(ActionEvent event) {
        try {
            FXMLLoader loader = new FXMLLoader();

            Parent page = loader.load(getClass().getResourceAsStream("/views/ChooseColorsView.fxml"));

            colorsStage = new Stage();
            colorsStage.setTitle("Choose colors");
            colorsStage.initModality(Modality.WINDOW_MODAL);

            Scene colorsScene = new Scene(page);
            colorsStage.setScene(colorsScene);

            ChooseColorsViewController controller = loader.getController();
            controller.setAppController(this.appController);
            controller.setStage(colorsStage);
            controller.loadData();
            colorsStage.show();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}