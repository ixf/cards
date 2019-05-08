/**
 * Created by kuba on 06/04/2019
 */

package pl.edu.agh.io.umniedziala.viewController;


import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.io.IOException;

public class AppController {
    private Stage primaryStage;

    public AppController(Stage primaryStage) {
        this.primaryStage = primaryStage;
    }


    public void initRootLayout() throws IOException {
        this.primaryStage.setTitle("WorkMonitor");

        // load layout from FXML file
        FXMLLoader loader = new FXMLLoader();
        Parent rootLayout = loader.load(getClass().getResourceAsStream("/views/MainView.fxml"));


        MainViewController controller = loader.getController();
        controller.setAppController(this);

        // add layout to a scene and show them all
        Scene scene = new Scene(rootLayout);
        primaryStage.setScene(scene);
        primaryStage.show();

    }

    public void showReportGenerationWindow(){
        try {
            // Load the fxml file and create a new stage for the dialog
            FXMLLoader loader = new FXMLLoader();

            Parent page = loader.load(getClass().getResourceAsStream("/views/ReportGenerationView.fxml"));

            // Create the dialog Stage.
            Stage reportStage = new Stage();
            reportStage.setTitle("Report Generation");
            reportStage.initModality(Modality.WINDOW_MODAL);
            //reportStage.initOwner(primaryStage);
            Scene scene = new Scene(page);
            reportStage.setScene(scene);

            ReportGenerationViewController controller = loader.getController();
            controller.setAppController(this);
            controller.setStage(reportStage);
            reportStage.show();


        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}