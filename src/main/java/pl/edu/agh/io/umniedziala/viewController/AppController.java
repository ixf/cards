/**
 * Created by kuba on 06/04/2019
 */

package pl.edu.agh.io.umniedziala.viewController;


import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

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
        primaryStage.getScene().getWindow().addEventFilter(WindowEvent.WINDOW_CLOSE_REQUEST, this::closeWindowEvent);
        primaryStage.show();

    }

    private void closeWindowEvent(WindowEvent event) {
        System.out.println("Window close request ...");
        //TODO: tu obsługa wydarzeń przed zamknięciem apki
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

    public void showCustomEventView(){
        try {
            // Load the fxml file and create a new stage for the dialog
            FXMLLoader loader = new FXMLLoader();

            Parent page = loader.load(getClass().getResourceAsStream("/views/CustomEventView.fxml"));

            // Create the dialog Stage.
            Stage eventStage = new Stage();
            eventStage.setTitle("Custom event");
            eventStage.initModality(Modality.WINDOW_MODAL);
            //reportStage.initOwner(primaryStage);
            Scene scene = new Scene(page);
            eventStage.setScene(scene);

            CustomEventController controller = loader.getController();
            controller.setAppController(this);
            controller.setStage(eventStage);
            eventStage.show();


        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}