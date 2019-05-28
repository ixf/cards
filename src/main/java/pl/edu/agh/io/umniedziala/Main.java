package pl.edu.agh.io.umniedziala;

import javafx.application.Application;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.configuration.Configuration;
import pl.edu.agh.io.umniedziala.monitors.activeApplicationMonitor.ActiveApplicationListener;
import pl.edu.agh.io.umniedziala.monitors.compuerMonitor.ActivityListener;
import pl.edu.agh.io.umniedziala.viewController.AppController;

import java.io.IOException;

public class Main extends Application {

    private Stage primaryStage;

    private AppController appController;

    private ActiveApplicationListener activeApplicationListener;

    @Override
    public void start(Stage primaryStage) throws IOException {
        activeApplicationListener =
                new ActiveApplicationListener(
                        Configuration.getInstance().getCheckInterval().intValue()
                );
        activeApplicationListener.start();

        ActivityListener activityListener = new ActivityListener();
        activityListener.start();

        this.primaryStage = primaryStage;
        this.primaryStage.setTitle("WorkMonitor");
        this.primaryStage.setOnCloseRequest(event -> activeApplicationListener.stop());

        this.appController = new AppController(primaryStage);
        this.appController.initRootLayout();
    }

    public static void main(String[] args) {
        launch(args);
    }
}