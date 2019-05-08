package pl.edu.agh.io.umniedziala;

import pl.edu.agh.io.umniedziala.activeApplicationMonitor.ActiveApplicationListener;

import javafx.application.Application;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.viewController.AppController;

import java.io.IOException;

public class Main extends Application {

    private Stage primaryStage;

    private AppController appController;

    @Override
    public void start(Stage primaryStage) throws IOException {
        this.primaryStage = primaryStage;
        this.primaryStage.setTitle("WorkMonitor");

        this.appController = new AppController(primaryStage);
        this.appController.initRootLayout();
    }

    public static void main(String[] args) {
        new ActiveApplicationListener(5000).start();
        launch(args);
    }
}
