package pl.edu.agh.io.umniedziala;

import pl.edu.agh.io.umniedziala.ReportsGenerator.BasicReport;
import pl.edu.agh.io.umniedziala.activeApplicationMonitor.ActiveApplicationListener;
import pl.edu.agh.io.umniedziala.model.ApplicationEntity;
import pl.edu.agh.io.umniedziala.model.RunningPeriodEntity;
import javafx.application.Application;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.viewController.AppController;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.Locale;

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
        new ActiveApplicationListener().start();
        launch(args);
    }
}