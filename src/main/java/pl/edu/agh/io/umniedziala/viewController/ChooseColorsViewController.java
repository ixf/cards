/**
 * Created by kuba on 2019-05-29
 */

package pl.edu.agh.io.umniedziala.viewController;


import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.model.ApplicationEntity;

import java.util.Map;

public class ChooseColorsViewController {
    private AppController appController;
    private Stage stage;
    private Map<String, String> appColors;
    private ObservableList<App> observableApps;

    private App changedApp;


    @FXML
    private TableView appColorsTable;

    @FXML
    private TableColumn<App, String> applicationColumn;

    @FXML
    private ColorPicker colorPicker;



    @FXML
    public void initialize(){
        appColorsTable.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        applicationColumn.setCellValueFactory(dataValue -> dataValue.getValue().nameProperty());

        appColorsTable.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<App>() {
            @Override
            public void changed(ObservableValue<? extends App> observable, App oldValue, App newValue) {
                colorPicker.setValue(Color.valueOf(newValue.getColor()));
                changedApp = newValue;
            }
        });

    }


    public void setAppController(AppController appController) {
        this.appController = appController;
    }


    public void setStage(Stage stage) {
        this.stage = stage;
    }


    public void loadData() {
        appColors = ApplicationEntity.getApplicationsColors();
        observableApps = FXCollections.observableArrayList();
        appColors.forEach((name, color) -> observableApps.add(new App(name, color)));
        appColorsTable.setItems(observableApps);
    }


    public void handleOK(ActionEvent event) {
        stage.close();
    }

    public void handleColorChange(ActionEvent event) {
        if(changedApp == null)
            return;
        Color color = colorPicker.getValue();
        String newColor = String.format("#%02X%02X%02X",
                (int)(color.getRed()*255),
                (int)(color.getGreen()*255),
                (int)(color.getBlue()*255));
        ApplicationEntity.updateApplicationColor(changedApp.getName(), newColor);
        changedApp.color = newColor;
    }

    private class App{
        StringProperty name;
        String color;

        public App(String name, String color) {
            this.name = new SimpleStringProperty(name);
            this.color = color;
        }

        public String getName() {
            return name.get();
        }

        public StringProperty nameProperty() {
            return name;
        }

        public String getColor() {
            return color;
        }
    }

}